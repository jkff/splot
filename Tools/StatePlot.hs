module Main where

import System (getArgs)
import System.Exit

import Control.Monad.RWS as RWS
import qualified Data.Map as M
import Data.List
import Data.Ord
import Data.Function
import Data.Maybe

import Data.Time
import Data.Time.Parse

import qualified Data.ByteString.Lazy.Char8 as B
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Renderable
import Graphics.Rendering.Chart.Gtk
import qualified Graphics.Rendering.Cairo as C
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names


data Event = Event {time :: LocalTime, track :: String, edge :: Edge} deriving (Show)
data Edge = Begin { color :: String } | End { color :: String } deriving (Show)

data Bar = Bar Double Double String | ExpiredBar Double Double String

getArg :: String -> String -> [String] -> String
getArg name def args = case [(k,v) | (k,v) <- zip args (tail args), k==("-"++name)] of
  (_,v):_ -> v
  _       -> def

parse :: (B.ByteString -> (LocalTime, B.ByteString)) -> B.ByteString -> Event
parse parseTime s = Event { time = ts, track = B.unpack $ B.tail track', edge = edge }
  where
    (ts, s') = parseTime s
    (track', color) = B.break (==' ') (B.tail s')
    clr = if B.null color then "" else B.unpack (B.tail color)
    edge = case (B.head track') of
      '>' -> Begin clr
      '<' -> End clr

diffToMillis :: LocalTime -> LocalTime -> Double
diffToMillis t2 t1 = fromIntegral (truncate (1000000*d)) / 1000
 where d = diffUTCTime (localTimeToUTC utc t2) (localTimeToUTC utc t1)

data BarHeight = BarHeightFixed Double | BarHeightFill

data RenderConfiguration = RenderConf {
        barHeight :: BarHeight,
        tickIntervalMs :: Double,
        largeTickFreq :: Int,
        expireTimeMs :: Double,
        cmpTracks :: Event -> Event -> Ordering,
        phantomColor :: Maybe String,
        fromTime :: Maybe LocalTime,
        toTime :: Maybe LocalTime,
        forcedNumTracks :: Maybe Int,
        streaming :: Bool
    }

data TickSize = LargeTick | SmallTick

renderEvents :: RenderConfiguration -> IO [Event] -> IO (Renderable ())
renderEvents conf readEs = if streaming conf 
                           then return (Renderable {minsize = return (0,0), render = render' (c $ liftIO readEs)})
                           else readEs >>= \es -> return $ Renderable {minsize = return (0,0), render = render' (return es)}
  where 
    maybeM :: (Monad m) => (a -> m b) -> Maybe a -> m ()
    maybeM f Nothing  = return ()
    maybeM f (Just x) = f x >> return ()

    readColour ('#':r1:r2:g1:g2:b1:b2:[]) = Just (sRGB24 r g b)
      where
        r = fromIntegral $ unhex r2 + 16*unhex r1
        g = fromIntegral $ unhex g2 + 16*unhex g1
        b = fromIntegral $ unhex b2 + 16*unhex b1
        unhex c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
                | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a'
                | c >= 'A' && c <= 'Z' = 10 + fromEnum c - fromEnum 'A'
    readColour cs = readColourName cs

    makeBars time2ms minRenderTime maxRenderTime es = snd $ RWS.execRWS (mapM_ step es >> flush) () M.empty
      where
        step e@(Event t track edge) = do
          (i, ms0) <- summon e
          flip maybeM ms0 $ \(t0,c0) -> do
            let overrideEnd c0 = case edge of { End c | c /= "" -> c ; _ -> c0  }
            if (time2ms t - time2ms t0 < expireTimeMs conf)
              then RWS.tell [(i, Bar        (time2ms t0) (time2ms t)                      (overrideEnd c0))]
              else RWS.tell [(i, ExpiredBar (time2ms t0) (time2ms t0 + expireTimeMs conf) (overrideEnd c0))]
          putTrack track (i, case edge of { Begin c -> Just (t,c); End _ -> Nothing })
        
        flush = RWS.gets M.keys >>= mapM_ flushTrack
        flushTrack track = do
          (i,ms0) <- RWS.gets (M.! track)
          flip maybeM ms0 $ \(t0,c0) -> 
            if (time2ms maxRenderTime - time2ms t0 < expireTimeMs conf)
              then RWS.tell [(i, Bar        (time2ms t0) (time2ms maxRenderTime)          c0)]
              else RWS.tell [(i, ExpiredBar (time2ms t0) (time2ms t0 + expireTimeMs conf) c0)]

        putTrack track s = modify (M.insert track s)

        summon (Event t track edge) = do
          m <- RWS.get
          when (not (M.member track m)) $ do
            let i = M.size m
            RWS.put (M.insert track (i, Nothing) m)
            case (phantomColor conf, edge) of
              (_,      End c) | c /= "" -> RWS.tell [(i, Bar (time2ms minRenderTime) (time2ms t) c)]
              (Just c, End _)           -> RWS.tell [(i, Bar (time2ms minRenderTime) (time2ms t) c)]
              _                         -> return ()
          RWS.gets (M.! track)

    render' :: CRender [Event] -> (Double,Double) -> CRender (PickFn a)
    render' readEs (w,h) = do
      es <- readEs
      minRenderTime <- case (fromTime conf, streaming conf) of { 
        (Just t, _)      -> return t;
        (Nothing, True)  -> fmap (minimum . map time) readEs; 
        (Nothing, False) -> return . minimum . map time $ es -- Will evaluate the whole of 'es' and hold it in memory
      } 
      maxRenderTime <- case (toTime conf,   streaming conf) of { 
        (Just t, _)      -> return t;
        (Nothing, True)  -> fmap (maximum . map time) readEs; 
        (Nothing, False) -> return . maximum . map time $ es -- Same here.
      } 

      let time2ms t | t < minRenderTime = time2ms minRenderTime
                    | t > maxRenderTime = time2ms maxRenderTime
                    | otherwise         = diffToMillis t minRenderTime
      let rangeMs = time2ms maxRenderTime


      let numUnique xs = M.size $ M.fromList $ zip xs (repeat ())
      numTracks <- case (forcedNumTracks conf, streaming conf) of { 
        (Just n, _)      -> return n;
        (Nothing, True)  -> fmap (numUnique . map track) readEs;
        (Nothing, False) -> return . numUnique . map track $ es
      }

      let ticks = takeWhile ((<rangeMs).snd) $ 
            map (\i -> if i`mod`largeTickFreq conf == 0 
                       then (LargeTick, fromIntegral i*tickIntervalMs conf) 
                       else (SmallTick, fromIntegral i*tickIntervalMs conf)) [0..]

      let ms2x ms = 10 + ms / rangeMs * (w - 10)
      let time2x t = ms2x (time2ms t)
      let yStep = case barHeight conf of {
          BarHeightFixed _ -> (h-20) / fromIntegral (numTracks+1)
        ; BarHeightFill    -> (h-20) / fromIntegral numTracks
        }
      let track2y i = case barHeight conf of {
          BarHeightFixed bh -> fromIntegral (i+1) * yStep - bh/2
        ; BarHeightFill     -> fromIntegral (i+1) * yStep - yStep/2
        }
      let drawTick (t, ms) = do {
          setLineStyle $ solidLine 1 (opaque black)
        ; moveTo $ Point (ms2x ms) (h-20)
        ; lineTo $ Point (ms2x ms) (h-case t of { LargeTick -> 13 ; SmallTick -> 17 })
        ; c $ C.stroke
        }
          
      let fillRectAA (Point x1 y1) (Point x2 y2) = do {
          c $ C.newPath
        ; c $ C.moveTo x1 y1
        ; c $ C.lineTo x1 y2
        ; c $ C.lineTo x2 y2
        ; c $ C.lineTo x2 y1
        ; c $ C.closePath
        ; c $ C.fill
        }
      let strokeLineAA (Point x1 y1) (Point x2 y2) = do {
          c $ C.newPath
        ; c $ C.moveTo x1 y1
        ; c $ C.lineTo x2 y2
        ; c $ C.stroke
        }
      let drawBar i (Bar ms1 ms2 color) = do {
            setLineStyle $ solidLine 1 transparent
          ; setFillStyle $ solidFillStyle $ opaque $ fromMaybe (error $ "unknown color: " ++ color) (readColour color)
          ; case barHeight conf of {
              BarHeightFixed bh -> fillRectAA (Point (ms2x ms1) (track2y i - bh   /2)) (Point (ms2x ms2) (track2y i + bh   /2))
            ; BarHeightFill     -> fillRectAA (Point (ms2x ms1) (track2y i - yStep/2)) (Point (ms2x ms2) (track2y i + yStep/2))
            }
          }
          drawBar i (ExpiredBar ms1 ms2 color) = do {
            setLineStyle $ dashedLine 1 [3,3] (opaque $ fromMaybe (error $ "unknown color: " ++ color) (readColour color))
          ; strokeLineAA (Point (ms2x ms1) (track2y i)) (Point (ms2x ms2) (track2y i))
          ; setLineStyle $ solidLine 1 (opaque red)
          ; strokeLineAA (Point (ms2x ms2 - 5) (track2y i - 5)) (Point (ms2x ms2 + 5) (track2y i + 5))
          ; strokeLineAA (Point (ms2x ms2 + 5) (track2y i - 5)) (Point (ms2x ms2 - 5) (track2y i + 5))
          }

      setFillStyle $ solidFillStyle (opaque white)
      fillPath $ rectPath $ Rect (Point 0 0) (Point w h)
      setLineStyle $ solidLine 1 (opaque black)
      moveTo (Point 10 (h-20))
      lineTo (Point w  (h-20))
      c $ C.stroke
      moveTo (Point 10 (h-20))
      lineTo (Point 10 0)
      c $ C.stroke
      moveTo (Point 10 (h-3))
      c $ C.showText $ "Origin at " ++ show minRenderTime ++ ", 1 small tick = " ++ show (tickIntervalMs conf) ++ "ms"
      mapM_ drawTick ticks

      let bars = makeBars time2ms minRenderTime maxRenderTime es 
      mapM_ (\(i,e) -> drawBar i e) bars
      
      return nullPickFn

showHelp = mapM_ putStrLn [
    "splot - a tool for visualizing the lifecycle of many concurrent multi-stage processes. See http://www.haskell.org/haskellwiki/Splot",
    "Usage: splot [-if INFILE] [-o PNGFILE] [-w WIDTH] [-h HEIGHT] [-bh BARHEIGHT] ",
    "             [-tf TIMEFORMAT] [-sort SORT] [-expire EXPIRE]",
    "             [-fromTime TIME] [-toTime TIME] [-numTracks NUMTRACKS]",
    "             [-tickInterval TICKINTERVAL] [-largeTickFreq N]",
    "             [-stream true]",
    "  -if INFILE    - filename from where to read the trace.",
    "                  If omitted or '-', read from stdin.",
    "  -o PNGFILE    - filename to which the output will be written in PNG format.",
    "                  If omitted, it will be shown in a window.",
    "  -w, -h        - width and height of the resulting picture. Default 640x480.",
    "  -bh           - height of the bar depicting each individual process. Default 5 pixels.",
    "                  Use 1 or so, or 'fill' if you have a lot of them. ",
    "                  '-bh fill' means 'fill the screen with bars, without vertical gaps'",
    "  -tf           - time format, as in http://linux.die.net/man/3/strptime but with ",
    "                  fractional seconds supported via %OS - will parse 12.4039 or 12,4039",
    "                  Also, %^[+-][N]s will parse seconds since the epoch, for example ",
    "                  %^-3s are milliseconds since the epoch (N can only be 1 digit)",
    "  -tickInterval - ticks on the X axis will be this often (in millis, default 1000).",
    "  -largeTickFreq N - every N'th tick will be larger than the others (default 10).",
    "  -sort SORT    - sort tracks by SORT, where: 'time' - sort by time of first event, ",
    "                  'name' - sort by track name.",
    "  -expire       - expire activities after given time period (in millis) - for instance,",
    "                  to account that if an activity doesn't tell you it's finished for too long,",
    "                  then it probably was killed.",
    "  -phantom COL  - 'phantom color' - if a track starts from a '<' event, it is assumed that",
    "                  the corresponding '>' event (not present in the log) was of color COL.",
    "                  Useful if you're drawing pieces of large logs.",
    "  -fromTime TIME - clip picture on left (time in same format as in trace)",
    "  -toTime TIME   - clip picture on right (time in same format as in trace)",
    "  -numTracks NUMTRACKS - explicitly specify number of tracks when using '-stream true'",
    "  -stream true  - use 'streaming mode' where the input is not loaded into memory and you",
    "                  can process multi-gigabyte inputs.",
    "                  In this mode, you MUST use an actual filename in '-if'.",
    "                  Note that you better also indicate -fromTime, -toTime and -numTracks,",
    "                  otherwise the data will be re-scanned once per each of these properties",
    "                  that is not indicated.",
    "",
    "Input is read from stdin. Example input (speaks for itself):",
    "2010-10-21 16:45:09,431 >foo green",
    "2010-10-21 16:45:09,541 >bar green",
    "2010-10-21 16:45:10,631 >foo yellow",
    "2010-10-21 16:45:10,725 >foo #ff0000",
    "2010-10-21 16:45:10,930 >bar blue",
    "2010-10-21 16:45:11,322 <foo",
    "2010-10-21 16:45:12,508 <bar red",
    "",
    "'>FOO COLOR' means 'start a bar of color COLOR on track FOO',",
    "'<FOO' means 'end the current bar for FOO'.",
    "'<FOO COLOR' means 'end the current bar for FOO and make the whole bar of color COLOR'",
    "(for example if we found that FOO failed and all the work since >FOO was wasted, COLOR",
    "might be red)"
    ]

main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp >> exitSuccess
    _          -> return ()
  let (w,h) = (read $ getArg "w" "640" args, read $ getArg "h" "480" args)
  let barHeight = case getArg "bh" "5" args of { "fill" -> BarHeightFill ; bh -> BarHeightFixed $ read bh }
  let tickIntervalMs = read $ getArg "tickInterval" "1000" args
  let largeTickFreq = read $ getArg "largeTickFreq" "10" args
  let timeFormat = getArg "tf" "%Y-%m-%d %H:%M:%OS" args
  let ptime = strptime (B.pack timeFormat)
  let parseTime s = fromMaybe (error $ "Invalid time: " ++ show s) . ptime $ s
  let fromTime = fst `fmap` (strptime timeFormat $ getArg "fromTime" "" args)
  let toTime = fst `fmap` (strptime timeFormat $ getArg "toTime" "" args)
  let forcedNumTracks = case getArg "numTracks" "" args of { "" -> Nothing ; n -> Just $ read n }
  let outPNG = getArg "o" "" args
  let inputFile = getArg "if" "-" args
  let pruneLF b | not (B.null b) && (B.last b == '\r') = B.init b
                | otherwise                            = b
  let cmpTracks = case getArg "sort" "time"  args of { "time" -> comparing time ; "name" -> comparing track }
  let expireTimeMs = read $ getArg "expire" "Infinity" args
  let phantomColor = case getArg "phantom" "" args of { "" -> Nothing; c -> Just c }
  let streaming = case getArg "stream" "false" args of { "true" -> True; _ -> False }

  let readInput = if inputFile == "-" then B.getContents else B.readFile inputFile
  let readEvents = (map (parse parseTime . pruneLF) . B.lines) `fmap` readInput
  
  when (streaming && (inputFile == "-"))  $ error "In streaming mode (-stream true) you MUST use an actual filename in '-if'"
  when (streaming && isNothing fromTime)  $ putStrLn "Warning: without -fromTime, input will be re-scanned to compute it."
  when (streaming && isNothing toTime)    $ putStrLn "Warning: without -toTime, input will be re-scanned to compute it."
  when (streaming && isNothing forcedNumTracks) $ putStrLn "Warning: without -numTracks, input will be re-scanned to compute it."

  pic <- renderEvents (RenderConf barHeight tickIntervalMs largeTickFreq expireTimeMs cmpTracks phantomColor fromTime toTime forcedNumTracks streaming) readEvents
  case outPNG of
    "" -> renderableToWindow pic w h
    f  -> const () `fmap` renderableToPNGFile pic w h outPNG

