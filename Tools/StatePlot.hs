module Tools.StatePlot where

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
import qualified Graphics.Rendering.Cairo as C
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names

import Tools.ColorMap

import Debug.Trace

data Event = Event { time :: LocalTime, track :: String, edge :: Edge } deriving (Show)
data Edge = Begin { color :: String } | End { color :: String } | Pulse { glyph :: Glyph, color :: String } deriving (Show)

-- Text is enough for most purposes (|, o, <, >, ...) but potentially more can exist.
data Glyph = GlyphText { text :: String } deriving (Show)

data OutputGlyph = Bar Double Double String | ExpiredBar Double Double String | OutPulse Double Glyph String

parse :: (B.ByteString -> (LocalTime, B.ByteString)) -> B.ByteString -> Event
parse parseTime s = Event { time = ts, track = B.unpack $ B.tail track', edge = edge }
  where
    (ts, s') = parseTime s
    (track', arg0) = B.break (==' ') (B.tail s')
    arg = if B.null arg0 then "" else B.unpack (B.tail arg0)
    edge = case (B.head track') of
      '>' -> Begin (if null arg then "gray" else arg)
      '<' -> End   (if null arg then ""     else arg)
      '!' -> Pulse (GlyphText text) color
        where (color, text0) = break (==' ') arg
              text = tail text0

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
                           then return (Renderable {minsize = return (0,0), render = renderGlyphsAtFront (c $ liftIO readEs)})
                           else readEs >>= \es -> return $ Renderable {minsize = return (0,0), render = renderGlyphsAtFront (return es)}
  where 
    maybeM :: (Monad m) => (a -> m b) -> Maybe a -> m ()
    maybeM f Nothing  = return ()
    maybeM f (Just x) = f x >> return ()

    computeColour map color = case readColour color of 
      Nothing -> cycleColor map color
      Just c  -> (c, map)
    
    readColour ('#':r1:r2:g1:g2:b1:b2:[]) = Just (sRGB24 r g b)
      where
        r = fromIntegral $ unhex r2 + 16*unhex r1
        g = fromIntegral $ unhex g2 + 16*unhex g1
        b = fromIntegral $ unhex b2 + 16*unhex b1
        unhex c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
                | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a'
                | c >= 'A' && c <= 'Z' = 10 + fromEnum c - fromEnum 'A'
    readColour cs = readColourName cs
    
    
    makeGlyphs time2ms minRenderTime maxRenderTime es = snd $ RWS.execRWS (mapM_ step es >> flush) () M.empty
      where
        step e@(Event t track edge) = do
          (i, ms0) <- summon e
          flip maybeM ms0 $ \(t0,c0) -> do
            let overrideEnd c0 = case edge of { End c | c /= "" -> c ; _ -> c0  }
            if (time2ms t - time2ms t0 < expireTimeMs conf)
              then RWS.tell [(i, Bar        (time2ms t0) (time2ms t)                      (overrideEnd c0))]
              else RWS.tell [(i, ExpiredBar (time2ms t0) (time2ms t0 + expireTimeMs conf) (overrideEnd c0))]
          case edge of { 
            Pulse glyph c -> RWS.tell [(i, OutPulse (time2ms t) glyph c)]
          ; Begin       c -> putTrack track (i, Just (t,c))
          ; End         _ -> putTrack track (i, Nothing)
          }
        
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

    renderGlyphsAtFront :: CRender [Event] -> (Double,Double) -> CRender (PickFn a)
    renderGlyphsAtFront readEs (w,h) = do
      setFillStyle $ solidFillStyle (opaque white)
      fillPath $ rectPath $ Rect (Point 0 0) (Point w h)
      render' readEs (w,h) False
      render' readEs (w,h) True
      return nullPickFn

    render' :: CRender [Event] -> (Double,Double) -> Bool -> CRender ()
    render' readEs (w,h) drawGlyphsNotBars = do
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
      let drawGlyph i (Bar ms1 ms2 color) map = if drawGlyphsNotBars then return map else do {
            setLineStyle $ solidLine 1 transparent
          ; let (colorToUse,map') = computeColour map color
          ; setFillStyle $ solidFillStyle $ opaque $ colorToUse
          ; case barHeight conf of {
              BarHeightFixed bh -> fillRectAA (Point (ms2x ms1) (track2y i - bh   /2)) (Point (ms2x ms2) (track2y i + bh   /2))
            ; BarHeightFill     -> fillRectAA (Point (ms2x ms1) (track2y i - yStep/2)) (Point (ms2x ms2) (track2y i + yStep/2))
            }
          ; return map'
          }
          drawGlyph i (ExpiredBar ms1 ms2 color) map = if drawGlyphsNotBars then return map else do {
            let (colorToUse,map') = computeColour map color
          ; setLineStyle $ dashedLine 1 [3,3] (opaque $ colorToUse)
          ; strokeLineAA (Point (ms2x ms1) (track2y i)) (Point (ms2x ms2) (track2y i))
          ; setLineStyle $ solidLine 1 (opaque red)
          ; strokeLineAA (Point (ms2x ms2 - 5) (track2y i - 5)) (Point (ms2x ms2 + 5) (track2y i + 5))
          ; strokeLineAA (Point (ms2x ms2 + 5) (track2y i - 5)) (Point (ms2x ms2 - 5) (track2y i + 5))
          ; return map'
          }
          drawGlyph i (OutPulse ms glyph color) map = if not drawGlyphsNotBars then return map else case glyph of {
            GlyphText text -> do {
              let (colorToUse,map') = computeColour map color
            ; setLineStyle $ solidLine 1 (opaque $ colorToUse)
            ; moveTo (Point (ms2x ms) (track2y i))
            ; c $ C.showText text
            ; return map'
            }
          }

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

      let glyphs = makeGlyphs time2ms minRenderTime maxRenderTime es 
      let colorMap = defaultColorMap
      foldM (\map (i,e) -> drawGlyph i e map) colorMap glyphs
      
      return ()
