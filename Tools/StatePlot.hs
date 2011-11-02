{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Tools.StatePlot where

import Control.Monad.State.Strict as ST
import qualified Data.Map as M
import Data.List
import Data.Ord
import Data.Function
import Data.Maybe

import Data.Time
import Data.Time.Parse

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as S
import Graphics.Rendering.Chart
import qualified Graphics.Rendering.Cairo.Internal as CI
import qualified Graphics.Rendering.Cairo as C
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names

import Tools.ColorMap

import Debug.Trace

data Event = Event { localTime :: LocalTime, utcTime :: UTCTime, track :: S.ByteString, edge :: Edge } deriving (Show)
data Edge = Begin { color :: S.ByteString } | End { color :: S.ByteString } | Pulse { glyph :: Glyph, color :: S.ByteString } deriving (Show)

-- Text is enough for most purposes (|, o, <, >, ...) but potentially more can exist.
data Glyph = GlyphText { text :: S.ByteString } deriving (Show)

data OutputGlyph = Bar Double Double S.ByteString | ExpiredBar Double Double S.ByteString | OutPulse Double Glyph S.ByteString deriving (Show)

parse :: (B.ByteString -> (LocalTime, B.ByteString)) -> B.ByteString -> Event
parse parseTime s = Event { localTime = ts, utcTime = localTimeToUTC utc ts, track = repack $ B.tail track', edge = edge }
  where
    (ts, s') = parseTime s
    (track', arg0) = B.break (==' ') (B.tail s')
    arg = if B.null arg0 then S.empty else repack (B.tail arg0)
    edge = case (B.head track') of
      '>' -> Begin (if S.null arg then grayStr else arg)
      '<' -> End   (if S.null arg then S.empty else arg)
      '!' -> Pulse (GlyphText text) color
        where (color, text0) = S.break (==' ') arg
              text = S.tail text0

repack = S.concat . B.toChunks

grayStr = S.pack "gray"

{-# INLINE diffToMillis #-}
diffToMillis :: UTCTime -> UTCTime -> Double
diffToMillis !t2 !t1 = 86400000.0 * fromIntegral dd + fromRational (toRational (tod2-tod1))*1000.0
  where
    (d1,d2,tod1,tod2) = (utctDay t1, utctDay t2, utctDayTime t1, utctDayTime t2)
    dd = toModifiedJulianDay d2 - toModifiedJulianDay d1

data BarHeight = BarHeightFixed Double | BarHeightFill

data RenderConfiguration = RenderConf {
        barHeight :: BarHeight,
        tickIntervalMs :: Double,
        largeTickFreq :: Int,
        expireTimeMs :: Double,
        cmpTracks :: Event -> Event -> Ordering,
        phantomColor :: Maybe S.ByteString,
        fromTime :: Maybe LocalTime,
        toTime :: Maybe LocalTime,
        forcedNumTracks :: Maybe Int,
        streaming :: Bool
    }

data TickSize = LargeTick | SmallTick

newtype RenderState s a = RenderState { runRenderState :: StateT s CRender a } deriving (Monad, MonadState s)

liftR :: CRender () -> RenderState s ()
liftR r = RenderState $ lift r

renderEvents :: RenderConfiguration -> IO [Event] -> IO (Renderable ())
renderEvents conf readEs = if streaming conf 
                           then return (Renderable {minsize = return (0,0), render = renderGlyphsAtFront (c $ liftIO readEs)})
                           else readEs >>= \es -> return $ Renderable {minsize = return (0,0), render = renderGlyphsAtFront (return es)}
  where 
    {-# INLINE maybeM #-}
    maybeM :: (Monad m) => (a -> m b) -> Maybe a -> m ()
    maybeM f Nothing  = return ()
    maybeM f (Just x) = f x >> return ()

    genGlyphs :: (UTCTime -> Double) -> Double -> [Event] -> Bool -> ((Int,OutputGlyph) -> RenderState (ColorMap Double) ()) -> RenderState (ColorMap Double) ()
    genGlyphs time2ms rangeMs es drawGlyphsNotBars withGlyph = genGlyphs' es M.empty
      where
        genGlyphs' [] m = if drawGlyphsNotBars then return () else mapM_ flushTrack (M.toList m)
          where
            flushTrack (track, (i, Nothing)) = return ()
            flushTrack (track, (i, Just (mst0,c0))) = do
              if (rangeMs - mst0 < expireTimeMs conf)
                then withGlyph (i, Bar        mst0 rangeMs                    c0)
                else withGlyph (i, ExpiredBar mst0 (mst0 + expireTimeMs conf) c0)
        genGlyphs' ((e@(Event _ t track edge)):es) m = do
          let mst = time2ms t
          ((i,ms0), m') <- summon e mst m
          if drawGlyphsNotBars 
            then do
              case edge of {Pulse glyph c -> withGlyph (i, OutPulse mst glyph c); _ -> return ()}
              genGlyphs' es m'
            else do
              flip maybeM ms0 $ \(mst0,c0) -> do
                let overrideEnd c0 = case edge of { End c | not (S.null c) -> c; _ -> c0 }
                if (mst - mst0 < expireTimeMs conf)
                  then withGlyph (i, Bar        mst0 mst                        (overrideEnd c0))
                  else withGlyph (i, ExpiredBar mst0 (mst0 + expireTimeMs conf) (overrideEnd c0))
              let m'' = M.insert track (i, case edge of { Begin c -> Just (mst,c); End _ -> Nothing }) m'
              genGlyphs' es m''
        
        summon (Event _ t track edge) mst m = case M.lookup track m of
          Just x  -> return (x, m)
          Nothing -> do
            let i = M.size m
            let m' = M.insert track (i, Nothing) m
            when (not drawGlyphsNotBars) $ case (phantomColor conf, edge) of
              (_,      End c) | not (S.null c) -> withGlyph (i, Bar 0 mst c)
              (Just c, End _)                  -> withGlyph (i, Bar 0 mst c)
              _                                -> return ()
            return ((i,Nothing), m')

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
      minRenderLocalTime <- case (fromTime conf, streaming conf) of { 
        (Just t, _)      -> return t;
        (Nothing, True)  -> fmap (minimum . map localTime) readEs; 
        (Nothing, False) -> return . minimum . map localTime $ es -- Will evaluate the whole of 'es' and hold it in memory
      } 
      let minRenderTime = localTimeToUTC utc minRenderLocalTime
      maxRenderTime <- case (toTime conf,   streaming conf) of { 
        (Just t, _)      -> return (localTimeToUTC utc t);
        (Nothing, True)  -> fmap (maximum . map utcTime) readEs; 
        (Nothing, False) -> return . maximum . map utcTime $ es -- Same here.
      } 

      let rangeMs = diffToMillis maxRenderTime minRenderTime

      let time2ms t | t < minRenderTime = 0
                    | t > maxRenderTime = rangeMs
                    | otherwise         = diffToMillis t minRenderTime

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
      let yStep = case barHeight conf of {
          BarHeightFixed _ -> (h-20) / fromIntegral (numTracks+1)
        ; BarHeightFill    -> (h-20) / fromIntegral numTracks
        }
      let track2y i = case barHeight conf of {
          BarHeightFixed bh -> fromIntegral (i+1) * yStep - bh/2
        ; BarHeightFill     -> fromIntegral (i+1) * yStep - yStep/2
        }
      let drawTick (t, ms) = c $ do {
          C.moveTo (ms2x ms) (h-20)
        ; C.lineTo (ms2x ms) (h-case t of { LargeTick -> 13 ; SmallTick -> 17 })
        ; C.stroke
        }
         
      let fillRect   x1 y1 x2 y2 = c $ C.rectangle x1 y1 (x2-x1) (y2-y1) >> C.fill
      let strokeLine x1 y1 x2 y2 = c $ C.moveTo x1 y1 >> C.lineTo x2 y2 >> C.stroke

      let getColor :: S.ByteString -> RenderState (ColorMap Double) (Colour Double)
          getColor c = do {
            map <- ST.get
          ; let (color, map') = computeColor map c
          ; ST.put map'
          ; return color
          }

      let drawGlyph :: Int -> OutputGlyph -> RenderState (ColorMap Double) ()
          drawGlyph !i (Bar !ms1 !ms2 !color)
            | drawGlyphsNotBars = return () 
            | otherwise = getColor color >>= \colorToUse -> liftR $ do {
            c $ setSourceColor (opaque colorToUse)
          ; let y = track2y i
          ; case barHeight conf of {
              BarHeightFixed bh -> fillRect (ms2x ms1) (y - bh   /2) (ms2x ms2) (y + bh   /2)
            ; BarHeightFill     -> fillRect (ms2x ms1) (y - yStep/2) (ms2x ms2) (y + yStep/2)
            } 
          ; return ()
          }
          drawGlyph i (ExpiredBar ms1 ms2 color) 
            | drawGlyphsNotBars = return ()
            | otherwise = getColor color >>= \colorToUse -> liftR $ do {
            setLineStyle $ dashedLine 1 [3,3] (opaque $ colorToUse)
          ; let y = track2y i
          ; strokeLine (ms2x ms1) y (ms2x ms2) y
          ; setLineStyle $ solidLine 1 (opaque red)
          ; strokeLine (ms2x ms2 - 5) (y - 5) (ms2x ms2 + 5) (y + 5)
          ; strokeLine (ms2x ms2 + 5) (y - 5) (ms2x ms2 - 5) (y + 5)
          ; return ()
          }
          drawGlyph i (OutPulse ms glyph color) 
            | not drawGlyphsNotBars = return () 
            | otherwise = getColor color >>= \colorToUse -> liftR $ case glyph of {
            GlyphText text -> do {
              setLineStyle $ solidLine 1 (opaque $ colorToUse)
            ; let y = track2y i
            ; moveTo (Point (ms2x ms) y)
            ; c $ C.showText (S.unpack text)
            ; return ()
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
      c $ C.showText $ "Origin at " ++ show minRenderLocalTime ++ ", 1 small tick = " ++ show (tickIntervalMs conf) ++ "ms"
      setLineStyle $ solidLine 1 (opaque black)
      mapM_ drawTick ticks

      let colorMap = defaultColorMap
      evalStateT (runRenderState $ genGlyphs time2ms rangeMs es drawGlyphsNotBars (uncurry drawGlyph)) defaultColorMap
