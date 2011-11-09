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

data Event = Event { localTime :: !LocalTime, utcTime :: !UTCTime, track :: !S.ByteString, edge :: !Edge } deriving (Show)
data Edge = Begin { color :: !S.ByteString } | End { color :: !S.ByteString } | Pulse { glyph :: !Glyph, color :: !S.ByteString } deriving (Show)

-- Text is enough for most purposes (|, o, <, >, ...) but potentially more can exist.
data Glyph = GlyphText { text :: !S.ByteString } deriving (Show)

data OutputGlyph = Bar {-# UNPACK #-} !Double !Double !S.ByteString 
                 | ExpiredBar {-# UNPACK #-} !Double !Double !S.ByteString 
                 | OutPulse {-# UNPACK #-} !Double !Glyph !S.ByteString deriving (Show)

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

    -- Returns: whether we have any non-bars glyphs (text)
    genGlyphs :: (UTCTime -> Double) -> Double -> [Event] -> Bool -> (Int -> OutputGlyph -> RenderState s ()) -> RenderState s Bool
    genGlyphs time2ms !rangeMs es drawGlyphsNotBars withGlyph = genGlyphs' es M.empty False
      where
        genGlyphs' [] m !havePulses = when (not drawGlyphsNotBars) (mapM_ flushTrack (M.toList m)) >> return havePulses
          where
            flushTrack (track, (i, Nothing)) = return ()
            flushTrack (track, (i, Just (mst0,c0))) = do
              if (rangeMs - mst0 < expireTimeMs conf)
                then withGlyph i (Bar        mst0 rangeMs                    c0)
                else withGlyph i (ExpiredBar mst0 (mst0 + expireTimeMs conf) c0)
        genGlyphs' ((e@(Event _ t track edge)):es) m !havePulses = do
          let mst = time2ms t
          ((i,ms0), m') <- summon e mst m
          case (drawGlyphsNotBars, edge) of
            (True, Pulse glyph color) -> do
              withGlyph i (OutPulse mst glyph color)
              genGlyphs' es m' True
            (True, _) -> do
              genGlyphs' es m' havePulses
            (False, Pulse glyph color) -> do
              genGlyphs' es m' True
            (False, _) -> do
              flip maybeM ms0 $ \(mst0,c0) -> do
                let overrideEnd c0 = case edge of { End c | not (S.null c) -> c; _ -> c0 }
                if (mst - mst0 < expireTimeMs conf)
                  then withGlyph i (Bar        mst0 mst                        (overrideEnd c0))
                  else withGlyph i (ExpiredBar mst0 (mst0 + expireTimeMs conf) (overrideEnd c0))
              let m'' = M.insert track (i, case edge of { Begin c -> Just (mst,c); End _ -> Nothing }) m'
              genGlyphs' es m'' havePulses
        
        summon (Event _ t track edge) mst m = case M.lookup track m of
          Just x  -> return (x, m)
          Nothing -> do
            let i = M.size m
            let m' = M.insert track (i, Nothing) m
            when (not drawGlyphsNotBars) $ case (phantomColor conf, edge) of
              (_,      End c) | not (S.null c) -> withGlyph i (Bar 0 mst c)
              (Just c, End _)                  -> withGlyph i (Bar 0 mst c)
              _                                -> return ()
            return ((i,Nothing), m')

    renderGlyphsAtFront :: CRender [Event] -> (Double,Double) -> CRender (PickFn a)
    renderGlyphsAtFront readEs (w,h) = do
      setFillStyle $ solidFillStyle (opaque white)
      fillPath $ rectPath $ Rect (Point 0 0) (Point w h)
      haveGlyphs <- render' readEs (w,h) False
      when haveGlyphs $ (render' readEs (w,h) True >> return ())
      return nullPickFn

    computeTimesTracks es = (a, b, M.size m)
      where
        (Just a, Just b, m) = foldl' f (Nothing, Nothing, M.empty) es
        f (!minRT, !maxRT, !tracks) e = (orJust min minRT (localTime e), orJust max maxRT (localTime e), M.insert (track e) () tracks)
        orJust f Nothing   x = Just x
        orJust f (Just x0) x = Just (f x0 x)

    override (ft, tt, nt) = (override' ft (fromTime conf), override' tt (toTime conf), override' nt (forcedNumTracks conf))
    override' x (Just y) = y
    override' x Nothing  = x

    render' :: CRender [Event] -> (Double,Double) -> Bool -> CRender Bool
    render' readEs (w,h) drawGlyphsNotBars = do
      es <- readEs

      (minRenderLocalTime, maxRenderLocalTime, numTracks) <- case (fromTime conf, toTime conf, forcedNumTracks conf, streaming conf) of
        (Just a, Just b, Just c, _) -> return (a, b, c)
        (_, _, _, True)             -> fmap (override . computeTimesTracks) readEs
        (_, _, _, False)            -> return $ override (computeTimesTracks es) -- Will evaluate the whole of 'es' and hold it in memory

      let (minRenderTime, maxRenderTime) = (localTimeToUTC utc minRenderLocalTime, localTimeToUTC utc maxRenderLocalTime)
      let rangeMs = diffToMillis maxRenderTime minRenderTime

      let time2ms t | t < minRenderTime = 0
                    | t > maxRenderTime = rangeMs
                    | otherwise         = diffToMillis t minRenderTime

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
         
      let fillRect   !x1 !y1 !x2 !y2 = C.rectangle x1 y1 (x2-x1) (y2-y1) >> C.fill
      let strokeLine !x1 !y1 !x2 !y2 = C.moveTo x1 y1 >> C.lineTo x2 y2 >> C.stroke

      let getColor :: S.ByteString -> RenderState (ColorMap Double) (RGB Double)
          getColor c = do {
            map <- ST.get
          ; let (color, map') = computeColor map c
          ; ST.put map'
          ; return color
          }

      let drawGlyph :: Int -> OutputGlyph -> RenderState (ColorMap Double) ()
          drawGlyph !i (Bar !ms1 !ms2 !color)
            | drawGlyphsNotBars = return () 
            | otherwise = getColor color >>= \(RGB r g b) -> liftR $ c $ do {
            C.setSourceRGB r g b
          ; let y = track2y i
          ; case barHeight conf of {
              BarHeightFixed bh -> fillRect (ms2x ms1) (y - bh   /2) (ms2x ms2) (y + bh   /2)
            ; BarHeightFill     -> fillRect (ms2x ms1) (y - yStep/2) (ms2x ms2) (y + yStep/2)
            } 
          ; return ()
          }
          drawGlyph i (ExpiredBar ms1 ms2 color) 
            | drawGlyphsNotBars = return ()
            | otherwise = getColor color >>= \(RGB r g b) -> liftR $ do {
            setLineStyle $ dashedLine 1 [3,3] (opaque $ sRGB r g b)
          ; let y = track2y i
          ; c $ strokeLine (ms2x ms1) y (ms2x ms2) y
          ; setLineStyle $ solidLine 1 (opaque red)
          ; c $ strokeLine (ms2x ms2 - 5) (y - 5) (ms2x ms2 + 5) (y + 5)
          ; c $ strokeLine (ms2x ms2 + 5) (y - 5) (ms2x ms2 - 5) (y + 5)
          ; return ()
          }
          drawGlyph i (OutPulse ms glyph color) 
            | not drawGlyphsNotBars = return () 
            | otherwise = getColor color >>= \(RGB r g b) -> liftR $ case glyph of {
            GlyphText text -> do {
              setLineStyle $ solidLine 1 (opaque $ sRGB r g b)
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
      evalStateT (runRenderState $ genGlyphs time2ms rangeMs es drawGlyphsNotBars drawGlyph) defaultColorMap
