{-# LANGUAGE BangPatterns, GeneralizedNewtypeDeriving, ScopedTypeVariables #-}
module Tools.StatePlot ( renderEvents
                       , BarHeight(..)
                       , CProgram
                       , parse
                       , RenderConfiguration(..)
                       ) where

import Control.Monad.State.Strict as ST
import qualified Data.Map as M
import Data.List

import Data.Time

import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.ByteString.Char8 as S
import Data.ByteString.Lex.Double
import qualified Graphics.Rendering.Cairo as C
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Char (isSpace)

import Tools.ColorMap

data Event = Event { localTime :: !LocalTime, utcTime :: !UTCTime, track :: !S.ByteString, edge :: !Edge } deriving (Show)
data Event' = Event' { msTime :: !Double, track' :: !S.ByteString, edge' :: !Edge } deriving (Show)
data Edge = Begin { color :: !S.ByteString }
          | End { color :: !S.ByteString }
          | Both { duration :: Double, color :: !S.ByteString }
          | Pulse { glyph :: !Glyph, color :: !S.ByteString }
          deriving (Show)

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
    arg = if B.null arg0 then S.empty else trim $ repack (B.tail arg0)
    edge = case (B.head track') of
      '>' -> Begin (if S.null arg then grayStr else arg)
      '<' -> End   (if S.null arg then S.empty else arg)
      '=' -> Both  duration color
        where (durationS, color0) = S.break (==' ') arg
              color = S.tail color0
              Just (duration, _) = readDouble durationS
      '!' -> Pulse (GlyphText text) color
        where (color, text0) = S.break (==' ') arg
              text = S.tail text0
    trim = fst . S.spanEnd isSpace

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
        phantomColor :: Maybe S.ByteString,
        fromTime :: Maybe LocalTime,
        toTime :: Maybe LocalTime,
        forcedNumTracks :: Maybe Int,
        colorWheels :: [(S.ByteString, [S.ByteString])],
        legendWidth :: Maybe Int
    }

data TickSize = LargeTick | SmallTick

newtype RenderState s a = RenderState { runRenderState :: StateT s C.Render a } deriving (Monad, MonadState s)

type CProgram = (Double, Double) -> C.Render ()

liftR :: C.Render () -> RenderState s ()
liftR r = RenderState $ lift r

colourChannel c = darken (recip (alphaChannel c)) (c `over` black)
setSourceColor c = let (RGB r g b) = toSRGB $ colourChannel c
                   in C.setSourceRGBA r g b (alphaChannel c)

fillRect, strokeLine :: Double -> Double -> Double -> Double -> C.Render ()
fillRect   !x1 !y1 !x2 !y2 = C.rectangle x1 y1 (x2-x1) (y2-y1) >> C.fill
strokeLine !x1 !y1 !x2 !y2 = C.moveTo x1 y1 >> C.lineTo x2 y2 >> C.stroke

lineStyle dashes c = do
  C.setLineWidth 1
  C.setDash dashes 0
  C.setLineCap C.LineCapButt
  C.setLineJoin C.LineJoinMiter
  setSourceColor c

renderEvents :: RenderConfiguration -> IO [Event] -> CProgram
renderEvents conf readEs = renderGlyphsAtFront $ liftIO readEs
  where 
    {-# INLINE maybeM #-}
    maybeM :: (Monad m) => (a -> m b) -> Maybe a -> m ()
    maybeM f Nothing  = return ()
    maybeM f (Just x) = f x >> return ()

    -- Returns: whether we have any non-bars glyphs (text)
    genGlyphs :: (UTCTime -> Double) -> Double -> [Event] -> Bool 
                 -> (Int -> OutputGlyph -> RenderState s ())  -- Glyph
                 -> (Int -> S.ByteString -> RenderState s ()) -- Legend item
                 -> RenderState s Bool
    genGlyphs time2ms !rangeMs es drawGlyphsNotBars withGlyph withLegend = genGlyphs' (map parseTime es) M.empty False
      where
        parseTime (Event _ t track edge) = Event' (time2ms t) track edge
        genGlyphs' [] m !havePulses = when (not drawGlyphsNotBars) (mapM_ flushTrack (M.toList m)) >> return havePulses
          where
            flushTrack (track, (i, Nothing)) = return ()
            flushTrack (track, (i, Just (mst0,c0))) = do
              if ((expireTimeMs conf > 0 && rangeMs - mst0 >= expireTimeMs conf) ||
                  expireTimeMs conf == 0)
                then withGlyph i (ExpiredBar mst0 (if expireTimeMs conf == 0
                                                   then rangeMs
                                                  else (mst0 + expireTimeMs conf))
                                                  c0)
                else withGlyph i (Bar        mst0 rangeMs c0)
        genGlyphs' ((e@(Event' mst track edge)):es) m !havePulses = do
          ((i,ms0), m') <- summon e mst m
          case (drawGlyphsNotBars, edge) of
            (True, Pulse glyph color) -> do
              withGlyph i (OutPulse mst glyph color)
              genGlyphs' es m' True
            (True, _) -> do
              genGlyphs' es m' havePulses
            (False, Pulse glyph color) -> do
              genGlyphs' es m' True
            (False, Both duration color) -> do
              let (start, end) = if (duration > 0)
                                 then (mst, mst + duration)
                                 else (mst + duration, mst)
              let eStart = Event' start track (Begin color)
              let eEnd   = Event' end   track (End S.empty)
              let eRestart = case ms0 of { Just (_, c0) -> [Event' end track (Begin c0)]; _ -> [] }
              genGlyphs' ([eStart,eEnd] ++ eRestart ++ es) m havePulses
            (False, _) -> do
              flip maybeM ms0 $ \(mst0,c0) -> do
                let overrideEnd c0 = case edge of { End c | not (S.null c) -> c; _ -> c0 }
                if (mst - mst0 >= expireTimeMs conf && expireTimeMs conf > 0)
                  then withGlyph i (ExpiredBar mst0 (mst0 + expireTimeMs conf) (overrideEnd c0))
                  else withGlyph i (Bar        mst0 mst                        (overrideEnd c0))
              let m'' = M.insert track (i, case edge of { Begin c -> Just (mst,c); End _ -> Nothing }) m'
              genGlyphs' es m'' havePulses
        
        summon (Event' t track edge) mst m = case M.lookup track m of
          Just x  -> return (x, m)
          Nothing -> do
            let i = M.size m
            let m' = M.insert track (i, Nothing) m
            withLegend i track
            when (not drawGlyphsNotBars) $ case (phantomColor conf, edge) of
              (_,      End c) | not (S.null c) -> withGlyph i (Bar 0 mst c)
              (Just c, End _)                  -> withGlyph i (Bar 0 mst c)
              _                                -> return ()
            return ((i,Nothing), m')

    renderGlyphsAtFront :: C.Render [Event] -> CProgram
    renderGlyphsAtFront readEs (w,h) = do
      setSourceColor $ opaque white
      fillRect 0 0 w h
      haveGlyphs <- render' readEs (w,h) False
      when haveGlyphs $ (render' readEs (w,h) True >> return ())

    computeTimesTracks es = (a, b, M.size m)
      where
        (Just a, Just b, m) = foldl' f (Nothing, Nothing, M.empty) es
        f (!minRT, !maxRT, !tracks) e = (orJust min minRT (localTime e), orJust max maxRT (localTime e), M.insert (track e) () tracks)
        orJust f Nothing   x = Just x
        orJust f (Just !x0) x = Just (f x0 x)

    override (ft, tt, nt) = (override' ft (fromTime conf), override' tt (toTime conf), override' nt (forcedNumTracks conf))
    override' x (Just y) = y
    override' x Nothing  = x

    render' :: C.Render [Event] -> (Double,Double) -> Bool -> C.Render Bool
    render' readEs (w,h) drawGlyphsNotBars = do
      es <- readEs

      (minRenderLocalTime, maxRenderLocalTime, numTracks) <- case (fromTime conf, toTime conf, forcedNumTracks conf) of
        (Just a, Just b, Just c) -> return (a, b, c)
        (_, _, _)                -> fmap (override . computeTimesTracks) readEs

      let (minRenderTime, maxRenderTime) = (localTimeToUTC utc minRenderLocalTime, localTimeToUTC utc maxRenderLocalTime)
      let rangeMs = diffToMillis maxRenderTime minRenderTime

      let time2ms t | t < minRenderTime = 0
                    | t > maxRenderTime = rangeMs
                    | otherwise         = diffToMillis t minRenderTime

      let ticks = takeWhile ((<rangeMs).snd) $ 
            map (\i -> if i`mod`largeTickFreq conf == 0 
                       then (LargeTick, fromIntegral i*tickIntervalMs conf) 
                       else (SmallTick, fromIntegral i*tickIntervalMs conf)) [0..]

      let legendW = case legendWidth conf of { Just w -> fromIntegral w; Nothing -> 0 }
      let ms2x ms = legendW + 10 + ms / rangeMs * (w - 10 - legendW)
      let yStep = case barHeight conf of
            BarHeightFixed _ -> (h-20) / fromIntegral (numTracks+1)
            BarHeightFill    -> (h-20) / fromIntegral numTracks

      let track2y i = case barHeight conf of
            BarHeightFixed bh -> fromIntegral (i+1) * yStep - bh/2
            BarHeightFill     -> fromIntegral (i+1) * yStep - yStep/2

      let drawTick (t, ms) = strokeLine (ms2x ms) (h-20)
                                        (ms2x ms) (h-case t of { LargeTick -> 13 ; SmallTick -> 17 })

      let getColor :: S.ByteString -> RenderState ColorMap (RGB Double)
          getColor c = do
            map <- ST.get
            let (color, map') = computeColor map c
            ST.put map'
            return color

      let drawGlyph :: Int -> OutputGlyph -> RenderState ColorMap ()
          drawGlyph !i (Bar !ms1 !ms2 !color)
            | drawGlyphsNotBars = return () 
            | otherwise = getColor color >>= \(RGB r g b) -> liftR $ do
              C.setSourceRGB r g b
              let y = track2y i
              case barHeight conf of
                BarHeightFixed bh -> fillRect (ms2x ms1) (y - bh   /2) (ms2x ms2) (y + bh   /2)
                BarHeightFill     -> fillRect (ms2x ms1) (y - yStep/2) (ms2x ms2) (y + yStep/2)

          drawGlyph i (ExpiredBar ms1 ms2 color) 
            | drawGlyphsNotBars = return ()
            | otherwise = getColor color >>= \(RGB r g b) -> liftR $ do
              lineStyle [3,3] $ opaque $ sRGB r g b
              let y = track2y i
              strokeLine (ms2x ms1) y (ms2x ms2) y
              lineStyle [] $ opaque red
              strokeLine (ms2x ms2 - 5) (y - 5) (ms2x ms2 + 5) (y + 5)
              strokeLine (ms2x ms2 + 5) (y - 5) (ms2x ms2 - 5) (y + 5)

          drawGlyph i (OutPulse ms glyph color) 
            | not drawGlyphsNotBars = return () 
            | otherwise = getColor color >>= \(RGB r g b) -> liftR $ case glyph of
              GlyphText text -> do
                lineStyle [] $ opaque $ sRGB r g b
                let y = track2y i
                C.moveTo (ms2x ms) y
                C.showText (S.unpack text)

      let drawLegendItem :: Int -> S.ByteString -> RenderState ColorMap ()
          drawLegendItem !i !s = liftR $ case legendWidth conf of
            Nothing -> return ()
            Just w -> do
              lineStyle [] $ opaque black
              let y = track2y i
              C.TextExtents xbear ybear tw th _ _ <- C.textExtents (S.unpack s)
              C.moveTo (fromIntegral w - tw - xbear - 5) (y - th/2 - ybear)
              C.showText (S.unpack s)

      when (not drawGlyphsNotBars) $ do
        C.setAntialias C.AntialiasNone
        lineStyle [] $ opaque black

        strokeLine (legendW+10) (h-20) w (h-20)
        strokeLine (legendW+10) (h-20) (legendW+10) 0
        mapM_ drawTick ticks
        C.moveTo (legendW+10) (h-3)
        C.setAntialias C.AntialiasGray
        C.setFontSize 12
        C.showText $ "Origin at " ++ show minRenderLocalTime ++ ", 1 small tick = " ++ show (tickIntervalMs conf) ++ "ms"

      C.setAntialias C.AntialiasSubpixel
      let colorMap = prepareColorMap (colorWheels conf)
      evalStateT (runRenderState $ genGlyphs time2ms rangeMs es drawGlyphsNotBars drawGlyph drawLegendItem) colorMap

