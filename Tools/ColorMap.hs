-- |Color cycling routines. 
-- This module provides functions for maintaining a map from arbitrary strings to colors and 
-- generating new colors for unknown names by cycling over the RGB spectrum.
-- 
-- P.S. We use the spelling "color" in our identifiers for no particular reason 
-- other than internal consistency.
module Tools.ColorMap (
  ColorMap,
  prepareColorMap,
  cycleColor,
  computeColor)
where
import Data.Bits
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import Data.Maybe
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M

data ColorMap = ColorMap {
  colorMaps :: M.Map S.ByteString ColorMap1 -- ^ Color scheme id -> Color map
}

data ColorMap1 = ColorMap1 {
  colorMap :: M.Map S.ByteString (RGB Double), -- ^ Current map from arbitrary strings to color descriptions
  colorWheel :: [RGB Double]             -- ^ Next colors for assigning to as yet unknown names
  } deriving (Eq, Show)
  
prepareColorMap :: [(S.ByteString, [S.ByteString])] -> ColorMap
prepareColorMap ms = ColorMap $ M.fromList $ (S.pack "", defaultColorMap):
  [(scheme, ColorMap1 M.empty $ cycle $ map (toSRGB . fromJust . readColor . S.unpack) colorNames) | (scheme, colorNames) <- ms]

-- | Starts with empty names-colors map and mid-range grey
defaultColorMap = ColorMap1 M.empty defaultColorWheel

defaultColorWheel = map toSRGB [green, blue, red, brown, orange, magenta, grey, purple, violet, lightblue, crimson, burlywood] ++ map nextColor defaultColorWheel

-- | Compute color for a given name within the associated map.
-- This function encapsulates the following rules:
-- 
--  * If @color@ is a 6-digit hexadecimal value of the form '#FA34B7' then this is used as an immediate
--    RGB color,
--
--  * If @color@ is a color name from SVG1.1 specification (http://www.w3.org/TR/SVG11/types.html#ColorKeywords)
--    then the corresponding color is returned,
--
--  * Otherwise, the @color@ name is looked up in the @map@ and if it is not found, a new color is generated
--    using a simple cycling function.
computeColor map color = case readColor (S.unpack color) of 
  Nothing -> cycleColor map color
  Just c  -> (toSRGB c, map)
    
readColor cs@('#':_) = Just (sRGB24read cs)
readColor cs         = readColourName cs
    
cycleColor :: ColorMap
              -> S.ByteString 
              -> (RGB Double,ColorMap)
cycleColor (ColorMap map) name = case M.lookup scheme map of
    Nothing -> cycleColor (ColorMap map) S.empty -- Use default color scheme then.
    Just m  -> let (res, m') = cycleColor1 m subColor in (res, ColorMap $ M.insert scheme m' map)
  where
    (scheme, subColor) = case S.uncons name of
      -- /scheme/color
      Just ('/', name') -> S.break (=='/') name'
      _                 -> (S.empty, name)

-- | Compute the color associated to a given name, providing an updated map with
-- possibly new colors in the cycle.
cycleColor1 :: ColorMap1
              -> S.ByteString 
              -> (RGB Double, ColorMap1)
cycleColor1 map name = case M.lookup name (colorMap map) of
  Just c  -> (c, map)
  Nothing -> (next, augment map (name,next) wheel')
  where
    (next:wheel') = colorWheel map
  

nextColor :: RGB Double -> RGB Double
nextColor (RGB r g b) = RGB (r+7) (g+17) (b+23)
    
augment :: ColorMap1 -> (S.ByteString, RGB Double) -> [RGB Double] -> ColorMap1
augment map (name,col) wheel = ColorMap1 (M.insert name col (colorMap map)) wheel
  
