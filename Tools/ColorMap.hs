-- |Color cycling routines. 
-- This module provides functions for maintaining a map from arbitrary strings to colors and 
-- generating new colors for unknown names by cycling over the RGB spectrum.
-- 
-- P.S. We use the spelling "color" in our identifiers for no particular reason 
-- other than internal consistency.
module Tools.ColorMap (
  ColorMap,
  defaultColorMap,
  cycleColor,
  computeColor)
where
import Data.Bits
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import qualified Data.ByteString.Char8 as S
import qualified Data.Map as M

data (Show b, Eq b, Ord b, Floating b) => ColorMap b = ColorMap {
  colorMap :: M.Map S.ByteString (RGB b), -- ^ Current map from arbitrary strings to color descriptions
  colorWheel :: [RGB b]             -- ^ Next colors for assigning to as yet unknown names
  } deriving (Eq, Show)
  
-- | Starts with empty names-colors map and mid-range grey
defaultColorMap = ColorMap M.empty defaultColorWheel

defaultColorWheel = map toSRGB [green, blue, red, brown, orange, magenta, grey, purple, violet, lightblue, crimson, burlywood] ++ map nextColor defaultColorWheel

-- | Compute color for a given name within the associated map.
-- This function encapsulates the following rules:
-- 
--  * If @color@ is a 6-digit hexadecimal value of the form '#FA34B7' then this is used as aan immediate
--    RGB color,
--
--  * If @color@ is a color name from SVG1.1 specification (http://www.w3.org/TR/SVG11/types.html#ColorKeywords)
--    then the corresponding color is returned,
--
--  * Otherwise, the @color@ name is looked up in the @map@ and if it is not found, a new color is generated
--    using a simple cycling function.
computeColor map color = case readColor color of 
  Nothing -> cycleColor map color
  Just c  -> (c, map)
    
readColor = readColor' . S.unpack

readColor' ('#':r1:r2:g1:g2:b1:b2:[]) = Just (RGB r g b)
  where
    r = fromIntegral $ unhex r2 + 16*unhex r1
    g = fromIntegral $ unhex g2 + 16*unhex g1
    b = fromIntegral $ unhex b2 + 16*unhex b1
    unhex c | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
            | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a'
            | c >= 'A' && c <= 'Z' = 10 + fromEnum c - fromEnum 'A'
readColor' cs = toSRGB `fmap` readColourName cs
    
    
-- | Compute the color associated to a given name, providing an updated map with
-- possibly new colors in the cycle.
cycleColor :: (RealFrac b, Show b, Eq b, Ord b, Floating b) => 
              ColorMap b
              -> S.ByteString 
              -> (RGB b,ColorMap b)
cycleColor map name = case M.lookup name (colorMap map) of
  Just c  -> (c, map)
  Nothing -> (next, augment map (name,next) wheel')
  where
    (next:wheel') = colorWheel map
  

nextColor :: (RealFrac b, Floating b) => 
             RGB b -> 
             RGB b
nextColor (RGB r g b) = RGB (r+7) (g+17) (b+23)
    
augment :: (Show b, Eq b, Ord b, Floating b) => 
             ColorMap b -> 
             (S.ByteString, RGB b) -> 
             [RGB b] ->
             ColorMap b
augment map (name,col) wheel = ColorMap (M.insert name col (colorMap map)) wheel
  
