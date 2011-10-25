-- |Color cycling routines. 
-- This module provide function for maintaining a map from arbitrary strings to colorus and 
-- generating new colours for unknown names by cycling over the RGB spectrum.
module Tools.ColorMap (
  ColorMap,
  defaultColorMap,
  cycleColor,
  computeColour)
where
import Data.Bits
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import qualified Data.Map as M

data (Show b, Eq b, Ord b, Floating b) => ColorMap b = ColorMap {
  colorMap :: M.Map String (Colour b), -- ^ Current map from arbitrary strings to colour descriptions
  lastColor :: Colour b                -- ^ Next color for assigning to as yet unknown names
  } deriving (Eq, Show)
  
-- | Starts with empty names-colors map and mid-range grey
defaultColorMap = ColorMap M.empty (sRGB24 128 128 128)

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
    
    
-- | Compute the colour associated to a given name, providing an updated map with
-- possibly new colours in the cycle.
cycleColor :: (RealFrac b, Show b, Eq b, Ord b, Floating b) => 
              ColorMap b
              -> String 
              -> (Colour b,ColorMap b)
cycleColor map name = case M.lookup name (colorMap map) of
  Just c  -> (c, map)
  Nothing -> (lastColor map, augment map (name,next))
  where
    next = nextColor (lastColor map)
  

nextColor :: (RealFrac b, Floating b) => 
             Colour b -> 
             Colour b
nextColor last = sRGB24 (r+7) (g+17) (b+23)
  where
    RGB r g b = toSRGB24 last
    
augment :: (Show b, Eq b, Ord b, Floating b) => 
             ColorMap b -> 
             (String, Colour b) -> 
             ColorMap b
augment map (name,col) = ColorMap (M.insert name (lastColor map) (colorMap map)) col
  
