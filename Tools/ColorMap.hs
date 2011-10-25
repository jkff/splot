-- |Color cycling routines. 
-- This module provide function for maintaining a map from arbitrary strings to colorus and 
-- generating new colours for unknown names by cycling over the RGB spectrum.
module Tools.ColorMap (
  ColorMap,
  defaultColorMap,
  cycleColor)
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
  
