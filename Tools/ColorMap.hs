module Tools.ColorMap where
import Data.Colour
import Data.Colour.SRGB
import Data.Colour.Names
import qualified Data.Map as M

data (Show b, Eq b, Ord b, Floating b) => ColorMap b = ColorMap {
  colorMap :: M.Map String (Colour b), -- ^ Current map from arbitrary strings to colour descriptions
  lastColor :: Colour b                -- ^ Next color for assigning to as yet unknown names
  } deriving (Eq, Show)
  
-- | Starts with empty association and mid-range grey
defaultColorMap = ColorMap M.empty (sRGB24 128 128 128)

-- | Compute the colour associated to a given name, providing an updated map with
-- possibly new colours in the cycle.
cycleColor :: (RealFrac b, Show b, Eq b, Ord b, Floating b) => 
              ColorMap b
              -> String 
              -> (Maybe (Colour b),ColorMap b)
cycleColor map name = case M.lookup name (colorMap map) of
  Just c  -> (Just c, map)
  Nothing -> (Just (lastColor map), augment map (name,next))
  where
    next = nextColor map
  

nextColor :: (RealFrac b, Show b, Eq b, Ord b, Floating b) => 
             ColorMap b -> 
             Colour b
nextColor (ColorMap _ last) = (sRGB24 (r+17 `mod` 255) (g+19 `mod` 255) (b+23 `mod` 255))
  where
    RGB r g b = toSRGB24 last
  
augment :: (Show b, Eq b, Ord b, Floating b) => 
             ColorMap b -> 
             (String, Colour b) -> 
             ColorMap b
augment map (name,col) = ColorMap (M.insert name (lastColor map) (colorMap map)) col
  
