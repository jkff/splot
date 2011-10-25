module Tools.SPlotTest where
import Test.HUnit

import Data.Colour
import Data.Colour.SRGB

import Tools.ColorMap

import Data.Map as M

import System.IO(stderr)
import System.Exit


colorCyclingTests = TestList [
  cycleColor defaultColorMap "bar"   ~?= (base, colorMap "bar")
  ,cycleColor (colorMap "foo") "bar" ~?= (nextColor, augmentedColorMap "bar")
  ,cycleColor (colorMap "foo") "baz" ~?= (nextColor, augmentedColorMap "baz")
  ]
  where
    base                   = (sRGB24 128 128 128)
    nextColor              = sRGB24 (128+7) (128+17) (128+23)
    nextNextColor          = sRGB24 (128+7+7) (128+17+17) (128+23+23)
    colorsAssoc       name = [(name,(sRGB24 128 128 128))]
    colorMap          name = ColorMap (M.fromList (colorsAssoc name)) nextColor
    augmentedColorMap name = augment (colorMap "foo") (name, nextNextColor)
    
splotTests = TestList [
  colorCyclingTests 
  ]
             
runAllTests tests = do counts <- runTest tests
                       case (errors counts + failures counts) of
                         0 -> return ExitSuccess
                         n -> return (ExitFailure n)

runTest :: Test -> IO Counts
runTest  t = do (counts, _) <- runTestText (putTextToHandle stderr False) t
		return counts
