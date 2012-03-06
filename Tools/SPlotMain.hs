module Main where

import System.Environment (getArgs)
import System.Exit

import Data.Time
import Data.Time.Parse

import Graphics.Rendering.Chart.Renderable(renderableToPNGFile)

import Data.Maybe(fromMaybe,isNothing)
import Data.Ord(comparing)

import Data.List (tails)

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as B

import Control.Monad (when)

import Tools.StatePlot

getArg :: String -> String -> [String] -> String
getArg name def args = case [(k,v) | (k,v) <- zip args (tail args), k==("-"++name)] of
  (_,v):_ -> v
  _       -> def

showHelp = mapM_ putStrLn [
    "splot - a tool for visualizing the lifecycle of many concurrent multi-stage processes. See http://www.haskell.org/haskellwiki/Splot",
    "Usage: splot [-if INFILE] [-o PNGFILE] [-w WIDTH] [-h HEIGHT] [-bh BARHEIGHT] ",
    "             [-tf TIMEFORMAT] [-sort SORT] [-expire EXPIRE]",
    "             [-fromTime TIME] [-toTime TIME] [-numTracks NUMTRACKS]",
    "             [-tickInterval TICKINTERVAL] [-largeTickFreq N]",
    "             [-stream true] [-colorscheme SCHEME COLORS]...",
    "  -if INFILE    - filename from where to read the trace.",
    "                  If omitted or '-', read from stdin.",
    "  -o PNGFILE    - filename to which the output will be written in PNG format. Required.",
    "  -w, -h        - width and height of the resulting picture. Default 640x480.",
    "  -bh           - height of the bar depicting each individual process. Default 5 pixels.",
    "                  Use 1 or so, or 'fill' if you have a lot of them. ",
    "                  '-bh fill' means 'fill the screen with bars, without vertical gaps'",
    "  -tf           - time format, as in http://linux.die.net/man/3/strptime but with ",
    "                  fractional seconds supported via %OS - will parse 12.4039 or 12,4039",
    "                  Also, %^[+-][N]s will parse seconds since the epoch, for example ",
    "                  %^-3s are milliseconds since the epoch (N can only be 1 digit)",
    "  -tickInterval - ticks on the X axis will be this often (in millis, default 1000).",
    "  -largeTickFreq N - every N'th tick will be larger than the others (default 10).",
    "  -sort SORT    - sort tracks by SORT, where: 'time' - sort by time of first event, ",
    "                  'name' - sort by track name.",
    "  -expire       - expire activities after given time period (in millis) - for instance,",
    "                  to account that if an activity doesn't tell you it's finished for too long,",
    "                  then it probably was killed.",
    "  -phantom COL  - 'phantom color' - if a track starts from a '<' event, it is assumed that",
    "                  the corresponding '>' event (not present in the log) was of color COL.",
    "                  Useful if you're drawing pieces of large logs.",
    "  -fromTime TIME - clip picture on left (time in same format as in trace)",
    "  -toTime TIME   - clip picture on right (time in same format as in trace)",
    "  -numTracks NUMTRACKS - explicitly specify number of tracks when using '-stream true'",
    "  -colorscheme SCHEME COLORS - declare a color scheme (see note about colors at the end).",
    "                  SCHEME is an arbitrary string, e.g.: 'pale' or 'bright'.",
    "                  COLORS is a space-separated list of colors in SVG or hex, e.g. ",
    "                  'red green #0000FF'. You may specify multiple -colorscheme arguments.",
    "",
    "Input is read from stdin. Example input (speaks for itself):",
    "2010-10-21 16:45:09,431 >foo green",
    "2010-10-21 16:45:09,541 >bar green",
    "2010-10-21 16:45:10,631 >foo yellow",
    "2010-10-21 16:45:10,725 >foo #ff0000",
    "2010-10-21 16:45:10,755 >foo /pale/THREAD25",
    "2010-10-21 16:45:10,775 >bar /bright/THREAD37",
    "2010-10-21 16:45:10,836 !foo black Some text",
    "2010-10-21 16:45:10,930 >bar blue",
    "2010-10-21 16:45:11,322 <foo",
    "2010-10-21 16:45:12,508 <bar red",
    "2010-10-21 16:45:10,631 >foo some data",
    "2010-10-21 16:45:10,631 <foo",
    "",
    "'!FOO COLOR TEXT' means 'draw text TEXT with color COLOR on track FOO',",
    "'>FOO COLOR' means 'start a bar of color COLOR on track FOO'.", 
    "'<FOO' means 'end the current bar for FOO'.",
    "'<FOO COLOR' means 'end the current bar for FOO and make the whole bar of color COLOR'",
    "(for example if we found that FOO failed and all the work since >FOO was wasted, COLOR",
    "might be red)",
    "",
    "Note that COLOR may be an hexadecimal RGB specification (like '#4B3AF7'), ",
    " a color name (see SVG 1.1 specifications) or an arbitrary token in which ",
    " case splot will generate a new color for each different token.",
    "COLOR may also have the form '/SCHEME/TOKEN', in which case the colors for ",
    " tokens are cycled within colors specified by --colorscheme for SCHEME.",
    "For example, if you have two types of threads in your program and you want them", 
    " to be colored differently but do not want to assign colors to each thread ",
    " individually, you can use colors named like /pale/THREADID and /bright/THREADID",
    " and specify --colorscheme bright 'red green blue orange yellow' --colorscheme pale ",
    " 'lightgray lightblue pink'.",
    "If you use an unspecified color scheme, or don't specify a color scheme at all, ",
    " the tool resorts to using a default scheme, which consists of a sequence of",
    " contrast and bright colors.",
    "" 
    ]

main = do
  args <- getArgs
  case args of
    ["--help"] -> showHelp >> exitSuccess
    _          -> return ()
  let (w,h) = (read $ getArg "w" "640" args, read $ getArg "h" "480" args)
  let barHeight = case getArg "bh" "5" args of { "fill" -> BarHeightFill ; bh -> BarHeightFixed $ read bh }
  let tickIntervalMs = read $ getArg "tickInterval" "1000" args
  let largeTickFreq = read $ getArg "largeTickFreq" "10" args
  let timeFormat = getArg "tf" "%Y-%m-%d %H:%M:%OS" args
  let ptime = strptime (B.pack timeFormat)
  let parseTime s = fromMaybe (error $ "Invalid time: " ++ show s) . ptime $ s
  let fromTime = fst `fmap` (strptime timeFormat $ getArg "fromTime" "" args)
  let toTime = fst `fmap` (strptime timeFormat $ getArg "toTime" "" args)
  let forcedNumTracks = case getArg "numTracks" "" args of { "" -> Nothing ; n -> Just $ read n }
  let outPNG = getArg "o" "" args
  let inputFile = getArg "if" "-" args
  let pruneLF b | not (B.null b) && (B.last b == '\r') = B.init b
                | otherwise                            = b
  let cmpTracks = case getArg "sort" "time"  args of { "time" -> comparing utcTime ; "name" -> comparing track }
  let expireTimeMs = read $ getArg "expire" "Infinity" args
  let phantomColor = case getArg "phantom" "" args of { "" -> Nothing; c -> Just (S.pack c) }

  let readInput = if inputFile == "-" then B.getContents else B.readFile inputFile
  let readEvents = (map (parse parseTime . pruneLF) . B.lines) `fmap` readInput
  
  let colorMaps = [(S.pack scheme, map S.pack (words wheel)) | ("-colorscheme":scheme:wheel:_) <- tails args ] 

  pic <- renderEvents (RenderConf barHeight tickIntervalMs largeTickFreq expireTimeMs cmpTracks phantomColor fromTime toTime forcedNumTracks True colorMaps) readEvents
  renderableToPNGFile pic w h outPNG

