module Main where

import System.Environment (getArgs)
import System.Exit

import Data.Time
import Data.Time.Parse

import Graphics.Rendering.Chart.Gtk(renderableToWindow)
import Graphics.Rendering.Chart.Renderable(renderableToPNGFile)

import Data.Maybe(fromMaybe,isNothing)
import Data.Ord(comparing)

import Control.Monad.RWS as RWS

import qualified Data.ByteString.Lazy.Char8 as B

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
    "             [-stream true]",
    "  -if INFILE    - filename from where to read the trace.",
    "                  If omitted or '-', read from stdin.",
    "  -o PNGFILE    - filename to which the output will be written in PNG format.",
    "                  If omitted, it will be shown in a window.",
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
    "  -stream true  - use 'streaming mode' where the input is not loaded into memory and you",
    "                  can process multi-gigabyte inputs.",
    "                  In this mode, you MUST use an actual filename in '-if'.",
    "                  Note that you better also indicate -fromTime, -toTime and -numTracks,",
    "                  otherwise the data will be re-scanned once per each of these properties",
    "                  that is not indicated.",
    "",
    "Input is read from stdin. Example input (speaks for itself):",
    "2010-10-21 16:45:09,431 >foo green",
    "2010-10-21 16:45:09,541 >bar green",
    "2010-10-21 16:45:10,631 >foo yellow",
    "2010-10-21 16:45:10,725 >foo #ff0000",
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
    " case splot will generate a new color for each different token"
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
  let cmpTracks = case getArg "sort" "time"  args of { "time" -> comparing time ; "name" -> comparing track }
  let expireTimeMs = read $ getArg "expire" "Infinity" args
  let phantomColor = case getArg "phantom" "" args of { "" -> Nothing; c -> Just c }
  let streaming = case getArg "stream" "false" args of { "true" -> True; _ -> False }

  let readInput = if inputFile == "-" then B.getContents else B.readFile inputFile
  let readEvents = (map (parse parseTime . pruneLF) . B.lines) `fmap` readInput
  
  when (streaming && (inputFile == "-"))  $ error "In streaming mode (-stream true) you MUST use an actual filename in '-if'"
  when (streaming && isNothing fromTime)  $ putStrLn "Warning: without -fromTime, input will be re-scanned to compute it."
  when (streaming && isNothing toTime)    $ putStrLn "Warning: without -toTime, input will be re-scanned to compute it."
  when (streaming && isNothing forcedNumTracks) $ putStrLn "Warning: without -numTracks, input will be re-scanned to compute it."

  pic <- renderEvents (RenderConf barHeight tickIntervalMs largeTickFreq expireTimeMs cmpTracks phantomColor fromTime toTime forcedNumTracks streaming) readEvents
  case outPNG of
    "" -> renderableToWindow pic w h
    f  -> const () `fmap` renderableToPNGFile pic w h outPNG

