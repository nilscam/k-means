module Main where

-- imports
import System.Environment
import Control.Monad
import System.Exit
import System.IO
import Lib

displayGroup tab width index = do
  when ((index `mod` width) == 0) $ print ((tab !! index) ++ " " ++ (tab !! (index + 1)))
  displayGroupTab tab width (index + 1)

displayGroupTab tab width index = do
  if (tab !! index) /= ""
    then displayGroup tab width index
    else print "\n"

displayFile name = do
  file <- openFile name ReadMode
  text <- hGetContents file
  let tab = words text
  displayGroupTab tab 2 0

-- getFileContent : return a string of the file content
getFileContent name = do
  file <- openFile name ReadMode
  text <- hGetContents file
  return text

-- ------- --
-- PARSING --
-- ------- --

--parseFileContent : ???
parseFileContent content = makeAdvPixelList (makePixelList (extractPosList content) (extractColorList content))

makePixelList positions colors = [ (Int pos color) | (pos, color) <- zip positions colors ]

-- makeAdvPixelList : create and initialize array of pixels
makeAdvPixelList pixels = [ (AdvPixel pixel 0 0) | pixel <- pixels ]

-- compress : ???
compress :: [String] -> IO ()
compress args = do
  print args
  let k = getNbInt (args !! 0)
  let e = getNbFloat (args !! 1)
  content <- getFileContent (args !! 2)
  let advPixel <- parseFileContent content
  exitWith ExitSuccess

-- main : start program
main :: IO ()
main = do
  io_args <- getArgs
  let args = io_args :: [String]
  if (length args /= 3)
    then usage
    else compress args
  exit 0

-- calcul the distance between two colors
-- d = sqrt(pow((r1 - r2), 2) + pow((g1 - g2), 2) + pow((b1 - b2), 2))
--       ______________________________________
-- d = \/ (r1 - r2)2 + (g1 - g2)2 + (b1 - b2)2
--
-- where :
-- (r1, g1, b1) = reg, green, blue of the first point
-- (r2, g2, b2) = reg, green, blue of the second point

-- mediane
-- quartile etc in order to avoid random
