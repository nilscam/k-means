module Main where

import System.Exit
import System.Environment
import Control.Monad
import System.IO
import System.Random
import Data.List
import Lib
import Debug.Trace

tabtest :: [Pixel]
tabtest = [Pixel (Pos2 0 1) (Pos3 23 54 75), Pixel (Pos2 1 1) (Pos3 54 244 123), Pixel (Pos2 3 3) (Pos3 123 54 65)]

data Pos2 = Pos2 {
  x :: Int,
  y :: Int
  }

data Pos3 = Pos3 {
  px :: Float,
  py :: Float,
  pz :: Float
  }

data Pixel = Pixel {
  pos :: Pos2,
  color :: Pos3
  }

data Centroid = Centroid {
  id_c :: Int,
  cpos :: Pos3
  }

data Cluster = Cluster {
  centroid :: Centroid,
  pointslist :: [Pixel]
  }

data Generation = Generation {
  clusters :: [Cluster]
  }

show' :: Show a => [a] -> String
show' = intercalate "" . map show

instance Show (Pos2) where
  show pos = "(" ++ show (x pos) ++ "," ++ show (y pos) ++ ")"

instance Show (Pos3) where
  show pos = "(" ++ show (round (px pos)) ++ "," ++ show (round (py pos)) ++ "," ++ show (round (pz pos)) ++ ")"

instance Show (Pixel) where
  show pixel = show (pos pixel) ++ " " ++ show (color pixel) ++ "\n"

instance Show (Centroid) where
  show centroid = show (cpos centroid)

instance Show (Cluster) where
  show cluster = "--\n" ++ show (centroid cluster) ++ "\n-\n" ++ show' (pointslist cluster)

instance Show (Generation) where
  show gen = show' (clusters gen)

usage = do
  putStrLn "USAGE: ./imageCompressor n e IN\n"
  putStrLn "\tn\tnumber of colors in the final image"
  putStrLn "\te\tconvergence limit"
  putStrLn "\tIN\tpath to the file containing the colors of the pixels"
  exit 84

main :: IO ()
main = do
  args <- getArgs
  if (length args) == 3
    then parseArgs args
    else usage

parseArgs :: [String] -> IO ()
parseArgs args = do
  let k = read (args !! 0) :: Int
  let e = read (args !! 1) :: Float

  pixelMap <- parseFile (args !! 2)
  compress pixelMap k e

compress :: [Pixel] -> Int -> Float -> IO ()
compress listPixels nbCentroid convergence = do
  let gen1 = firstGeneration nbCentroid listPixels
  let fulledCentroidExpected = (length listPixels) - ((length listPixels) - nbCentroid)
  kmean listPixels gen1 nbCentroid convergence fulledCentroidExpected

kmean :: [Pixel] -> Generation -> Int -> Float -> Int -> IO ()
kmean listPixels prevgen nbCentroid convergence fulledCentroidExpected = do

  randNbs <-randInts nbCentroid
  let newgen = makeNewGen prevgen listPixels

  if fulledCentroidExpected > (nbFulledCentroid (clusters newgen))
    then kmean listPixels (replaceLostCentroid newgen randNbs) nbCentroid convergence fulledCentroidExpected
    else do
    let newGenUpdate = computeCentroids newgen nbCentroid
    let maxMoveDistance = maxCentroidsMoveDistance prevgen newGenUpdate
    if (maxMoveDistance <= convergence)
      then putStr . show $ newGenUpdate
      else kmean listPixels newGenUpdate nbCentroid convergence fulledCentroidExpected

dupCentroidList :: Generation -> Generation
dupCentroidList origin = Generation [(Cluster (centroid x) []) | x <- (clusters origin)]

nbFulledCentroid :: [Cluster] -> Int
nbFulledCentroid [] = 0
nbFulledCentroid [x] = if (length (pointslist x)) > 0 then 1 else 0
nbFulledCentroid (x:xs) = (nbFulledCentroid [x]) + (nbFulledCentroid xs)

replaceLostCentroid :: Generation -> [Int] -> Generation
replaceLostCentroid gen seed = Generation (map(\x -> Cluster (if (length (pointslist x)) > 0 then (centroid x) else (genRandCentroid (id_c (centroid x)) (seed !! (id_c (centroid x))))) (pointslist x)) (clusters gen))

-- compare 2 generation and return biggest cluster move
minCluster :: [Cluster] -> [Cluster] -> Float
minCluster [] [] = 99999
minCluster [x] [y] = calcDistC (centroid x) (centroid y)
minCluster (x:xs) (y:ys) = min (calcDistC (centroid x) (centroid y)) (minCluster xs ys)

maxCentroidsMoveDistance :: Generation -> Generation -> Float
maxCentroidsMoveDistance old new = minCluster (clusters old) (clusters new)

calcDistC :: Centroid -> Centroid -> Float
calcDistC c1 c2 = sqrt(((px (cpos c1)) - (px (cpos c2))) ^ 2 + ((py (cpos c1)) - (py (cpos c2))) ^ 2 + ((pz (cpos c1)) - (pz (cpos c2))) ^ 2)

calcDistance :: Pixel -> Centroid -> Float
calcDistance pixel centroid = sqrt(((px (color pixel)) - (px (cpos centroid))) ^ 2 + ((py (color pixel)) - (py (cpos centroid))) ^ 2 + ((pz (color pixel)) - (pz (cpos centroid))) ^ 2)

--return the id of the closest Centroid
getClosest :: Pixel -> [Cluster] -> (Int, Float)
getClosest pixel [x] = ((id_c (centroid x)), calcDistance pixel (centroid x))
getClosest pixel (x:xs)
  | (snd result) < (snd result2) = result
  | otherwise = result2
  where
    result = (getClosest pixel xs)
    result2 = (getClosest pixel [x])

makeNewGen :: Generation -> [Pixel] -> Generation
makeNewGen oldgen listPixels = Generation (map (\x -> Cluster (centroid x) [ y | y <- listPixels, (id_c (centroid x)) == (fst (getClosest y (clusters oldgen)))]) (clusters dupCentroid))
  where dupCentroid = (dupCentroidList oldgen)

genNewCentroid :: [Pixel] -> Centroid -> Centroid
genNewCentroid [] old = old
genNewCentroid pixels old = Centroid (id_c old) (Pos3 r g b)
  where r = (sum (map(\(Pixel (Pos2 _ _) (Pos3 r _ _)) -> r) pixels)) / (fromIntegral len)
        g = (sum (map(\(Pixel (Pos2 _ _) (Pos3 _ g _)) -> g) pixels)) / (fromIntegral len)
        b = (sum (map(\(Pixel (Pos2 _ _) (Pos3 _ _ b)) -> b) pixels)) / (fromIntegral len)
        len = (length pixels)

computeCentroids :: Generation -> Int -> Generation
computeCentroids newgen nbCentroid = Generation (map(\x -> Cluster ((genNewCentroid (pointslist x) (centroid x))) (pointslist x)) (clusters newgen))

firstGeneration :: Int -> [Pixel] -> Generation
firstGeneration nbCentroid listPixels = Generation [(Cluster (genRandCentroid x x) []) | x <- [1..nbCentroid]]

genRandCentroid :: Int -> Int -> Centroid
genRandCentroid id seed = Centroid id (randColor (mkStdGen seed))

randInts :: Int -> IO [Int]
randInts nb = do
  gen <- newStdGen
  let ns = randoms gen :: [Int]
  return (take nb ns)

randColor :: StdGen -> Pos3
randColor s0 = Pos3 x y z
  where
    (x, s1) = r s0
    (y, s2) = r s1
    (z, s3) = r s2
    r = randomR (0, 255)

exit code = exitWith (ExitFailure code)









replace str a b = map (\c -> if c == a then b else c) str

makePosFromString str = Pos2 (read ((words new) !! 0)) (read ((words new) !! 1))
  where new = replace str ',' ' '

makeColorFromString str = Pos3 (read ((words new) !! 0)) (read ((words new) !! 1)) (read ((words new) !! 2))
  where new = replace str ',' ' '

extractColorList list = map (\elem -> makeColorFromString ((words elem) !! 1)) list

extractPosList list = map (\elem -> makePosFromString ((words elem) !! 0)) list

epureRawContent rawContent = lines (filter (\c -> c /= '(' && c /= ')') rawContent)

makePixelList positions colors = [ (Pixel pos color) | (pos, color) <- zip positions colors ]

parseFileContent rawContent = makePixelList (extractPosList content) (extractColorList content)
  where content = epureRawContent rawContent

parseFile :: String -> IO [Pixel]
parseFile filename = do
  file <- openFile (filename) ReadMode
  content <- hGetContents file
  let pixelMap = parseFileContent content
  return pixelMap

