module Main where

import System.Exit
import System.Environment
import Control.Monad
import System.IO
import System.Random
import Lib

tabtest :: [Pixel]
tabtest = [Pixel (Pos2 0 1) (Pos3 23 54 75), Pixel (Pos2 1 1) (Pos3 54 244 123), Pixel (Pos2 3 3) (Pos3 123 54 65)]

data Pos2 = Pos2 { x :: Int,
                   y :: Int
                 } deriving (Show)

data Pos3 = Pos3 {
  px :: Float,
  py :: Float,
  pz :: Float
  } deriving (Show)

data Pixel = Pixel {
  pos :: Pos2,
  color :: Pos3
  } deriving (Show)

data Centroid = Centroid {
  id_c :: Int,
  cpos :: Pos3
  } deriving (Show)

data Cluster = Cluster {
  centroid :: Centroid,
  pointslist :: [Pixel]
  } deriving (Show)

data Generation = Generation {
  clusters :: [Cluster]
  } deriving (Show)

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
    then compress tabtest 4 0.8
    --then print (firstGeneration 4 tabtest)
    else usage

compress :: [Pixel] -> Int -> Float -> IO ()
compress listPixels nbCentroid convergence = do
  let gen1 = firstGeneration nbCentroid listPixels
  kmean listPixels gen1 nbCentroid convergence

kmean :: [Pixel] -> Generation -> Int -> Float -> IO ()
kmean listPixels prevgen nbCentroid convergence = do

  -- recompute pixels associations with centroids
  let newgen = makeNewGen prevgen listPixels
  let newGenUpdate = computeCentroids newgen nbCentroid
  let maxMoveDistance = maxCentroidsMoveDistance prevgen newGenUpdate

  print maxMoveDistance

  if (maxMoveDistance <= convergence)
    --then printCompressedImage newgen
    then putStrLn "Success"
    else kmean listPixels newGenUpdate nbCentroid convergence

dupCentroidList :: Generation -> Generation
dupCentroidList origin = Generation [(Cluster (centroid x) []) | x <- (clusters origin)]

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
firstGeneration nbCentroid listPixels = Generation [(Cluster genRandCentroid []) | x <- [1..nbCentroid]]

genRandCentroid :: Centroid
genRandCentroid = Centroid 5 (randColor (mkStdGen 42))

randColor :: StdGen -> Pos3
randColor s0 = Pos3 x y z
  where
    (x, s1) = r s0
    (y, s2) = r s1
    (z, s3) = r s2
    r = randomR (0, 255)

exit code = exitWith (ExitFailure code)

-- return an array of array of int
--makePixelMap :: String -> [[Int]]
--makePixelMap filecontent = 
