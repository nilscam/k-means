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
  print gen1
  kmean listPixels gen1 nbCentroid convergence


kmean :: [Pixel] -> Generation -> Int -> Float -> IO ()
kmean listPixels prevgen nbCentroid convergence = do

  -- recompute pixels associations with centroids
  let newgen = makeNewGen prevgen listPixels

  print newgen

  {-
  -- recompute centroids positions
  let newGenUpdate = computeCentroids newgen nbCentroid

  let maxMoveDistance = maxCentroidsMoveDistance prevgen newgen

  if (maxMoveDistance <= convergence)
    then printCompressedImage newgen
    else kmean listPixels newgen nbCentroid convergence
-}

dupCentroidList :: Generation -> Generation
dupCentroidList origin = Generation [(Cluster (centroid x) []) | x <- (clusters origin)]

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
