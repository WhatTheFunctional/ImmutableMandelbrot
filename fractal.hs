-- fractal.hs
-- Create an image with a fractal
-- Copyright Laurence Emms 2018
--
-- Special thanks to u/jared--w for feedback and improvements.

module Main (main) where

import System.Environment
import System.Exit
import Safe
import Codec.Picture

newtype Counter = Counter Int
newtype Height = Height Int
newtype Width = Width Int

data Dimensions = Dimensions Width Height
data Coordinate = Coordinate Int Int
data Point = Point Float Float

printFileName :: Maybe String -> IO (Maybe String)
printFileName Nothing = putStrLn "Usage: fractal <output file>" >> return Nothing
printFileName (Just fileName) = putStrLn ("Generating " ++ fileName) >> return (Just fileName)

savePNGFile :: Maybe String -> DynamicImage -> IO ()
savePNGFile Nothing _ = return ()
savePNGFile (Just fileName) image = savePngImage fileName image

genMandelbrot :: Counter -> Counter -> Point -> Point -> Int
genMandelbrot (Counter iter) (Counter maxIter) (Point x y) (Point px py)
    | diverges || iter > maxIter = iter
    | otherwise = genMandelbrot (Counter (iter + 1)) (Counter maxIter) (Point updatedX updatedY) (Point px py)
    where
        diverges = x * x + y * y > 4.0
        --Update coordinates based on the definition of the Mandelbrot Set
        updatedX = x * x - y * y + px
        updatedY = 2.0 * x * y + py

mandelbrot :: Counter -> Dimensions -> Int -> Int -> PixelRGB8
mandelbrot (Counter maxIter) (Dimensions (Width w) (Height h)) x y =
    let maxDimension = max w h
        --Coordinates recentered so that the origin is in the middle of the image
        Coordinate rx ry = Coordinate (x - w `div` 2) (y - h `div` 2)
        --Coordinates normalized into the range [-1, 1]
        Point nx ny = Point (fromIntegral rx / fromIntegral maxDimension) (fromIntegral ry / fromIntegral maxDimension)
        pixelValue = genMandelbrot (Counter 0) (Counter maxIter) (Point 0.0 0.0) (Point (nx * 3.0) (ny * 3.0)) --Scale point to the range [-3, 3]
    in PixelRGB8 0 0 (fromIntegral (min pixelValue 255))

main = let imageWidth = 1920
           imageHeight = 1080
           fractal = mandelbrot (Counter 1000) (Dimensions (Width imageWidth) (Height imageHeight))
       in getArgs >>=
          (\args -> printFileName (headMay args)) >>=
          (\fileName -> return (fileName, (ImageRGB8 (generateImage fractal 1920 1080)))) >>=
          (\(fileName, image) -> savePNGFile fileName image) >>
          exitSuccess
