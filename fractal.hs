-- fractal.hs
-- Create an image with a fractal
-- Copyright Laurence Emms 2018

module Main (main) where

import System.Environment
import System.Exit
import Safe
import Codec.Picture

printFileName :: Maybe String -> IO (Maybe String)
printFileName Nothing = (putStrLn "Usage: fractal <output file>") >> (return Nothing)
printFileName (Just fileName) = (putStrLn ("Generating " ++ fileName)) >> (return (Just fileName))

saveImageFile :: Maybe String -> DynamicImage -> IO ()
saveImageFile Nothing _ = return ()
saveImageFile (Just fileName) image = savePngImage fileName image

genMandelbrot :: Int -> Int -> Float -> Float -> Float -> Float -> Int
genMandelbrot iter maxIter x y px py = if (iter > maxIter) || (x * x + y * y > 4.0) then iter else (genMandelbrot (iter + 1) maxIter (x * x - y * y + px) (2.0 * x * y + py) px py)

mandelbrot :: Int -> Int -> Int -> Int -> Int -> PixelRGB8
mandelbrot maxIter w h x y = let p = min (genMandelbrot 0 maxIter 0.0 0.0 ((fromIntegral (x - w `div` 2)) / (fromIntegral (max w h)) * 3.0) ((fromIntegral (y - h `div` 2)) / (fromIntegral (max w h)) * 3.0)) 255
                             in PixelRGB8 0 0 (fromIntegral p)

main = getArgs >>=
       (\args -> (printFileName (headMay args))) >>=
       (\fileName -> return (fileName, (ImageRGB8 (generateImage (mandelbrot 1000 1920 1080) 1920 1080)))) >>=
       (\(fileName, image) -> saveImageFile fileName image) >>
       exitSuccess
