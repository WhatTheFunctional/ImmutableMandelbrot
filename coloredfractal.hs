-- coloredfractal.hs
-- Create an image with a fractal, colored using the discrete Escape Time Algorithm:
-- https://en.wikipedia.org/wiki/Mandelbrot_set#Escape_time_algorithm
-- Copyright Laurence Emms 2018
--
-- Special thanks to u/jared--w for feedback and improvements and @geophf for suggesting that I should
-- use the escape time algorithm for coloring the fractal.

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
data Point c = Point c c

-- CIE color conversions
-- https://en.wikipedia.org/wiki/Lab_color_space
data LRGB c = LRGB c c c --Linear RGB
data SRGB c = SRGB c c c --sRGB
data CIEXYZ c = CIEXYZ c c c
data CIELAB c = CIELAB c c c
data Illuminant c = Illuminant c c c

illuminantD50 :: Floating c => Illuminant c
illuminantD50 = Illuminant 96.6797 100.0 82.5188

illuminantD65 :: Floating c => Illuminant c
illuminantD65 = Illuminant 95.047 100.0 108.883

--CIE functions
cieDelta :: Floating c => c
cieDelta = 6.0 / 29.0

cieF :: (Floating c, Ord c) => c -> c
cieF t
    | t > cieDelta * cieDelta * cieDelta = t ** (1.0 / 3.0)
    | otherwise = t / (3.0 * cieDelta * cieDelta) + 4.0 / 29.0

cieFInv :: (Floating c, Ord c) => c -> c
cieFInv t
    | t > cieDelta = t * t * t
    | otherwise = 3 * cieDelta * cieDelta * (t - 4.0 / 29.0)

--Convert from XYZ to LAB color space
xyzToLab :: (Floating c, Ord c) => Illuminant c -> CIEXYZ c -> CIELAB c
xyzToLab (Illuminant xn yn zn) (CIEXYZ x y z) = CIELAB x y z
                          where l = 116.0 * (cieF (y / yn)) - 16.0
                                a = 500.0 * ((cieF (x / xn)) - (cieF (y / yn)))
                                b = 200.0 * ((cieF (y / yn)) - (cieF (z / zn)))

xyzToLabD50 :: (Floating c, Ord c) => CIEXYZ c -> CIELAB c
xyzToLabD50 xyz = xyzToLab illuminantD50 xyz

xyzToLabD65 :: (Floating c, Ord c) => CIEXYZ c -> CIELAB c
xyzToLabD65 xyz = xyzToLab illuminantD65 xyz

--Convert from LAB to XYZ color space
labToXyz :: (Floating c, Ord c) => Illuminant c -> CIELAB c -> CIEXYZ c
labToXyz (Illuminant xn yn zn) (CIELAB l a b) = CIEXYZ x y z
                                                where x = xn * (cieFInv ((l + 16.0) / 116.0) + (a / 500.0))
                                                      y = yn * (cieFInv ((l + 16.0) / 116.0))
                                                      z = zn * (cieFInv ((l + 16.0) / 116.0) - (b / 200.0))

labToXyzD50 :: (Floating c, Ord c) => CIELAB c -> CIEXYZ c
labToXyzD50 lab = labToXyz illuminantD50 lab

labToXyzD65 :: (Floating c, Ord c) => CIELAB c -> CIEXYZ c
labToXyzD65 lab = labToXyz illuminantD65 lab

--sRGB functions
srgbGamma :: Floating c => c
srgbGamma = 2.4

srgbGammaInv :: Floating c => c
srgbGammaInv = 1.0 / 2.4

srgbOffset :: Floating c => c
srgbOffset = 0.055

linearToGammaCorrected :: (Floating c, Ord c) => c -> c
linearToGammaCorrected c
    | c <= 0.0031308 = 12.92 * c
    | otherwise = (1.0 + srgbOffset) * (c ** srgbGammaInv)

gammaCorrectedToLinear :: (Floating c, Ord c) => c -> c
gammaCorrectedToLinear c
    | c <= 0.04045 = c / 12.92
    | otherwise = (((c + srgbOffset) / (1.0 + srgbOffset)) ** srgbGamma) / (1.0 + srgbOffset)

--Convert from XYZ to sRGB
--https://en.wikipedia.org/wiki/SRGB#The_forward_transformation_(CIE_XYZ_to_sRGB)
xyzToLrgb :: (Floating c, Ord c) => CIEXYZ c -> LRGB c
xyzToLrgb (CIEXYZ x y z) = LRGB r g b
                           where r = 3.2406 * x - 1.5372 * y - 0.4986 * z
                                 g = (-0.9689) * x + 1.8758 * y + 0.0415 * z
                                 b = 0.0557 * x - 0.2040 * y + 1.0570 * z
lrgbToSrgb :: (Floating c, Ord c) => LRGB c -> SRGB c
lrgbToSrgb (LRGB r g b) = SRGB sr sg sb
                          where sr = linearToGammaCorrected r
                                sg = linearToGammaCorrected g
                                sb = linearToGammaCorrected b

lrgbToXyz :: (Floating c, Ord c) => LRGB c -> CIEXYZ c
lrgbToXyz (LRGB r g b) = CIEXYZ x y z
                           where x = 0.4124 * r + 0.3576 * g + 0.1805 * b
                                 y = 0.2126 * r + 0.7152 * g + 0.0722 * b
                                 z = 0.0193 * r + 0.1192 * g + 0.9505 * b
srgbToLrgb :: (Floating c, Ord c) => SRGB c -> LRGB c
srgbToLrgb (SRGB sr sg sb) = LRGB r g b
                          where r = gammaCorrectedToLinear sr
                                g = gammaCorrectedToLinear sg
                                b = gammaCorrectedToLinear sb

--Convert from LAB to sRGB
labToSrgb :: (Floating c, Ord c) => CIELAB c -> SRGB c
labToSrgb lab = lrgbToSrgb $ xyzToLrgb $ labToXyzD65 lab

--Convert from sRGB to LAB
srgbToLab :: (Floating c, Ord c) => SRGB c -> CIELAB c
srgbToLab srgb = xyzToLabD65 $ lrgbToXyz $ srgbToLrgb srgb

--Mandelbrot

printFileName :: Maybe String -> IO (Maybe String)
printFileName Nothing = putStrLn "Usage: fractal <output file>" >> return Nothing
printFileName (Just fileName) = putStrLn ("Generating " ++ fileName) >> return (Just fileName)

savePNGFile :: Maybe String -> DynamicImage -> IO ()
savePNGFile Nothing _ = return ()
savePNGFile (Just fileName) image = savePngImage fileName image

genMandelbrot :: (Floating c, Ord c) => Counter -> Counter -> Point c -> Point c -> Int
genMandelbrot (Counter iter) (Counter maxIter) (Point x y) (Point px py)
    | diverges || iter > maxIter = iter
    | otherwise = genMandelbrot (Counter (iter + 1)) (Counter maxIter) (Point updatedX updatedY) (Point px py)
    where
        diverges = x * x + y * y > 4.0
        --Update coordinates based on the definition of the Mandelbrot Set
        updatedX = x * x - y * y + px
        updatedY = 2.0 * x * y + py

--Convert from [0, 1] to radians
percentToRadians :: Floating c => c -> c
percentToRadians percent = percent * 2.0 * pi

mandelbrot :: Counter -> Dimensions -> Int -> Int -> PixelRGB8
mandelbrot (Counter maxIter) (Dimensions (Width w) (Height h)) x y =
    let maxDimension = max w h
        --Coordinates recentered so that the origin is in the middle of the image
        Coordinate rx ry = Coordinate (x - w `div` 2) (y - h `div` 2)
        --Coordinates normalized into the range [-1, 1]
        Point nx ny = Point (fromIntegral rx / fromIntegral maxDimension) (fromIntegral ry / fromIntegral maxDimension)
        pixelValue = genMandelbrot (Counter 0) (Counter maxIter) (Point 0.0 0.0) (Point (nx * 3.0) (ny * 3.0)) --Scale point to the range [-3, 3]
        cieLabAngle = percentToRadians ((fromIntegral pixelValue) / (fromIntegral maxIter) * 0.125 - 0.5) --Percent scaled for pretty colors
        lab = CIELAB 0.0 ((cos cieLabAngle) * 100.0) ((sin cieLabAngle) * 100.0)
        SRGB r g b = labToSrgb lab
    in PixelRGB8 (round (r * 255.0)) (round (g * 255.0)) (round (b * 255.0))

main = let imageWidth = 1920
           imageHeight = 1080
           fractal = mandelbrot (Counter 1000) (Dimensions (Width imageWidth) (Height imageHeight))
       in getArgs >>=
          (\args -> printFileName (headMay args)) >>=
          (\fileName -> return (fileName, (ImageRGB8 (generateImage fractal 1920 1080)))) >>=
          (\(fileName, image) -> savePNGFile fileName image) >>
          exitSuccess
