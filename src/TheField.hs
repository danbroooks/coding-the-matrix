{-# LANGUAGE FlexibleContexts #-}

module TheField where

import           Data.Complex
import           Data.Char
import           Data.List as L
import           Graphics.Rendering.Chart.Easy hiding(transform)
import           Graphics.Rendering.Chart.Backend.Cairo
--
import           TheField.Image
import           TheField.Plot

i :: Num a => a -> Complex a
i n = 0 :+ n

e :: Floating a => Complex a
e = exp 1 :+ 0

s :: [Complex Double]
s = [ 2 + i 2
    , 3 + i 2
    , 1.75 + i 1
    , 2 + i 1
    , 2.25 + i 1
    , 2.5 + i 1
    , 2.75 + i 1
    , 3 + i 1
    , 3.25 + i 1
    ]

-- Task 1.4.1
plotComplex :: EC (Layout Double Double) ()
plotComplex = plot' pts 4 4
  where pts = s

-- Task 1.4.3
plotTranslate :: EC (Layout Double Double) () 
plotTranslate = plot' pts 5 5
  where pts = [ pt + (1 + i 2) | pt <- s ]

-- Task 1.4.4
plotEyeCentral :: EC (Layout Double Double) ()
plotEyeCentral = plot' pts 4 4
  where pts = [ - 1 * (2 :+ 2) + pt | pt <- s ]

-- Task 1.4.7
plotScaled :: EC (Layout Double Double) () 
plotScaled = plot' pts 4 4
  where pts = [ 0.5 * pt | pt <- s ]

-- Task 1.4.8
plotRotation :: EC (Layout Double Double) ()
plotRotation = plot' pts 4 4
  where pts = [ i 0.5 * pt | pt <- s ]

-- Task 1.4.9
plotTranslatedRotation :: EC (Layout Double Double) ()
plotTranslatedRotation = plot' pts 4 4
  where pts = [ (i 0.5 * pt) + i (-1) + 2 | pt <- s ]

-- Task 1.4.10
plotImage :: [(Double, Double)] -> EC (Layout Double Double) ()
plotImage img = plot' pts 200 200
  where pts = [ x :+ y | (x, y) <- img ]

examplePng :: IO [(Double, Double)]
examplePng = withIntensity 120 . pixels <$> readImage "./src/TheField/img01.png"
  where
    withIntensity ins pts =
      [ (fromIntegral x, fromIntegral y) | (x, y, pxl) <- pts, intensity pxl > ins ]

-- Task 1.4.17
plotE :: EC (Layout Double Double) ()
plotE = plot' pts 2 2
  where pts = [ e ** ((x * pi :+ 0) * i 2 / (n :+ 0)) | x <- [0..n-1] ]
        n = 20

-- Task 1.4.18
plotRotation' :: EC (Layout Double Double) ()
plotRotation' = plot' pts 4 4
  where pts = [ pt * e ** i (pi / 4) | pt <- s ]

-- Task 1.4.19
plotImageRotation :: [(Double, Double)] -> EC (Layout Double Double) ()
plotImageRotation img = plot' pts 200 200
  where
    pts = [ rotate $ x :+ y | (x, y) <- img ]
    rotate pt = pt * e ** i (pi / 4)

-- Task 1.4.20
plotMultipleOperations :: [(Double, Double)] -> EC (Layout Double Double) ()
plotMultipleOperations img = plot' pts 200 200
  where
    pts = [ scale . rotate . transform $ x :+ y | (x, y) <- img ]
    rotate = (*) (e ** i (pi / 4))
    scale = (*) 0.5
    transform = (+) (-1 * (100 :+ 100))

-- Task 1.5.1
message :: [String]
message =
  [ "10101"
  , "00100"
  , "10101"
  , "01011"
  , "11001"
  , "00011"
  , "01011"
  , "10101"
  , "00100"
  , "11001"
  , "11010"
  ]

decryptMessage :: String
decryptMessage = decryptCyphertext [1,0,0,0,1] message

decryptCyphertext :: [Int] -> [String] -> String
decryptCyphertext key = L.foldr ((:) . decode) ""
  where
    decode = decodeChar . decodeNumber . L.map (uncurry decryptBit) . zip key . L.map toBit
    toBit b = if b == '1' then 1 else 0

-- try all possible keys in attempt to find the answer
brute :: [([Int], String)]
brute = L.map (\k -> (k, decryptCyphertext k message)) keys
  where
    keys = [ [a,b,c,d,e] | a <- [0..1], b <- [0..1], c <- [0..1], d <- [0..1], e <- [0..1] ]

decryptBit :: Int -> Int -> Int
decryptBit k v = (k + v) `mod` 2

decodeNumber :: [Int] -> Int
decodeNumber str = L.foldr ((+) . uncurry decode) 0 $ zip [0..] (reverse str)
  where decode idx val = val * 2 ^ idx

decodeChar :: Int -> Char
decodeChar n = 
  case n' of
    26 -> ' '
    _ -> chr $ n' + 65
  where n' = n `mod` 27

-- Task 1.7.10
sumComplexNumbersA :: EC (Layout Double Double) ()
sumComplexNumbersA = plotComplexAddition (3.0 :+ 1.0) (2.0 :+ 2.0)

sumComplexNumbersB :: EC (Layout Double Double) ()
sumComplexNumbersB = plotComplexAddition ((-1.0) :+ 2.0) (1.0 :+ (-1.0))

sumComplexNumbersC :: EC (Layout Double Double) ()
sumComplexNumbersC = plotComplexAddition (2.0 :+ 0.0) ((-3.0) :+ 0.001)

sumComplexNumbersD :: EC (Layout Double Double) ()
sumComplexNumbersD = plotComplexAddition (4 * (0.0 :+ 2.0)) (0.001 :+ 1.0)

generateImages :: IO ()
generateImages = do
  toFile def "./src/TheField/task-1.4.1.png" plotComplex
  toFile def "./src/TheField/task-1.4.3.png" plotTranslate
  toFile def "./src/TheField/task-1.4.4.png" plotEyeCentral
  toFile def "./src/TheField/task-1.4.7.png" plotScaled
  toFile def "./src/TheField/task-1.4.8.png" plotRotation
  toFile def "./src/TheField/task-1.4.9.png" plotTranslatedRotation
  toFile def "./src/TheField/task-1.4.10.png" =<< plotImage <$> examplePng
  toFile def "./src/TheField/task-1.4.17.png" plotE
  toFile def "./src/TheField/task-1.4.18.png" plotRotation'
  toFile def "./src/TheField/task-1.4.19.png" =<< plotImageRotation <$> examplePng
  toFile def "./src/TheField/task-1.4.20.png" =<< plotMultipleOperations <$> examplePng
  toFile def "./src/TheField/task-1.7.10-a.png" sumComplexNumbersA
  toFile def "./src/TheField/task-1.7.10-b.png" sumComplexNumbersB
  toFile def "./src/TheField/task-1.7.10-c.png" sumComplexNumbersC
  toFile def "./src/TheField/task-1.7.10-d.png" sumComplexNumbersD
