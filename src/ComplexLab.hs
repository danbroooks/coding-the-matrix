module ComplexLab where

import Data.Complex

-- 1.2 Complex numbers in ~~ Haskell ~~

i :: Num a => a -> Complex a
i n = 0 :+ n

-- Haskell supports complex numbers.
-- The square root of -9, the imaginary number 3i, is written:

squareRootOfNine :: Num a => Complex a
squareRootOfNine =
  i 3 -- Written 3j in python

-- Haskell allows the use of + to add a real number to an imaginary one.
-- We can write the complex solution to `(x − 1)^2 = −9` as:

addRealToImag :: RealFloat a => Complex a
addRealToImag = 
  1 + squareRootOfNine -- 1 + 3j in python

-- In fact, the operators +, -, *, /, and ^^ all work with complex numbers.
-- When you add two complex numbers, the real parts are added and the imaginary parts are added.

supportsOperators :: RealFloat a => Complex a
supportsOperators = 
  (((1 + i 3) / 8) ^^ 2) * i 3
  -- ((((1 + 3j) / 8) ** 2) * 3j) in python

