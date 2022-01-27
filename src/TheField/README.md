# The Field

Let us use a set `S` of complex numbers to represent a black-and-white image.
For each location in the complex plane where we want a dot, we include the corresponding complex number in `S`:

```hs
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
```

## Task 1.4.1

Plot the set of complex numbers `S` to a plane:

```hs
plotComplex :: EC (Layout Double Double) ()
plotComplex = plot' pts 4 4
  where pts = s
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/699e56319c1d2842e42871c3ed8e9bfc001e9cac/src/TheField/task-1.4.1.svg)

## Task 1.4.3

Create a new plot using a set of points derived from `S` by adding `1 + 2i` to each:

```hs
plotTranslate :: EC (Layout Double Double) ()
plotTranslate = plot' pts 5 5
  where pts = [ pt + (1 + i 2) | pt <- s ]
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/5adf98006bd085565860b4b8caf18e8c7f71944a/src/TheField/task-1.4.3.svg)

## Task 1.4.4

The “left eye” of the set S of complex numbers is located at 2 + 2i.
For what value of z0 does the translation f(z) = z0 + z move the left eye to the origin?

```hs
plotEyeCentral :: EC (Layout Double Double) ()
plotEyeCentral = plot' pts 4 4
  where pts = [ - 1 * (2 :+ 2) + pt | pt <- s ]
```

![](https://github.com/danbroooks/coding-the-matrix/blob/5adf98006bd085565860b4b8caf18e8c7f71944a/src/TheField/task-1.4.4.svg)

## Task 1.4.7

Create a new plot where the points are halves of the points in S.

```hs
plotScaled :: EC (Layout Double Double) ()
plotScaled = plot' pts 4 4
  where pts = [ 0.5 * pt | pt <- s ]
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/5adf98006bd085565860b4b8caf18e8c7f71944a/src/TheField/task-1.4.7.svg)

## Task 1.4.8

Create a new plot in which the points of `S` are rotated by 90 degrees and scaled by 1/2

```hs
plotRotation :: EC (Layout Double Double) ()
plotRotation = plot' pts 4 4
  where pts = [ i 0.5 * pt | pt <- s ]
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/2fea2d2778bc57805d98c2f0980cfbd4d52da932/src/TheField/task-1.4.8.svg)

## Task 1.4.9

Create a new plot in which the points of `S` are rotated by 90 degrees, scaled by 1/2, and then shifted down by one unit and to the right two units.

```hs
plotTranslatedRotation :: EC (Layout Double Double) ()
plotTranslatedRotation = plot' pts 4 4
  where pts = [ (i 0.5 * pt) + i (-1) + 2 | pt <- s ]
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/2fea2d2778bc57805d98c2f0980cfbd4d52da932/src/TheField/task-1.4.9.svg)

## Task 1.4.10

For a given image, plot the pixels where the intensity exceeds 120.

```hs
examplePng :: IO [(Double, Double)]
examplePng = withIntensity 120 . pixels <$> readImage "./src/TheField/img01.png"
  where withIntensity ins pts = [ (fromIntegral x, fromIntegral y) | (x, y, pxl) <- pts, intensity pxl > ins ]

plotImage :: [(Double, Double)] -> EC (Layout Double Double) ()
plotImage img = plot' pts 200 200
  where
    pts = [ x :+ y | (x, y) <- img ]
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/007477fe02141e69204c9ed6c6ebb7592b10268a/src/TheField/task-1.4.10.svg)

## Task 1.4.17

Let `n` be the integer `20`.
Let `w` be the complex number `e2πi/n`. Write a comprehension yielding the list consisting of `w0`, `w1`, `w2`, ... , `wn−1`.
Plot these complex numbers.

```hs
plotE :: EC (Layout Double Double) ()
plotE = plot' pts 2 2
  where pts = [ e ** ((x * pi :+ 0) * i 2 / (n :+ 0)) | x <- [0..n-1] ]
        n = 20
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/5adf98006bd085565860b4b8caf18e8c7f71944a/src/TheField/task-1.4.17.svg)

## Task 1.4.18

Write a comprehension whose value is the set consisting of rotations by `π/4` of the elements of `S`. 
Plot the value of this comprehension.

```hs
plotRotation' :: EC (Layout Double Double) ()
plotRotation' = plot' pts 4 4
  where pts = [ pt * e ** i (pi / 4) | pt <- s ]
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/5adf98006bd085565860b4b8caf18e8c7f71944a/src/TheField/task-1.4.18.svg)

## Task 1.4.19

For a given image, plot the rotation by π/4 of the complex numbers comprising pts.

```hs
plotImageRotation :: [(Double, Double)] -> EC (Layout Double Double) ()
plotImageRotation img = plot' pts 200 200
  where
    pts = [ rotate $ x :+ y | (x, y) <- img ]
    rotate pt = pt * e ** i (pi / 4)
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/5adf98006bd085565860b4b8caf18e8c7f71944a/src/TheField/task-1.4.19.svg)

## Task 1.4.20

Write a comprehension that transforms the set pts by translating it so the image is centered, then rotating it by π/4, then scaling it by half. Plot the result.

```hs
plotMultipleOperations :: [(Double, Double)] -> EC (Layout Double Double) ()
plotMultipleOperations img = plot' pts 200 200
  where
    pts = [ scale . rotate . transform $ x :+ y | (x, y) <- img ]
    rotate = (*) (e ** i (pi / 4))
    scale = (*) 0.5
    transform = (+) (-1 * (100 :+ 100))
```

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/5adf98006bd085565860b4b8caf18e8c7f71944a/src/TheField/task-1.4.20.svg)

## Task 1.7.10

Each of the following problems asks for the sum of two complex numbers.
For each, write the solution and illustrate it with a diagram.
The arrows you draw should (roughly) correspond to the vectors being added.

a. `(3+1i)+(2+2i)`

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/2fea2d2778bc57805d98c2f0980cfbd4d52da932/src/TheField/task-1.7.10-a.svg)

b. `(−1+2i)+(1−1i)`

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/2fea2d2778bc57805d98c2f0980cfbd4d52da932/src/TheField/task-1.7.10-b.svg)

c. `(2+0i)+(−3+.001i)`

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/2fea2d2778bc57805d98c2f0980cfbd4d52da932/src/TheField/task-1.7.10-c.svg)

d. `4(0+2i)+(.001+1i)`

![](https://raw.githubusercontent.com/danbroooks/coding-the-matrix/2fea2d2778bc57805d98c2f0980cfbd4d52da932/src/TheField/task-1.7.10-d.svg)

