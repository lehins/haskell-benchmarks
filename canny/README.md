# Canny benchmarks

## Setup

Dependencies for `accelerate-llvm-native`

```
$ sudo apt-get install llvm-8-dev
$ sudo apt-get install libffi-dev
```

_Warning_ - `libffi-dev` didn't work for me and I had install one from source, your
mileage might vary.

_Note_ - No GPU benchmarks here


## Intro


Here are the functions available from `Canny` module that you can use to get the results
for each individual library:

* `applyMassivCanny`
* `applyRepaCanny`
* `applyAccelerateCanny`
* `applyYarrCanny`
* `applyFridayCanny`

`massiv`, `repa` and `accelrate` produce nearly identical results, while others differ
sligtly. Some examples will follow below.

`friday` is the only one that doesn't do any automatic parallelization, so results are
naturally much slower

## Small size - Lena

### Reproduce results

Results can be reproduced with:

```haskell
λ> :set -XDataKinds
λ> :set -XScopedTypeVariables
λ> import Canny
λ> import Data.Massiv.Array.IO
λ> lena :: Image S (SRGB 'NonLinear) Word8 <- readImageAuto "files/lena.bmp"
λ> size lena
Sz (256 :. 256)
λ> cannyLena <- applyMassivCanny 50 100 lena
λ> writeImage "files/cannyLena.png" cannyLena
```

`displayImage cannyLena` can useful to display an image from ghci instead of writing it
to a file.


Original Lena

![lena](files/lena.bmp)

After Canny edge detection:

![lenaCanny](files/cannyLena.png)

### Reproduce benchmarks

```shell
$ stack bench :bench-canny --ba '--match prefix Small
```

These are the results running canny algorithm on Lena image.

```
benchmarking Small/massiv
time                 2.774 ms   (2.684 ms .. 2.861 ms)
                     0.992 R²   (0.989 R² .. 0.995 R²)
mean                 2.789 ms   (2.723 ms .. 2.841 ms)
std dev              181.7 μs   (148.5 μs .. 242.1 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking Small/repa
time                 5.075 ms   (4.962 ms .. 5.171 ms)
                     0.993 R²   (0.986 R² .. 0.997 R²)
mean                 5.128 ms   (5.020 ms .. 5.242 ms)
std dev              362.2 μs   (262.1 μs .. 540.5 μs)
variance introduced by outliers: 45% (moderately inflated)

benchmarking Small/accelerate
time                 23.62 ms   (22.89 ms .. 24.30 ms)
                     0.993 R²   (0.981 R² .. 0.998 R²)
mean                 23.10 ms   (22.60 ms .. 23.83 ms)
std dev              1.346 ms   (935.0 μs .. 2.021 ms)
variance introduced by outliers: 24% (moderately inflated)

benchmarking Small/yarr
time                 11.52 ms   (11.07 ms .. 12.10 ms)
                     0.991 R²   (0.982 R² .. 0.997 R²)
mean                 12.30 ms   (11.96 ms .. 12.96 ms)
std dev              1.107 ms   (508.0 μs .. 1.864 ms)
variance introduced by outliers: 48% (moderately inflated)

benchmarking Small/friday
time                 33.79 ms   (33.42 ms .. 34.59 ms)
                     0.999 R²   (0.996 R² .. 1.000 R²)
mean                 33.78 ms   (33.50 ms .. 34.28 ms)
std dev              695.5 μs   (397.7 μs .. 1.034 ms)
```

## Medium size - Frog

### Reproduce results

Results can be replicated with:

```haskell
λ> :set -XDataKinds
λ> :set -XScopedTypeVariables
λ> import Canny
λ> import Data.Massiv.Array.IO
λ> frog :: Image S (SRGB 'NonLinear) Word8 <- readImageAuto "files/frog.jpg"
λ> size frog
Sz (1236 :. 1920)
λ> cannyFrog <- applyMassivCanny 50 100 frog
λ> writeImage "files/cannyFrog.png" cannyFrog
```

Original Frog

![frog](files/frog.jpg)

After Canny edge detection:

![frogCanny](files/cannyFrog.png)

### Reproduce benchmarks

```shell
$ stack bench :bench-canny --ba '--match prefix Medium'
```

These are the results running canny algorithm on the image of a frog.


```
benchmarking Medium/massiv
time                 51.68 ms   (49.36 ms .. 54.38 ms)
                     0.994 R²   (0.987 R² .. 0.998 R²)
mean                 52.65 ms   (51.09 ms .. 55.09 ms)
std dev              3.450 ms   (2.402 ms .. 5.016 ms)
variance introduced by outliers: 22% (moderately inflated)

benchmarking Medium/repa
time                 90.83 ms   (88.25 ms .. 94.90 ms)
                     0.996 R²   (0.990 R² .. 1.000 R²)
mean                 90.74 ms   (87.92 ms .. 92.91 ms)
std dev              3.854 ms   (2.018 ms .. 6.030 ms)

benchmarking Medium/accelerate
time                 106.8 ms   (100.9 ms .. 113.5 ms)
                     0.996 R²   (0.989 R² .. 1.000 R²)
mean                 108.8 ms   (106.5 ms .. 113.7 ms)
std dev              5.585 ms   (3.103 ms .. 8.623 ms)
variance introduced by outliers: 10% (moderately inflated)

benchmarking Medium/yarr
time                 311.8 ms   (295.6 ms .. 326.4 ms)
                     0.998 R²   (0.994 R² .. 1.000 R²)
mean                 297.5 ms   (288.3 ms .. 303.0 ms)
std dev              9.787 ms   (457.2 μs .. 14.64 ms)
variance introduced by outliers: 16% (moderately inflated)

benchmarking Medium/friday
time                 1.241 s    (1.161 s .. 1.394 s)
                     0.998 R²   (0.996 R² .. 1.000 R²)
mean                 1.198 s    (1.183 s .. 1.223 s)
std dev              24.48 ms   (2.561 ms .. 30.72 ms)
variance introduced by outliers: 19% (moderately inflated)

```
