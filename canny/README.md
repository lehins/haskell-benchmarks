# Canny benchmarks



```
benchmarking Canny/massiv
time                 63.04 ms   (61.24 ms .. 66.39 ms)
                     0.994 R²   (0.983 R² .. 1.000 R²)
mean                 66.76 ms   (64.75 ms .. 70.33 ms)
std dev              4.592 ms   (3.181 ms .. 6.190 ms)
variance introduced by outliers: 17% (moderately inflated)

benchmarking Canny/repa
time                 106.8 ms   (99.67 ms .. 117.0 ms)
                     0.990 R²   (0.969 R² .. 0.999 R²)
mean                 107.9 ms   (104.1 ms .. 111.8 ms)
std dev              6.045 ms   (4.246 ms .. 8.121 ms)
variance introduced by outliers: 10% (moderately inflated)

benchmarking Canny/accelerate
time                 146.6 ms   (139.3 ms .. 153.3 ms)
                     0.997 R²   (0.994 R² .. 1.000 R²)
mean                 145.5 ms   (142.1 ms .. 149.5 ms)
std dev              5.599 ms   (4.295 ms .. 8.142 ms)
variance introduced by outliers: 12% (moderately inflated)
```


## Setup instructions on Ubuntu-18.04

Dependencies for `accelerate-llvm-native`

```
$ sudo apt-get install llvm-8-dev
$ sudo apt-get install libffi-dev
```

_Note_ - No GPU benchmarks here
