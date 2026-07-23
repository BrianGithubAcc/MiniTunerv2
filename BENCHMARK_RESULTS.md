# OpTuner and MiniTuner benchmark results

The time in each successful cell is core tuning time: importing, exact
search, and rigorous validation, excluding plot generation where the
runner records it separately. A check without a time means the backend
is implemented but its timing row is unavailable. A cross reports the
suite's explicit failure or unsupported status.

| # | Benchmark | OpTuner status | Time | MiniTuner + FPTaylor status | Time | MiniTuner + SATIRE status | Time |
|---:|---|---|---:|---|---:|---|---:|
| 1 | Data.HyperLogLog.Type:size from hyperloglog-0.3.4, A | ✅ | 22.567 s | ✅ | 11.017 s | ✅ | 0.319 s |
| 2 | Data.Number.Erf:$dmerfcx from erf-2.0.0.0 | ✅ | 39.013 s | ✅ | 10.284 s | ✅ | 0.319 s |
| 3 | Data.Random.Distribution.Normal:normalF from random-fu-0.2.6.2 | ✅ | 16.289 s | ✅ | 6.072 s | ✅ | 0.219 s |
| 4 | Diagrams.ThreeD.Transform:aboutX from diagrams-lib-1.3.0.3, A | ✅ | 211.606 s | ✅ | 34.260 s | ✅ | 0.821 s |
| 5 | Diagrams.ThreeD.Transform:aboutX from diagrams-lib-1.3.0.3, B | ✅ | 185.652 s | ✅ | 30.395 s | ✅ | 0.871 s |
| 6 | Diagrams.ThreeD.Transform:aboutY from diagrams-lib-1.3.0.3 | ✅ | 178.878 s | ✅ | 28.473 s | ✅ | 0.821 s |
| 7 | Diagrams.TwoD.Path.Metafont.Internal:hobbyF from diagrams-contrib-1.3.0.5 | ❌ exceeded 300 seconds | — | ❌ division domain includes zero | — | ❌ division domain includes zero | — |
| 8 | Linear.Quaternion:$cexp from linear-1.19.1.3 | ✅ | 56.290 s | ✅ | 9.930 s | ✅ | 0.420 s |
| 9 | Numeric.SpecFunctions:invIncompleteBetaWorker from math-functions-0.1.5.2, B | ✅ | 111.449 s | ✅ | 128.675 s | ✅ | 0.320 s |
| 10 | Numeric.SpecFunctions:logBeta from math-functions-0.1.5.2, A | ✅ | 153.460 s | ✅ | 148.088 s | ✅ | 0.371 s |
| 11 | Numeric.SpecFunctions:logBeta from math-functions-0.1.5.2, B | ❌ exceeded 300 seconds | — | ✅ | 234.410 s | ✅ | 0.671 s |
| 12 | Numeric.SpecFunctions:logGammaL from math-functions-0.1.5.2 | ❌ exceeded 300 seconds | — | ✅ | 199.127 s | ✅ | 1.020 s |
| 13 | Numeric.SpecFunctions:$slogFactorial from math-functions-0.1.5.2, B | ✅ | 120.481 s | ✅ | 31.150 s | ✅ | 0.471 s |
| 14 | Numeric.SpecFunctions:stirlingError from math-functions-0.1.5.2 | ✅ | 62.049 s | ✅ | 14.030 s | ✅ | 0.420 s |
| 15 | Statistics.Distribution.Beta:$cdensity from math-functions-0.1.5.2 | ✅ | 121.495 s | ✅ | 129.733 s | ✅ | 0.370 s |
| 16 | Statistics.Distribution.Binomial:directEntropy from math-functions-0.1.5.2 | ✅ | 21.483 s | ✅ | 6.749 s | ✅ | 0.270 s |
| 17 | Statistics.Distribution.Poisson.Internal:probability from math-functions-0.1.5.2 | ✅ | 187.192 s | ✅ | 29.033 s | ✅ | 0.521 s |
| 18 | Statistics.Distribution.Poisson:$clogProbability from math-functions-0.1.5.2 | ✅ | 35.004 s | ✅ | 10.054 s | ✅ | 0.268 s |
| 19 | azimuth | ❌ exceeded 300 seconds | — | ❌ division domain includes zero | — | ❌ division domain includes zero | — |
| 20 | complex_sine_and_cosine | ❌ exceeded 300 seconds | — | ✅ | 80.085 s | ✅ | 0.819 s |
| 21 | exp1x | ✅ | 39.369 s | ✅ | 8.370 s | ✅ | 0.318 s |
| 22 | exp1x_log | ❌ one or more exact candidates failed FPTaylor validation | — | ✅ | 29.399 s | ✅ | 0.668 s |
| 23 | hartman3 | ✅ | 188.712 s | ✅ | 102.947 s | ✅ | 0.519 s |
| 24 | hartman6 | ❌ exceeded 300 seconds | — | ❌ exact pipeline exited with code 1 | — | ✅ | 2.172 s |
| 25 | i6 | ❌ precision is binary32 | — | ❌ unsupported precision: binary32 | — | ❌ unsupported precision: binary32 | — |
| 26 | logexp | ✅ | 24.988 s | ✅ | 11.941 s | ✅ | 0.621 s |
| 27 | logexp2 | ✅ | 26.670 s | ✅ | 2.503 s | ✅ | 0.470 s |
| 28 | nmse_example_3_10 | ✅ | 73.359 s | ✅ | 16.553 s | ✅ | 0.471 s |
| 29 | nmse_example_3_3 | ✅ | 95.190 s | ✅ | 15.584 s | ✅ | 0.620 s |
| 30 | nmse_example_3_4 | ❌ one or more exact candidates failed FPTaylor validation | — | ❌ division domain includes zero | — | ❌ division domain includes zero | — |
| 31 | nmse_example_3_7 | ✅ | 14.373 s | ✅ | 5.079 s | ✅ | 0.269 s |
| 32 | nmse_example_3_8 | ✅ | 182.257 s | ✅ | 24.186 s | ✅ | 0.671 s |
| 33 | nmse_problem_3_3_2 | ✅ | 99.564 s | ✅ | 17.501 s | ✅ | 0.671 s |
| 34 | nmse_problem_3_3_6 | ✅ | 74.885 s | ✅ | 13.293 s | ✅ | 0.671 s |
| 35 | nmse_problem_3_3_7 | ✅ | 273.548 s | ✅ | 16.561 s | ✅ | 0.671 s |
| 36 | nmse_problem_3_4_2 | ❌ exceeded 300 seconds | — | ✅ | 292.579 s | ✅ | 1.772 s |
| 37 | nmse_problem_3_4_4 | ❌ exceeded 300 seconds | — | ✅ | 38.195 s | ✅ | 0.822 s |
| 38 | nmse_section_3_11 | ✅ | 55.681 s | ✅ | 13.654 s | ✅ | 0.470 s |
| 39 | nmse_section_3_5 | ✅ | 30.993 s | ✅ | 7.718 s | ✅ | 0.319 s |
| 40 | povray_photons | ❌ exceeded 300 seconds | — | ❌ exact pipeline exited with code 1 | — | ✅ | 1.672 s |
| 41 | sphere | ✅ | 193.136 s | ✅ | 39.288 s | ✅ | 0.518 s |

## Coverage

- OpTuner: 29/41
- MiniTuner + FPTaylor: 35/41
- MiniTuner + SATIRE: 37/41

## Accuracy note

Historical OpTuner and MiniTuner+FPTaylor plots may contain the
legacy exponent-zero conversion that disabled a nonzero absolute
delta term. Regenerate the FPTaylor suite with the current code
before comparing error bounds. Times above remain the recorded
runtime measurements from their respective suite runs.

Regenerate this file after running the three suites:

```bash
python src/scripts/generate_benchmark_table.py
```
