# QAP regression

Regression between matrices, with a test based on the permutation of the
nodes (Krackhardt, 1988; Dekker, Krackhardt and Snijders, 2007).

## Usage

``` r
qap_lm(
  Y,
  X,
  reps = 1000,
  family = c("gaussian", "binomial"),
  method = c("dsp", "y"),
  diag = FALSE
)
```

## Arguments

- Y:

  A square matrix with the dependent relation

- X:

  A list of square matrices with the independent relations

- reps:

  Number of permutations

- family:

  `gaussian` for a linear regression (default) or `binomial` for a
  logistic regression

- method:

  Whether to permute the dependent matrix (`y`) or the residuals of each
  predictor (`dsp`, default)

- diag:

  Whether the diagonal is considered

## Value

This function returns the coefficients, the proportion of permutations
with a coefficient greater or equal, lower or equal, and larger in
absolute value, than the observed one, and the fit of the model.

## Details

The coefficients are those of an ordinary regression (or a logistic
regression when `family = "binomial"`) of the ties of `Y` on the ties of
the matrices in `X`. As the ties are not independent, the standard
errors of the regression do not apply, and the coefficients are compared
with the ones obtained after permuting the nodes.

With `method = "y"` the rows and columns of `Y` are permuted. With
`method = "dsp"` (default) the double semi-partialling of Dekker et al.
(2007) is used: each predictor is regressed on the other predictors, and
the residuals of that regression are permuted, which behaves better when
the predictors are correlated with each other. The intercept is not
permuted by the double semi-partialling, so its p-values are not
returned.

## References

Dekker, D., Krackhardt, D. and Snijders, T. A. B. (2007). Sensitivity of
MRQAP tests to collinearity and autocorrelation conditions.
Psychometrika, 72(4), 563–581.
[doi:10.1007/s11336-007-9016-1](https://doi.org/10.1007/s11336-007-9016-1)

Krackhardt, D. (1988). Predicting with networks: Nonparametric multiple
regression analysis of dyadic data. Social Networks, 10(4), 359–381.
[doi:10.1016/0378-8733(88)90004-4](https://doi.org/10.1016/0378-8733%2888%2990004-4)

## Author

Alejandro Espinosa-Rada

## Examples

``` r
set.seed(18051889)
Y <- matrix(c(
  0, 1, 1, 0, 0,
  1, 0, 1, 0, 0,
  1, 1, 0, 1, 0,
  0, 0, 1, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)
X1 <- matrix(c(
  0, 1, 0, 0, 0,
  1, 0, 1, 0, 0,
  0, 1, 0, 1, 0,
  0, 0, 1, 0, 1,
  0, 0, 0, 1, 0
), byrow = TRUE, ncol = 5)

qap_lm(Y, list(distance = X1), reps = 100)
#> $coefficients
#>           coefficient p_greater p_lower p_two_sided
#> intercept   0.1666667        NA      NA          NA
#> distance    0.8333333      0.03       1        0.04
#> 
#> $fit
#> [1] 0.6666667
#> 
#> $distribution
#>        intercept      distance
#>   [1,] 0.6666667 -4.166667e-01
#>   [2,] 0.5000000 -2.239957e-17
#>   [3,] 0.6666667 -4.166667e-01
#>   [4,] 0.5000000  1.364684e-17
#>   [5,] 0.5000000  4.902545e-17
#>   [6,] 0.5000000  1.114879e-16
#>   [7,] 0.6666667 -4.166667e-01
#>   [8,] 0.3333333  4.166667e-01
#>   [9,] 0.6666667 -4.166667e-01
#>  [10,] 0.3333333  4.166667e-01
#>  [11,] 0.5000000  8.025668e-17
#>  [12,] 0.1666667  8.333333e-01
#>  [13,] 0.6666667 -4.166667e-01
#>  [14,] 0.5000000  6.718253e-18
#>  [15,] 0.5000000 -1.947467e-17
#>  [16,] 0.6666667 -4.166667e-01
#>  [17,] 0.5000000 -1.947467e-17
#>  [18,] 0.5000000  4.902545e-17
#>  [19,] 0.5000000  1.114879e-16
#>  [20,] 0.5000000  1.114879e-16
#>  [21,] 0.3333333  4.166667e-01
#>  [22,] 0.6666667 -4.166667e-01
#>  [23,] 0.6666667 -4.166667e-01
#>  [24,] 0.5000000  1.114879e-16
#>  [25,] 0.3333333  4.166667e-01
#>  [26,] 0.6666667 -4.166667e-01
#>  [27,] 0.6666667 -4.166667e-01
#>  [28,] 0.5000000  6.718253e-18
#>  [29,] 0.3333333  4.166667e-01
#>  [30,] 0.8333333 -8.333333e-01
#>  [31,] 0.6666667 -4.166667e-01
#>  [32,] 0.3333333  4.166667e-01
#>  [33,] 0.5000000  1.114879e-16
#>  [34,] 0.5000000  6.718253e-18
#>  [35,] 0.5000000  6.718253e-18
#>  [36,] 0.5000000  1.364684e-17
#>  [37,] 0.5000000  1.114879e-16
#>  [38,] 0.5000000  1.364684e-17
#>  [39,] 0.5000000  1.100837e-16
#>  [40,] 0.3333333  4.166667e-01
#>  [41,] 0.3333333  4.166667e-01
#>  [42,] 0.6666667 -4.166667e-01
#>  [43,] 0.6666667 -4.166667e-01
#>  [44,] 0.5000000  6.718253e-18
#>  [45,] 0.5000000  6.718253e-18
#>  [46,] 0.5000000  4.902545e-17
#>  [47,] 0.5000000  6.718253e-18
#>  [48,] 0.5000000  1.364684e-17
#>  [49,] 0.5000000  4.902545e-17
#>  [50,] 0.5000000  8.025668e-17
#>  [51,] 0.1666667  8.333333e-01
#>  [52,] 0.3333333  4.166667e-01
#>  [53,] 0.5000000  6.718253e-18
#>  [54,] 0.3333333  4.166667e-01
#>  [55,] 0.3333333  4.166667e-01
#>  [56,] 0.6666667 -4.166667e-01
#>  [57,] 0.6666667 -4.166667e-01
#>  [58,] 0.5000000  1.114879e-16
#>  [59,] 0.5000000  4.902545e-17
#>  [60,] 0.6666667 -4.166667e-01
#>  [61,] 0.5000000  8.025668e-17
#>  [62,] 0.6666667 -4.166667e-01
#>  [63,] 0.5000000  4.676835e-17
#>  [64,] 0.8333333 -8.333333e-01
#>  [65,] 0.5000000  6.718253e-18
#>  [66,] 0.6666667 -4.166667e-01
#>  [67,] 0.3333333  4.166667e-01
#>  [68,] 0.5000000  6.718253e-18
#>  [69,] 0.6666667 -4.166667e-01
#>  [70,] 0.1666667  8.333333e-01
#>  [71,] 0.3333333  4.166667e-01
#>  [72,] 0.5000000  1.364684e-17
#>  [73,] 0.5000000  4.902545e-17
#>  [74,] 0.5000000  1.114879e-16
#>  [75,] 0.5000000  1.114879e-16
#>  [76,] 0.5000000  1.364684e-17
#>  [77,] 0.5000000  1.364684e-17
#>  [78,] 0.3333333  4.166667e-01
#>  [79,] 0.3333333  4.166667e-01
#>  [80,] 0.3333333  4.166667e-01
#>  [81,] 0.6666667 -4.166667e-01
#>  [82,] 0.5000000 -1.947467e-17
#>  [83,] 0.5000000  6.718253e-18
#>  [84,] 0.6666667 -4.166667e-01
#>  [85,] 0.5000000  1.114879e-16
#>  [86,] 0.3333333  4.166667e-01
#>  [87,] 0.5000000  1.364684e-17
#>  [88,] 0.5000000  6.718253e-18
#>  [89,] 0.5000000  1.364684e-17
#>  [90,] 0.6666667 -4.166667e-01
#>  [91,] 0.6666667 -4.166667e-01
#>  [92,] 0.5000000  4.902545e-17
#>  [93,] 0.6666667 -4.166667e-01
#>  [94,] 0.6666667 -4.166667e-01
#>  [95,] 0.5000000  4.902545e-17
#>  [96,] 0.5000000 -1.947467e-17
#>  [97,] 0.6666667 -4.166667e-01
#>  [98,] 0.6666667 -4.166667e-01
#>  [99,] 0.5000000  4.902545e-17
#> [100,] 0.6666667 -4.166667e-01
#> 
```
