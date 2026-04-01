# Structural Missing Data

Assign NA to missing data in matrices.

## Usage

``` r
structural_na(
  A,
  label = NULL,
  row_labels = NULL,
  col_labels = NULL,
  two_mode = FALSE
)
```

## Arguments

- A:

  An incident or symmetric matrix object.

- label:

  A string vector with the names of the theoretical complete matrix
  (used for one-mode networks only).

- row_labels:

  A string vector with the names of the rows (used for two-mode
  networks).

- col_labels:

  A string vector with the names of the columns (used for two-mode
  networks).

- two_mode:

  Boolean indicating whether the matrix is two-mode. Default is FALSE.

## Value

This function returns a matrix with NA assigned to missing data.

## Examples

``` r
# Example for one-mode network
A <- matrix(c(
  0, 1, 1,
  1, 0, 1,
  0, 0, 0
), byrow = TRUE, ncol = 3)
colnames(A) <- c("A", "C", "D")
rownames(A) <- c("A", "C", "D")
label <- c("A", "B", "C", "D", "E")
structural_na(A, label = label)
#> Warning: Provided labels do not match the dimensions of the matrix.
#>    A  B  C  D  E
#> A  0 NA  1  1 NA
#> B NA NA NA NA NA
#> C  1 NA  0  1 NA
#> D  0 NA  0  0 NA
#> E NA NA NA NA NA

# Example for two-mode network
B <- matrix(c(
  0, 1, 0,
  1, 0, 1,
  0, 1, 0,
  1, 0, 1
), byrow = TRUE, ncol = 3)
rownames(B) <- c("X1", "X2", "X3", "X4")
colnames(B) <- c("Y1", "Y2", "Y3")
rlabels <- c("X1", "X2", "X3", "X4", "X5")
clabels <- c("Y1", "Y2", "Y3", "Y4")
structural_na(B, row_labels = rlabels, col_labels = clabels, two_mode = TRUE)
#>    Y1 Y2 Y3 Y4
#> X1  0  1  0 NA
#> X2  1  0  1 NA
#> X3  0  1  0 NA
#> X4  1  0  1 NA
#> X5 NA NA NA NA
```
