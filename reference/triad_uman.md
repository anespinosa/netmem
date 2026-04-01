# Triad census analysis assuming U\|MAN

Considering the triad census of Davis and Leinhardt (1972) for vector A,
B, and C:

## Usage

``` r
triad_uman(A, ztest = FALSE, covar = FALSE)
```

## Arguments

- A:

  A symmetric matrix object

- ztest:

  Return Z and p-value

- covar:

  Return the covariance matrix for triadic analysis

## Value

This function gives the counts of the triad census, the expected counts,
assuming that U\|MAN distribution (Holland and Leinhardt, 1975, 1976) is
operating, and the standard deviations of these counts.

## Details

003 = A,B,C, empty triad

012 = A -\> B, C, triad with a single directed edge

102 = A \<-\> B, C, triad with a reciprocated connection between two
vertices

021D = A \<-B-\> C, triadic out-star

021U = A -\> B \<- C triadic in-star

021C = A-\> B-\> C, directed line

111D = A \<-\> B \<-C

111U = A \<-\> B-\> C

030T = A-\> B \<-C, A-\> C

030C = A \<-B \<-C, A-\> C

201 = A \<-\> B \<-\> C

120D = A \<-B-\> C, A \<-\> C

120U = A-\> B \<-C, A \<-\>C

120C = A-\> B-\> C, A \<-\> C

210 = A-\> B \<-\> C, A \<-\> C

300 = A \<-\> B \<-\> C, A \<-\>C, complete triad.

## References

Davis, J.A. and Leinhardt, S. (1972). The Structure of Positive
Interpersonal Relations in Small Groups. In J. Berger (Ed.),
Sociological Theories in Progress, Volume 2, 218-251. Boston: Houghton
Mifflin.

Holland, P. W. and Leinhardt, S. (1975). The statistical analysis of
local structure in social networks. In D. R. Heise (Ed.), Sociological
Methodology, 1976 (Jossey-Bass, pp. 1–45).

Holland, P. W. and Leinhardt, S. (1976). Local Structure in Social
Networks. Sociological Methodology, 7, 1–45.
[doi:10.2307/270703](https://doi.org/10.2307/270703)

Wasserman, S. and Faust, K. (1994). Social network analysis: Methods and
applications. Cambridge University Press.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
data(krackhardt_friends)
triad_uman(krackhardt_friends)
#>    label OBS     EXP     VAR    STD
#> 1    003 376 320.057  88.239  9.394
#> 2    012 366 416.818 211.856 14.555
#> 3    102 143 171.193  88.921  9.430
#> 4   021D 114  44.087  38.736  6.224
#> 5   021U  34  44.087  38.736  6.224
#> 6   021C  35  88.173  66.770  8.171
#> 7   111D  39  73.745  60.470  7.776
#> 8   111U 101  73.745  60.470  7.776
#> 9   030T  23  18.173  14.902  3.860
#> 10  030C   0   6.058   5.694  2.386
#> 11   201  20  28.971  20.471  4.524
#> 12  120D  16   7.740   7.359  2.713
#> 13  120U  25   7.740   7.359  2.713
#> 14  120C   9  15.481  13.957  3.736
#> 15   210  23  12.385  10.534  3.246
#> 16   300   6   1.548   1.441  1.200
if (FALSE) { # \dontrun{
triad_uman(krackhardt_friends, ztest = TRUE, covar = TRUE)
} # }
```
