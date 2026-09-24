# Social influence

Simulates how the opinions of the actors change when they are influenced
by their neighbours, under the rules compared by Flache et al. (2017).

## Usage

``` r
social_influence(
  A,
  opinion,
  rule = c("assimilation", "bounded", "repulsion", "friedkin"),
  mu = 0.3,
  epsilon = 0.15,
  susceptibility = 0.5,
  steps = 50
)
```

## Arguments

- A:

  A square matrix of influence, which can be weighted. The rows are
  usually normalised so that the weights of each actor add up to one

- opinion:

  A vector with the initial opinion of every actor, usually between zero
  and one

- rule:

  The rule of influence: `assimilation` (default), `bounded`,
  `repulsion` or `friedkin`

- mu:

  How much an actor moves at each step

- epsilon:

  Maximum difference of opinion that still influences an actor, for the
  `bounded` rule

- susceptibility:

  Weight given to the neighbours in the `friedkin` rule, as a number or
  a vector with one value per actor

- steps:

  Number of steps

## Value

This function returns the opinions of every actor at every step, the
final opinions, and the number of groups of opinions at the end.

## Details

Every actor starts with an opinion and updates it at each step:

`assimilation`: the actor moves towards the opinions of its neighbours,
which leads the network to consensus (French, 1956; DeGroot, 1974).

`bounded`: the actor is only influenced by the neighbours whose opinion
differs less than `epsilon`, which leads to fragmentation into groups
that no longer influence each other (Hegselmann and Krause, 2002).

`repulsion`: the actor moves towards similar neighbours and away from
those who are too different, which leads to bi-polarization. The
opinions are kept between zero and one.

`friedkin`: the actor combines the opinions of its neighbours with the
opinion it started with, and `susceptibility` is the weight given to the
neighbours (Friedkin and Johnsen, 1990).

## References

DeGroot, M. H. (1974). Reaching a consensus. Journal of the American
Statistical Association, 69(345), 118–121.
[doi:10.1080/01621459.1974.10480137](https://doi.org/10.1080/01621459.1974.10480137)

Flache, A., Mas, M., Feliciani, T., Chattoe-Brown, E., Deffuant, G.,
Huet, S. and Lorenz, J. (2017). Models of social influence: Towards the
next frontiers. Journal of Artificial Societies and Social Simulation,
20(4), 2. [doi:10.18564/jasss.3521](https://doi.org/10.18564/jasss.3521)

French, J. R. P. (1956). A formal theory of social power. Psychological
Review, 63(3), 181–194.
[doi:10.1037/h0046123](https://doi.org/10.1037/h0046123)

Friedkin, N. E. and Johnsen, E. C. (1990). Social influence and
opinions. Journal of Mathematical Sociology, 15(3-4), 193–206.
[doi:10.1080/0022250X.1990.9990069](https://doi.org/10.1080/0022250X.1990.9990069)

Hegselmann, R. and Krause, U. (2002). Opinion dynamics and bounded
confidence models, analysis and simulation. Journal of Artificial
Societies and Social Simulation, 5(3), 2.

## Author

Alejandro Espinosa-Rada

## Examples

``` r
A <- matrix(c(
  0, 1, 1, 0, 0, 0,
  1, 0, 1, 0, 0, 0,
  1, 1, 0, 1, 0, 0,
  0, 0, 1, 0, 1, 1,
  0, 0, 0, 1, 0, 1,
  0, 0, 0, 1, 1, 0
), byrow = TRUE, ncol = 6)
rownames(A) <- letters[1:nrow(A)]
colnames(A) <- rownames(A)
W <- A / rowSums(A)
opinion <- c(0.1, 0.2, 0.3, 0.7, 0.8, 0.9)

social_influence(W, opinion, rule = "assimilation", steps = 20)$final
#> [1] 0.4021102 0.4021108 0.4421799 0.5578201 0.5978892 0.5978898
social_influence(W, opinion, rule = "bounded", epsilon = 0.15, steps = 20)$groups
#> [1] 2
```
