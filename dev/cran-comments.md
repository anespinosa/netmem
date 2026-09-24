# netmem 1.1-0

This is a minor release of a package already on CRAN (1.0-3). It corrects
several errors in the results of existing functions and adds new ones.

## Test environments

* local macOS, R 4.5.0 (R CMD check --as-cran on the built tarball)
* win-builder, R-devel
* macOS builder (mac.r-project.org), R release
* GitHub Actions: macOS, Windows and Ubuntu (R-devel, release and oldrel-1)

## R CMD check results

0 errors | 0 warnings | 1 note

The macOS builder returns OK, with no notes. win-builder returns one note with
two parts:

* "Possibly misspelled words in DESCRIPTION: Brandes, Schoch, Traag". They are
  the surnames of the authors of the references cited in the description.
* An invalid URL in README.md, pointing to a vignette of the website. The
  website is rebuilt when this version is released, so the page will exist. The
  links were replaced by the names of the vignettes.

The notes of the local machine ("unable to verify current time" and "'tidy'
doesn't look like recent enough HTML Tidy") come from the machine and not from
the package.

## Changes that affect the results of existing functions

Users of version 1.0-3 will obtain different results from the functions below.
The reason is that the previous results were wrong, and each correction is
checked against another implementation or against a published table. They are
listed at the top of NEWS.md.

* `count_geodesics()` did not count all the geodesics between two nodes. The
  counts now agree with `sna::geodist()`.
* `wall_distances()` returned one path per node instead of one per pair of
  nodes, and it enumerated every simple path instead of using Dijkstra's
  algorithm, which did not finish on networks of a dozen nodes.
* `mix_matrix()` counted only the ties in the upper triangle of the matrix, and
  dropped the groups that never sent or received a tie.
* `ei_index()` ignored the attribute of the nodes with its default arguments,
  and returned one for any network without loops. It now agrees with
  `netseg::ei()`.
* `components_id()` did not return the components of a directed network.
* `multilevel_degree()` returned the degrees of the second level in the rows of
  the third level when three levels were given without `B3`.
* `gen_degree()` did not apply the loops and the symmetrization to the weights.
* `triad_uman()` had a wrong covariance between the counts of the triads 201
  and 102, and its `z_test` element was `NaN` for every network, as it tested a
  quantity that is constant by construction. The element is replaced by a test
  of a linear combination of the census, through the new argument `l`.
* `trans_matrix()` and `edgelist_to_matrix()` returned wrong results or failed
  in cases described in NEWS.md.
* `gen_density()` removed the diagonal of two-mode matrices, and computed the
  density of directed matrices in a list from their lower triangle.
* `eb_constraint()` used the wrong maximum for egos with seven alters, which
  changed their normalized constraint.
* `q_analysis()` skipped the dimensions at which no simplex had exactly that
  size, and built the complex of a network from its triangles instead of its
  maximal cliques. Its result is now a list with more elements.
* `simplicial_complexes()` built the complex from triangles and edges instead
  of maximal cliques, failed without triangles, and `zero_simplex = TRUE`
  returned columns that did not match the nodes. Its columns are now named
  after the nodes of each simplex.
* An audit of every function corrected `multiplex_census()`,
  `mixed_census(quad = TRUE)`, `kp_reciprocity()`, `posneg_index()`, the
  weighted and multilevel `k_core()`, the undirected versions of several
  functions that ignored the ties of the lower triangle, the conversions
  between matrices and edge lists, `meta_matrix()`, `extract_component()` and
  `dist_sim_matrix()`. NEWS.md describes each change.
* `k_core()` counted the loops of a binary network even with `loops = FALSE`.
* `zone_sample()` returns adjacency matrices instead of igraph objects, with the
  same nodes and ties.
* `fractional_approach()` did not compute the networks of Batagelj (2020). It
  now takes the citation network between works and the authorship matrix, so
  calls written for version 1.0-3 stop with an informative error.
* `dyad_triad_table()` called every pair of neighbours of a node a forbidden
  triad, closed or not. It now returns the type of each triad, and `min` and
  `max` refer to the number of forbidden triads centred on a node.

## Downstream dependencies

There are no reverse dependencies on CRAN.

## Other notes

* The new functions are implemented with matrix algebra and do not add
  dependencies. The package now imports only Matrix and stats; igraph moved to
  Suggests and is used only to draw the plots of the vignette, which are not
  evaluated when it is not installed.
* Two vignettes were added; the three vignettes use only the data of the
  package and take a few seconds to build.
* The results of the new functions are compared with igraph, sna, netrankr,
  netseg, ergm, signnet, ape and geosphere in scripts that are kept in the
  repository and excluded from the build.
