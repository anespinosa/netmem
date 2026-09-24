# netmem 1.1-0

## Changes that affect results

These fixes change the numbers that some functions returned. Code written for
version 1.0-3 runs without errors, but gives different results:

* `count_geodesics()` counted only some of the geodesics between two nodes. A
  node that had already been reached did not add the geodesics of the other
  nodes at the same distance, so the counts were too low. The counts now agree
  with `sna::geodist()`.
* `wall_distances()` returned a single path for each node instead of one path
  for each pair of nodes. It now returns `fromTo[[i]][[j]]` and
  `toFrom[[i]][[j]]` for every pair.
* `wlocal_distances()` and `wall_distances()` now use Dijkstra's algorithm, as
  the documentation said. Before they enumerated every simple path, which did
  not finish on networks of a dozen nodes. Between paths of the same length,
  the path returned might differ from the one returned before.
* `triad_uman()`: the covariance between the counts of 201 and 102 was
  positive, and should have been negative. The term of the two triads that
  share a dyad used `m + n + 4` where the combinatorics give `m + n - 4`. The
  covariance matrix now adds up to zero, as it must because the number of
  triads is fixed.
* `triad_uman()` no longer returns the element `z_test` by default. It tested
  the sum of the sixteen counts, which is always `choose(g, 3)`, so it was
  `NaN` for every network. The new argument `l` tests a linear combination of
  the census (Wasserman and Faust, 1994: 583); with `ztest = TRUE` and no `l`,
  the function returns the data frame with the columns `Z` and `P`. The
  covariance matrix is now returned symmetric instead of triangular.
* `eb_constraint()` failed for egos with a single alter, and for egos whose
  alters are not tied to each other. It now returns the constraint in both
  cases, and gives an informative error for isolates.
* `components_id()` did not return the components of a directed network. It
  grouped the nodes that reach the same nodes, so a chain of citations gave as
  many components as nodes. It now returns the weak components, which for
  undirected networks are the ones it returned before, and takes the arguments
  `mode = "strong"` and `bipartite`.
* `edgelist_to_matrix()` placed some ties in the wrong direction, depending on
  the order in which the nodes appeared in the edge list, and dropped the ties
  of a node with itself. The names are now matched one by one, and `loops =
  TRUE` keeps the diagonal. It also returned a vector instead of a matrix when
  the result had a single row or column.
* `mix_matrix()` counted only the ties of the upper triangle of the matrix, so
  for a directed network with 15 arcs it returned a table adding up to 5, and
  it dropped the row or the column of a group that never sent or received a
  tie, which made the table lose its shape. The mixing matrix is now built from
  the ties between every pair of groups.
* `ei_index()` ignored the attribute unless `mixed = FALSE` was given, and with
  its default arguments it returned one for any network without loops. The
  attribute now takes precedence, and the index agrees with `netseg::ei()` for
  directed and undirected networks.
* `trans_matrix()` failed with "the condition has length > 1" whenever a node
  belonged to more than one transitive triple, which is the usual case in an
  undirected network with a triangle. It now marks the nodes of every
  transitive triple.
* `triad_uman()` failed on a network without ties, because the matrix was
  stored in a class that cannot be modified.
* `multilevel_degree()` returned wrong values for the nodes of the third level
  when `B2` was given without `B3`: the rows `k1, k2, ...` of the column
  `multilevel` held the degrees of the nodes of the second level. They now count
  the ties of each node within the third level and with the second level, which
  is the value of the same nodes in `high_multilevel`, and the documentation
  describes what every column counts.
* `gen_degree()` with `weighted = TRUE` removed the loops from the degree but
  not from the strength when `loops = FALSE`, and symmetrized the ties but not
  the weights when `digraph = FALSE`. The weights now follow the ties in both
  cases.
* `gen_density()` removed the diagonal of two-mode matrices, which holds real
  ties, so with the default `loops = FALSE` the density of an incidence matrix
  was too low (its own example gave 0.33 instead of 0.5). For a directed matrix
  in a list (`multilayer = TRUE`) it used only the lower triangle, and with
  `directed = FALSE` it kept the ties of the upper triangle instead of the
  underlying graph. With `loops = TRUE` the diagonal is now counted among the
  possible ties. The results agree with `igraph::edge_density()`.
* `eb_constraint()` chose the maximum constraint by the size of the ego network
  including ego, so an ego with seven alters got the maximum of the shadow ego
  network (0.486) instead of the complete one (0.493, Everett and Borgatti,
  2020: Table 1), and a normalization that was too high. The maximum is now the
  larger of the two. An ego with a single alter has a normalization of one, as
  in Everett and Borgatti (2020: Eq. 5), instead of `NaN`.
* `q_analysis()` reported only the dimensions q that some simplex had exactly,
  so the levels at which components merge were missing: two 4-simplices that
  share a face of dimension 2 were never reported as 2-connected. It now
  reports every q from the largest dimension down to 0. A network was turned
  into a complex of its triangles and all its edges, so a clique of four nodes
  became four triangles; the simplices are now the maximal cliques. A square
  incidence matrix, which stopped the function, is now accepted. The function
  returns a list with the simplices, the table of the structure vectors, the
  components at each q and the eccentricities; the argument `dimensions` is
  kept but no longer needed. The values agree with Freeman (1980) and with the
  Python package of Smirnov et al. (2025).
* `simplicial_complexes()` built the complex of a network from its triangles
  and all its edges, so a clique of four nodes became four triangles and six
  edges, and it stopped with an error on networks without triangles. The
  simplices are now the maximal cliques, named after their nodes (`a-b-c`)
  instead of numbered. `zero_simplex = TRUE`, now the default, adds the isolated
  nodes as simplices of dimension 0; before it added columns that did not
  correspond to the nodes. The rows are still the nodes and the columns the
  simplices.
* `k_core()` of a binary network counted the loops even when `loops = FALSE`,
  which is the default. The loops are now counted only with `loops = TRUE`.
  Without loops the values are the same as before.
* `zone_sample()` returns adjacency matrices instead of igraph objects. With
  `core = TRUE`, the indicator of the actors at distance one is the attribute
  `core` of each matrix. The nodes and the ties are the same as before.

* An audit of every exported function found further errors, recorded in
  `dev/audit/audit.md` with how each was verified:
  * `multiplex_census()` added counts of the two networks instead of counting
    the triples of each joint configuration (two empty networks gave 5 of 10
    triples). It now returns the classes of Figure 12 of Espinosa-Rada (2021),
    named by the type of the directed triad and the position of the
    undirected edges (e.g. `021U_102ac`); `merge = "overlap"` merges the
    classes that give the same overlapped triad.
  * `mixed_census(quad = TRUE)`: the class "201" multiplied two counts instead
    of adding them.
  * `kp_reciprocity()` used the number of arcs minus twice the mutual dyads
    instead of the number of arcs for free choices.
  * `posneg_index()`: `select = "all"` returned the out-index and `"out"` the
    all-index. They now agree with `signnet::pn_index()`.
  * `k_core()` with `weighted = TRUE` or `multilevel = TRUE` returned the round
    in which a node was removed instead of its core value (a path gave 1 0 1).
  * With `digraph = FALSE`, `gen_degree()`, `eb_constraint()`, `redundancy()`,
    `clique_table()`, `dyad_triad_table()`, `struc_balance()` and the signed
    `eigenvector_centrality()` copied the upper triangle of the matrix over the
    lower one, so the ties present only in the lower triangle were lost and
    the results depended on the order of the nodes. They now use the
    underlying graph.
  * `edgelist_to_matrix(digraph = FALSE)` dropped the edges listed as (b, a);
    `matrix_to_edgelist(valued = TRUE)` dropped ties with non-integer values
    and listed undirected ties twice; `matrix_adjlist()` dropped ties below 1.
  * `adj_to_matrix(type = "adjacency")` returned one row per line of the list
    instead of one per node.
  * `meta_matrix()` placed the ties between the second and third levels only
    above the diagonal, and dropped them when `B3` was given.
  * `extract_component()` returned wrong matrices when several components had
    the same size; `position` now counts the distinct sizes.
  * `dist_sim_matrix()` computed the Hamming distance of two-mode matrices
    over the wrong nodes, failed with rows without ties, and now keeps the
    names of the nodes.

* `fractional_approach()` did not compute the networks of Batagelj (2020): its
  "citation" network was a product of the two incidence matrices, and the
  fractional co-citation was normalised on one side only. It now takes the
  citation network between works (`A1`) and, for the citations between
  authors, the authorship matrix (`A2`), with full or fractional counting
  (`fractional`); the fractional bibliographic coupling can be made symmetric
  with the six measures of Batagelj (2020) (`symmetric`). Calls with two
  incidence matrices, as in version 1.0-3, now stop.
* `dyad_triad_table()` called "Triad201" every pair of neighbours of a node,
  closed or not, and `min` and `max` limited how often a triad was repeated
  (which selected the closed triads). It now returns the `type` of each triad
  (201 for the forbidden triad of Granovetter, 300 when closed, 102 for a dyad
  and 003 for an isolated node) and its `members`, and `min` and `max` limit
  the number of forbidden triads of which a node is the centre.

## Other bug fixes

* `structural_na()` warned whenever the labels had more nodes than the matrix,
  which is its purpose. It now warns only when a node of the matrix is not in
  the labels and is dropped.
* `eb_constraint()`, `redundancy()` and `ego_net()` without `ego` failed with
  "argument is of length zero"; they now ask for the name of ego.
* `trans_coef(method = "mean")` always failed with `could not find function
  "local_trans"`.
* `short_path()` failed when there was no path between the two nodes. It now
  warns and returns `NULL`.
* `percolation_clique()` failed when every node belonged to a clique.
* `power_function()` reached the limit of nested expressions for large powers,
  which made its own example fail. It now uses a loop instead of recursion.
* `matrix_to_edgelist()` failed on a network without ties. It now returns an
  empty edge list.
* `percolation_clique()` failed when the network had a single clique, and when
  some nodes did not belong to any clique.
* `redundancy()` failed with an obscure message for an isolated ego, and
  `clique_table()` stopped without a message when there were no cliques.

* Several functions failed or never ended on common inputs: `k_core()` looped
  forever on a matrix with `NA`, and `ind_rand_matrix(type = "edges")` for
  undirected networks whenever `l` was larger than the number of nodes.
  `bfs_ugraph()`, `count_geodesics()`, `gen_degree()`, `short_path()`,
  `wall_distances()`, `wlocal_distances()`, `matrix_to_edgelist()`,
  `multiplex_census()` and `ego_net()` now treat `NA` as an absent tie;
  `adj_to_incidence()` and `edgelist_to_matrix()` accept networks without
  ties; `minmax_overlap()` accepts a single row; `co_occurrence(occurrence =
  FALSE)` no longer fails; `ego_net()` returns a matrix for an ego with one
  alter, and `redundancy()` no longer calls that ego an isolate.
* `wall_distances()` and `wlocal_distances()` accept binary matrices, in
  which every tie has length one.

## Extensions of the existing functions

* `ind_rand_matrix()` takes `sparse`, which draws the ties among the cells that
  the model allows and returns a sparse matrix of the `Matrix` package, without
  building the dense one. A network of 10,000 nodes takes 0.2 MB instead of
  800 MB. It is available for one-mode and two-mode networks with `trials = 1`.
* `edgelist_to_matrix()` orders the nodes as in `label` (and `label2` for
  two-mode networks), with the nodes that are not in the labels after them in
  alphabetical order. Before, the nodes were always in alphabetical order, so
  a matrix could not be recovered from its edge list in its own order.
* `components_id()` takes `mode` (weak or strong) and `bipartite`, which covers
  the nodes of both modes (issue #4).
* `eigenvector_centrality()` takes `signed`, for networks with negative ties
  (Bonacich and Lloyd, 2004) (issue #9).
* `trans_coef()` takes `method = "barrat"`, the weighted transitivity of Barrat
  et al. (2004) (issue #5).
* `gen_density()` computes the density of weighted networks, which is the
  average strength of the possible ties (issue #12).
* `edgelist_to_matrix()` takes `valued`, which reads the value of the ties from
  a third column, and `loops` (issue #2).
* `dist_sim_matrix()` takes a list of matrices, and compares the nodes across
  all the relations at once (issue #7).
* `q_analysis()` returns the second and third structure vectors (Raj et al.,
  2024), the obstruction vector and the eccentricity of each simplex, with the
  definition of Atkin (1974) or of Johnson (`eccentricity`). A network can be
  analysed through its clique complex or its neighbourhood complex (`complex`),
  with open or closed neighbourhoods (`closed`). It is also about a hundred
  times faster.
* `simplicial_complexes()` takes `complex = "neighbourhood"` and `closed`, as
  `q_analysis()`, which now builds its complex with it, and `valued` for
  projections that count the shared nodes and simplices.
* `eb_constraint()` takes `digraph = TRUE` and `weighted = TRUE`, which used to
  stop. A directed network has the constraint of the valued network `A + t(A)`,
  and the maximum used in the normalization is that of Everett and Borgatti
  (2020: Eq. 6 to 9). The results agree with their Tables 1 and 2 and with
  `igraph::constraint()` on the ego network.

* `edgelist_to_matrix()` takes `rule` for undirected networks: a tie listed in
  either order (`weak`, default) or only in both orders (`strong`), as
  `sna::symmetrize()`.
* `gen_degree()` and `gen_density()` no longer warn that a symmetric matrix is
  undirected when `digraph = TRUE` (or `directed = TRUE`), as the result is the
  same.

## New functions

* `supra_adjacency()` arranges the layers of a multiplex network in a single
  matrix of actor-layer pairs, with categorical, ordinal or no coupling between
  the layers (De Domenico et al., 2013; Kivela et al., 2014), and
  `aggregate_layers()` reduces the layers to a single matrix by sum, binary or
  mean (Battiston et al., 2014).
Centrality:

* `closeness_centrality()`, `betweenness_centrality()` (Brandes' algorithm),
  `eigenvector_centrality()`, `katz_centrality()`, `bonacich_power()`,
  `page_rank_centrality()` and `centrality_centralization()`.
* `geo_distances()` and `geo_summary()` for the distances, the diameter and the
  average distance.

Positions and dominance:

* `neigh_inclusion()`, `set_inclusion()`, `pareto_dominance()`,
  `dominance_pairs()`, `preserved_order()` and `dominance_layers()`.
* `hyperevent_dominance()` for the dominance of authors through the chain
  author, citing paper, cited paper, author (Espinosa-Rada, 2026). With the
  data of the article, it gives the same dominance matrices as the analysis
  scripts, the maximal and dominant authors of Section 6 and the values of
  Table 1. The arguments `strict` and `closure_papers` give the alternatives
  of the text of Section 3.5 where it differs from the analysis scripts.
* `dir_inclusion()` with the nine directed neighbourhood-inclusion criteria of
  Marmulla and Brandes (2026).
* `pos_dominance()`, `indirect_rel()` and `dominance_ranks()`.

Roles, positions and macro structure:

* `block_density()`, `concor()` and `rege()`.
* `krackhardt_index()` with the four dimensions of Krackhardt (1994) and the
  condition recommended by Everett and Krackhardt (2012).
* `core_periphery()` and `clique_max()` (maximal cliques of any size).

Communities:

* `modularity_score()` (Newman and Girvan, Arenas et al. for directed networks,
  and the LinkRank of Kim, Son and Jeong).
* `leiden()` (Traag et al., 2019), which with `refine = FALSE` is the algorithm
  of Louvain, `leading_eigen()`, `community_greedy()`, `community_label()` and
  `community_betweenness()`.

Inference:

* `cug_test()` for conditional uniform graphs, and `qap_cor()` and `qap_lm()`
  for the QAP correlation and the MRQAP regressions, linear and logistic.

Dynamics and generators:

* `social_influence()` with the rules of assimilation, bounded confidence,
  repulsion and Friedkin-Johnsen, and `threshold_diffusion()`.
* `small_world()` and `pref_attachment()`.

Segregation:

* `segregation()` with the five measures reviewed by Bojanowski and Corten
  (2014).

Overlapping categories (Everett and Borgatti, 2026):

* `alter_composition()`, `alter_heterogeneity()`, `alter_homophily()` (E-I index
  and Yule's Q), `brokerage_roles()` (Gould and Fernandez) and
  `partition_centrality()` for nodes that belong to several categories, given as
  a membership matrix that is made row-stochastic, or as a vector when the
  categories are a partition. `alter_homophily()` takes `similarity` for the
  three definitions of the similarity of two memberships (product, minimum and
  cosine).
* `structural_holes()` with the effective size, efficiency and constraint of
  Burt for every node of a valued or directed network, computed within the ego
  networks (as UCINET) or the whole network (as igraph), and with the option of
  treating the alters of the same category as redundant (`B` and `beta`).
* The results agree with Tables 1, 3, 4, 5, 6 and 10 of the article, with
  `sna::brokerage()` and `igraph::constraint()`. Tables 8 and 9 are reproduced
  within rounding when Chuck spends 12 of 51 hours on the third task instead of
  11 of 44 as printed in Table 2, which suggests that they were computed with
  those hours.
* The dataset `campnet`, the Camp 92 network with the gender and role of each
  person.

Citation networks:

* `traversal_weights()` with the search path count (SPC), the search path link
  count (SPLC) and the search path node pair (SPNP) of each arc, and the
  weighted in-degree and out-degree of each paper (Kuan, 2020). The weights
  agree with the published values of Liu and Lu (2012, Fig. 1) and Kuan (2020,
  Tables 3 and 4).
* `main_path()` with the global, local and key-route main paths (Liu and Lu,
  2012), `main_path_diag()`, `citation_decay()`, and `dag_check()` and
  `dag_sort()` to remove the cycles of a citation network and to order it.

## Notes

* The documentation of every exported function is checked by
  `dev/audit/03_documentation.R`: that each one says what it returns, that the
  value is not copied from another function, that the arguments of the
  documentation and of the function are the same, that the cross-references
  point to topics that exist, and that every DOI resolves and belongs to the
  reference that cites it. It corrected the value of `ind_rand_matrix()`, which
  said that it returned a dyad census, and made the value of
  `neigh_inclusion()`, `dir_inclusion()` and `pos_dominance()` say what their
  matrices mean.
* Two new vignettes: *Getting started with netmem*, with the standard analysis
  of a network, and *What netmem adds*, with the measures that are not
  available elsewhere (neighbourhood-inclusion dominance, overlapping
  categories, Q-analysis, main paths, fractional counting and hyper-event
  dominance). The vignette *Multilayer networks* uses the new
  `multiplex_census()` and corrects two references.
* netmem no longer imports igraph. `k_core()` and `zone_sample()` were the
  only functions that used it. igraph is in Suggests, for the plots of the
  vignette.
* The new functions do not use igraph. Their results are compared with igraph,
  sna, netrankr and netseg in `dev/validation`, which is not part of the
  package.
* `leiden()`, `community_label()` and `core_periphery()` start from random
  partitions, so they need a seed to be reproducible.
* Functions that could mask those of other packages were named to avoid it:
  `clique_max()`, `pos_dominance()`, `indirect_rel()`, `dominance_ranks()`,
  `segregation()` and `centrality_centralization()` (`sna::centralization()`).

# netmem 1.0-3

* Version on CRAN.
