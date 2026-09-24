# Changelog

## netmem 1.1-0

### Changes that affect results

These fixes change the numbers that some functions returned. Code
written for version 1.0-3 runs without errors, but gives different
results:

- [`count_geodesics()`](https://anespinosa.github.io/netmem/reference/distances.md)
  counted only some of the geodesics between two nodes. A node that had
  already been reached did not add the geodesics of the other nodes at
  the same distance, so the counts were too low. The counts now agree
  with `sna::geodist()`.

- [`wall_distances()`](https://anespinosa.github.io/netmem/reference/distances.md)
  returned a single path for each node instead of one path for each pair
  of nodes. It now returns `fromTo[[i]][[j]]` and `toFrom[[i]][[j]]` for
  every pair.

- [`wlocal_distances()`](https://anespinosa.github.io/netmem/reference/distances.md)
  and
  [`wall_distances()`](https://anespinosa.github.io/netmem/reference/distances.md)
  now use Dijkstra’s algorithm, as the documentation said. Before they
  enumerated every simple path, which did not finish on networks of a
  dozen nodes. Between paths of the same length, the path returned might
  differ from the one returned before.

- [`triad_uman()`](https://anespinosa.github.io/netmem/reference/triad_uman.md):
  the covariance between the counts of 201 and 102 was positive, and
  should have been negative. The term of the two triads that share a
  dyad used `m + n + 4` where the combinatorics give `m + n - 4`. The
  covariance matrix now adds up to zero, as it must because the number
  of triads is fixed.

- [`triad_uman()`](https://anespinosa.github.io/netmem/reference/triad_uman.md)
  no longer returns the element `z_test` by default. It tested the sum
  of the sixteen counts, which is always `choose(g, 3)`, so it was `NaN`
  for every network. The new argument `l` tests a linear combination of
  the census (Wasserman and Faust, 1994: 583); with `ztest = TRUE` and
  no `l`, the function returns the data frame with the columns `Z` and
  `P`. The covariance matrix is now returned symmetric instead of
  triangular.

- [`eb_constraint()`](https://anespinosa.github.io/netmem/reference/eb_constraint.md)
  failed for egos with a single alter, and for egos whose alters are not
  tied to each other. It now returns the constraint in both cases, and
  gives an informative error for isolates.

- [`components_id()`](https://anespinosa.github.io/netmem/reference/components_id.md)
  did not return the components of a directed network. It grouped the
  nodes that reach the same nodes, so a chain of citations gave as many
  components as nodes. It now returns the weak components, which for
  undirected networks are the ones it returned before, and takes the
  arguments `mode = "strong"` and `bipartite`.

- [`edgelist_to_matrix()`](https://anespinosa.github.io/netmem/reference/edgelist_to_matrix.md)
  placed some ties in the wrong direction, depending on the order in
  which the nodes appeared in the edge list, and dropped the ties of a
  node with itself. The names are now matched one by one, and
  `loops = TRUE` keeps the diagonal. It also returned a vector instead
  of a matrix when the result had a single row or column.

- [`mix_matrix()`](https://anespinosa.github.io/netmem/reference/mix_matrix.md)
  counted only the ties of the upper triangle of the matrix, so for a
  directed network with 15 arcs it returned a table adding up to 5, and
  it dropped the row or the column of a group that never sent or
  received a tie, which made the table lose its shape. The mixing matrix
  is now built from the ties between every pair of groups.

- [`ei_index()`](https://anespinosa.github.io/netmem/reference/ei_index.md)
  ignored the attribute unless `mixed = FALSE` was given, and with its
  default arguments it returned one for any network without loops. The
  attribute now takes precedence, and the index agrees with
  `netseg::ei()` for directed and undirected networks.

- [`trans_matrix()`](https://anespinosa.github.io/netmem/reference/trans_matrix.md)
  failed with “the condition has length \> 1” whenever a node belonged
  to more than one transitive triple, which is the usual case in an
  undirected network with a triangle. It now marks the nodes of every
  transitive triple.

- [`triad_uman()`](https://anespinosa.github.io/netmem/reference/triad_uman.md)
  failed on a network without ties, because the matrix was stored in a
  class that cannot be modified.

- [`multilevel_degree()`](https://anespinosa.github.io/netmem/reference/multilevel_degree.md)
  returned wrong values for the nodes of the third level when `B2` was
  given without `B3`: the rows `k1, k2, ...` of the column `multilevel`
  held the degrees of the nodes of the second level. They now count the
  ties of each node within the third level and with the second level,
  which is the value of the same nodes in `high_multilevel`, and the
  documentation describes what every column counts.

- [`gen_degree()`](https://anespinosa.github.io/netmem/reference/gen_degree.md)
  with `weighted = TRUE` removed the loops from the degree but not from
  the strength when `loops = FALSE`, and symmetrized the ties but not
  the weights when `digraph = FALSE`. The weights now follow the ties in
  both cases.

- [`gen_density()`](https://anespinosa.github.io/netmem/reference/gen_density.md)
  removed the diagonal of two-mode matrices, which holds real ties, so
  with the default `loops = FALSE` the density of an incidence matrix
  was too low (its own example gave 0.33 instead of 0.5). For a directed
  matrix in a list (`multilayer = TRUE`) it used only the lower
  triangle, and with `directed = FALSE` it kept the ties of the upper
  triangle instead of the underlying graph. With `loops = TRUE` the
  diagonal is now counted among the possible ties. The results agree
  with
  [`igraph::edge_density()`](https://r.igraph.org/reference/edge_density.html).

- [`eb_constraint()`](https://anespinosa.github.io/netmem/reference/eb_constraint.md)
  chose the maximum constraint by the size of the ego network including
  ego, so an ego with seven alters got the maximum of the shadow ego
  network (0.486) instead of the complete one (0.493, Everett and
  Borgatti, 2020: Table 1), and a normalization that was too high. The
  maximum is now the larger of the two. An ego with a single alter has a
  normalization of one, as in Everett and Borgatti (2020: Eq. 5),
  instead of `NaN`.

- [`q_analysis()`](https://anespinosa.github.io/netmem/reference/q_analysis.md)
  reported only the dimensions q that some simplex had exactly, so the
  levels at which components merge were missing: two 4-simplices that
  share a face of dimension 2 were never reported as 2-connected. It now
  reports every q from the largest dimension down to 0. A network was
  turned into a complex of its triangles and all its edges, so a clique
  of four nodes became four triangles; the simplices are now the maximal
  cliques. A square incidence matrix, which stopped the function, is now
  accepted. The function returns a list with the simplices, the table of
  the structure vectors, the components at each q and the
  eccentricities; the argument `dimensions` is kept but no longer
  needed. The values agree with Freeman (1980) and with the Python
  package of Smirnov et al. (2025).

- [`simplicial_complexes()`](https://anespinosa.github.io/netmem/reference/simplicial_complexes.md)
  built the complex of a network from its triangles and all its edges,
  so a clique of four nodes became four triangles and six edges, and it
  stopped with an error on networks without triangles. The simplices are
  now the maximal cliques, named after their nodes (`a-b-c`) instead of
  numbered. `zero_simplex = TRUE`, now the default, adds the isolated
  nodes as simplices of dimension 0; before it added columns that did
  not correspond to the nodes. The rows are still the nodes and the
  columns the simplices.

- [`k_core()`](https://anespinosa.github.io/netmem/reference/k_core.md)
  of a binary network counted the loops even when `loops = FALSE`, which
  is the default. The loops are now counted only with `loops = TRUE`.
  Without loops the values are the same as before.

- [`zone_sample()`](https://anespinosa.github.io/netmem/reference/zone_sample.md)
  returns adjacency matrices instead of igraph objects. With
  `core = TRUE`, the indicator of the actors at distance one is the
  attribute `core` of each matrix. The nodes and the ties are the same
  as before.

- An audit of every exported function found further errors, recorded in
  `dev/audit/audit.md` with how each was verified:

  - [`multiplex_census()`](https://anespinosa.github.io/netmem/reference/multiplex_census.md)
    added counts of the two networks instead of counting the triples of
    each joint configuration (two empty networks gave 5 of 10 triples).
    It now returns the classes of Figure 12 of Espinosa-Rada (2021),
    named by the type of the directed triad and the position of the
    undirected edges (e.g. `021U_102ac`); `merge = "overlap"` merges the
    classes that give the same overlapped triad.
  - `mixed_census(quad = TRUE)`: the class “201” multiplied two counts
    instead of adding them.
  - [`kp_reciprocity()`](https://anespinosa.github.io/netmem/reference/kp_reciprocity.md)
    used the number of arcs minus twice the mutual dyads instead of the
    number of arcs for free choices.
  - [`posneg_index()`](https://anespinosa.github.io/netmem/reference/posneg_index.md):
    `select = "all"` returned the out-index and `"out"` the all-index.
    They now agree with `signnet::pn_index()`.
  - [`k_core()`](https://anespinosa.github.io/netmem/reference/k_core.md)
    with `weighted = TRUE` or `multilevel = TRUE` returned the round in
    which a node was removed instead of its core value (a path gave 1 0
    1).
  - With `digraph = FALSE`,
    [`gen_degree()`](https://anespinosa.github.io/netmem/reference/gen_degree.md),
    [`eb_constraint()`](https://anespinosa.github.io/netmem/reference/eb_constraint.md),
    [`redundancy()`](https://anespinosa.github.io/netmem/reference/redundancy.md),
    [`clique_table()`](https://anespinosa.github.io/netmem/reference/clique_table.md),
    [`dyad_triad_table()`](https://anespinosa.github.io/netmem/reference/dyad_triad_table.md),
    [`struc_balance()`](https://anespinosa.github.io/netmem/reference/struc_balance.md)
    and the signed
    [`eigenvector_centrality()`](https://anespinosa.github.io/netmem/reference/eigenvector_centrality.md)
    copied the upper triangle of the matrix over the lower one, so the
    ties present only in the lower triangle were lost and the results
    depended on the order of the nodes. They now use the underlying
    graph.
  - `edgelist_to_matrix(digraph = FALSE)` dropped the edges listed as
    (b, a); `matrix_to_edgelist(valued = TRUE)` dropped ties with
    non-integer values and listed undirected ties twice;
    [`matrix_adjlist()`](https://anespinosa.github.io/netmem/reference/matrix_adjlist.md)
    dropped ties below 1.
  - `adj_to_matrix(type = "adjacency")` returned one row per line of the
    list instead of one per node.
  - [`meta_matrix()`](https://anespinosa.github.io/netmem/reference/meta_matrix.md)
    placed the ties between the second and third levels only above the
    diagonal, and dropped them when `B3` was given.
  - [`extract_component()`](https://anespinosa.github.io/netmem/reference/extract_component.md)
    returned wrong matrices when several components had the same size;
    `position` now counts the distinct sizes.
  - [`dist_sim_matrix()`](https://anespinosa.github.io/netmem/reference/dist_sim_matrix.md)
    computed the Hamming distance of two-mode matrices over the wrong
    nodes, failed with rows without ties, and now keeps the names of the
    nodes.

- [`fractional_approach()`](https://anespinosa.github.io/netmem/reference/fractional_approach.md)
  did not compute the networks of Batagelj (2020): its “citation”
  network was a product of the two incidence matrices, and the
  fractional co-citation was normalised on one side only. It now takes
  the citation network between works (`A1`) and, for the citations
  between authors, the authorship matrix (`A2`), with full or fractional
  counting (`fractional`); the fractional bibliographic coupling can be
  made symmetric with the six measures of Batagelj (2020) (`symmetric`).
  Calls with two incidence matrices, as in version 1.0-3, now stop.

- [`dyad_triad_table()`](https://anespinosa.github.io/netmem/reference/dyad_triad_table.md)
  called “Triad201” every pair of neighbours of a node, closed or not,
  and `min` and `max` limited how often a triad was repeated (which
  selected the closed triads). It now returns the `type` of each triad
  (201 for the forbidden triad of Granovetter, 300 when closed, 102 for
  a dyad and 003 for an isolated node) and its `members`, and `min` and
  `max` limit the number of forbidden triads of which a node is the
  centre.

### Other bug fixes

- [`structural_na()`](https://anespinosa.github.io/netmem/reference/structural_na.md)
  warned whenever the labels had more nodes than the matrix, which is
  its purpose. It now warns only when a node of the matrix is not in the
  labels and is dropped.

- [`eb_constraint()`](https://anespinosa.github.io/netmem/reference/eb_constraint.md),
  [`redundancy()`](https://anespinosa.github.io/netmem/reference/redundancy.md)
  and
  [`ego_net()`](https://anespinosa.github.io/netmem/reference/ego_net.md)
  without `ego` failed with “argument is of length zero”; they now ask
  for the name of ego.

- `trans_coef(method = "mean")` always failed with
  `could not find function "local_trans"`.

- [`short_path()`](https://anespinosa.github.io/netmem/reference/distances.md)
  failed when there was no path between the two nodes. It now warns and
  returns `NULL`.

- [`percolation_clique()`](https://anespinosa.github.io/netmem/reference/percolation_clique.md)
  failed when every node belonged to a clique.

- [`power_function()`](https://anespinosa.github.io/netmem/reference/power_function.md)
  reached the limit of nested expressions for large powers, which made
  its own example fail. It now uses a loop instead of recursion.

- [`matrix_to_edgelist()`](https://anespinosa.github.io/netmem/reference/matrix_to_edgelist.md)
  failed on a network without ties. It now returns an empty edge list.

- [`percolation_clique()`](https://anespinosa.github.io/netmem/reference/percolation_clique.md)
  failed when the network had a single clique, and when some nodes did
  not belong to any clique.

- [`redundancy()`](https://anespinosa.github.io/netmem/reference/redundancy.md)
  failed with an obscure message for an isolated ego, and
  [`clique_table()`](https://anespinosa.github.io/netmem/reference/clique_table.md)
  stopped without a message when there were no cliques.

- Several functions failed or never ended on common inputs:
  [`k_core()`](https://anespinosa.github.io/netmem/reference/k_core.md)
  looped forever on a matrix with `NA`, and
  `ind_rand_matrix(type = "edges")` for undirected networks whenever `l`
  was larger than the number of nodes.
  [`bfs_ugraph()`](https://anespinosa.github.io/netmem/reference/distances.md),
  [`count_geodesics()`](https://anespinosa.github.io/netmem/reference/distances.md),
  [`gen_degree()`](https://anespinosa.github.io/netmem/reference/gen_degree.md),
  [`short_path()`](https://anespinosa.github.io/netmem/reference/distances.md),
  [`wall_distances()`](https://anespinosa.github.io/netmem/reference/distances.md),
  [`wlocal_distances()`](https://anespinosa.github.io/netmem/reference/distances.md),
  [`matrix_to_edgelist()`](https://anespinosa.github.io/netmem/reference/matrix_to_edgelist.md),
  [`multiplex_census()`](https://anespinosa.github.io/netmem/reference/multiplex_census.md)
  and
  [`ego_net()`](https://anespinosa.github.io/netmem/reference/ego_net.md)
  now treat `NA` as an absent tie;
  [`adj_to_incidence()`](https://anespinosa.github.io/netmem/reference/adj_to_incidence.md)
  and
  [`edgelist_to_matrix()`](https://anespinosa.github.io/netmem/reference/edgelist_to_matrix.md)
  accept networks without ties;
  [`minmax_overlap()`](https://anespinosa.github.io/netmem/reference/minmax_overlap.md)
  accepts a single row; `co_occurrence(occurrence = FALSE)` no longer
  fails;
  [`ego_net()`](https://anespinosa.github.io/netmem/reference/ego_net.md)
  returns a matrix for an ego with one alter, and
  [`redundancy()`](https://anespinosa.github.io/netmem/reference/redundancy.md)
  no longer calls that ego an isolate.

- [`wall_distances()`](https://anespinosa.github.io/netmem/reference/distances.md)
  and
  [`wlocal_distances()`](https://anespinosa.github.io/netmem/reference/distances.md)
  accept binary matrices, in which every tie has length one.

### Extensions of the existing functions

- [`ind_rand_matrix()`](https://anespinosa.github.io/netmem/reference/ind_rand_matrix.md)
  takes `sparse`, which draws the ties among the cells that the model
  allows and returns a sparse matrix of the `Matrix` package, without
  building the dense one. A network of 10,000 nodes takes 0.2 MB instead
  of 800 MB. It is available for one-mode and two-mode networks with
  `trials = 1`.

- [`edgelist_to_matrix()`](https://anespinosa.github.io/netmem/reference/edgelist_to_matrix.md)
  orders the nodes as in `label` (and `label2` for two-mode networks),
  with the nodes that are not in the labels after them in alphabetical
  order. Before, the nodes were always in alphabetical order, so a
  matrix could not be recovered from its edge list in its own order.

- [`components_id()`](https://anespinosa.github.io/netmem/reference/components_id.md)
  takes `mode` (weak or strong) and `bipartite`, which covers the nodes
  of both modes (issue
  [\#4](https://github.com/anespinosa/netmem/issues/4)).

- [`eigenvector_centrality()`](https://anespinosa.github.io/netmem/reference/eigenvector_centrality.md)
  takes `signed`, for networks with negative ties (Bonacich and
  Lloyd, 2004) (issue
  [\#9](https://github.com/anespinosa/netmem/issues/9)).

- [`trans_coef()`](https://anespinosa.github.io/netmem/reference/trans_coef.md)
  takes `method = "barrat"`, the weighted transitivity of Barrat et
  al. (2004) (issue
  [\#5](https://github.com/anespinosa/netmem/issues/5)).

- [`gen_density()`](https://anespinosa.github.io/netmem/reference/gen_density.md)
  computes the density of weighted networks, which is the average
  strength of the possible ties (issue
  [\#12](https://github.com/anespinosa/netmem/issues/12)).

- [`edgelist_to_matrix()`](https://anespinosa.github.io/netmem/reference/edgelist_to_matrix.md)
  takes `valued`, which reads the value of the ties from a third column,
  and `loops` (issue
  [\#2](https://github.com/anespinosa/netmem/issues/2)).

- [`dist_sim_matrix()`](https://anespinosa.github.io/netmem/reference/dist_sim_matrix.md)
  takes a list of matrices, and compares the nodes across all the
  relations at once (issue
  [\#7](https://github.com/anespinosa/netmem/issues/7)).

- [`q_analysis()`](https://anespinosa.github.io/netmem/reference/q_analysis.md)
  returns the second and third structure vectors (Raj et al., 2024), the
  obstruction vector and the eccentricity of each simplex, with the
  definition of Atkin (1974) or of Johnson (`eccentricity`). A network
  can be analysed through its clique complex or its neighbourhood
  complex (`complex`), with open or closed neighbourhoods (`closed`). It
  is also about a hundred times faster.

- [`simplicial_complexes()`](https://anespinosa.github.io/netmem/reference/simplicial_complexes.md)
  takes `complex = "neighbourhood"` and `closed`, as
  [`q_analysis()`](https://anespinosa.github.io/netmem/reference/q_analysis.md),
  which now builds its complex with it, and `valued` for projections
  that count the shared nodes and simplices.

- [`eb_constraint()`](https://anespinosa.github.io/netmem/reference/eb_constraint.md)
  takes `digraph = TRUE` and `weighted = TRUE`, which used to stop. A
  directed network has the constraint of the valued network `A + t(A)`,
  and the maximum used in the normalization is that of Everett and
  Borgatti (2020: Eq. 6 to 9). The results agree with their Tables 1 and
  2 and with
  [`igraph::constraint()`](https://r.igraph.org/reference/constraint.html)
  on the ego network.

- [`edgelist_to_matrix()`](https://anespinosa.github.io/netmem/reference/edgelist_to_matrix.md)
  takes `rule` for undirected networks: a tie listed in either order
  (`weak`, default) or only in both orders (`strong`), as
  `sna::symmetrize()`.

- [`gen_degree()`](https://anespinosa.github.io/netmem/reference/gen_degree.md)
  and
  [`gen_density()`](https://anespinosa.github.io/netmem/reference/gen_density.md)
  no longer warn that a symmetric matrix is undirected when
  `digraph = TRUE` (or `directed = TRUE`), as the result is the same.

### New functions

- [`supra_adjacency()`](https://anespinosa.github.io/netmem/reference/supra_adjacency.md)
  arranges the layers of a multiplex network in a single matrix of
  actor-layer pairs, with categorical, ordinal or no coupling between
  the layers (De Domenico et al., 2013; Kivela et al., 2014), and
  [`aggregate_layers()`](https://anespinosa.github.io/netmem/reference/aggregate_layers.md)
  reduces the layers to a single matrix by sum, binary or mean
  (Battiston et al., 2014). Centrality:

- [`closeness_centrality()`](https://anespinosa.github.io/netmem/reference/closeness_centrality.md),
  [`betweenness_centrality()`](https://anespinosa.github.io/netmem/reference/betweenness_centrality.md)
  (Brandes’ algorithm),
  [`eigenvector_centrality()`](https://anespinosa.github.io/netmem/reference/eigenvector_centrality.md),
  [`katz_centrality()`](https://anespinosa.github.io/netmem/reference/katz_centrality.md),
  [`bonacich_power()`](https://anespinosa.github.io/netmem/reference/bonacich_power.md),
  [`page_rank_centrality()`](https://anespinosa.github.io/netmem/reference/page_rank_centrality.md)
  and
  [`centrality_centralization()`](https://anespinosa.github.io/netmem/reference/centrality_centralization.md).

- [`geo_distances()`](https://anespinosa.github.io/netmem/reference/geodesics.md)
  and
  [`geo_summary()`](https://anespinosa.github.io/netmem/reference/geodesics.md)
  for the distances, the diameter and the average distance.

Positions and dominance:

- [`neigh_inclusion()`](https://anespinosa.github.io/netmem/reference/neigh_inclusion.md),
  [`set_inclusion()`](https://anespinosa.github.io/netmem/reference/set_inclusion.md),
  [`pareto_dominance()`](https://anespinosa.github.io/netmem/reference/pareto_dominance.md),
  [`dominance_pairs()`](https://anespinosa.github.io/netmem/reference/dominance_pairs.md),
  [`preserved_order()`](https://anespinosa.github.io/netmem/reference/preserved_order.md)
  and
  [`dominance_layers()`](https://anespinosa.github.io/netmem/reference/dominance_layers.md).
- [`hyperevent_dominance()`](https://anespinosa.github.io/netmem/reference/hyperevent_dominance.md)
  for the dominance of authors through the chain author, citing paper,
  cited paper, author (Espinosa-Rada, 2026). With the data of the
  article, it gives the same dominance matrices as the analysis scripts,
  the maximal and dominant authors of Section 6 and the values of
  Table 1. The arguments `strict` and `closure_papers` give the
  alternatives of the text of Section 3.5 where it differs from the
  analysis scripts.
- [`dir_inclusion()`](https://anespinosa.github.io/netmem/reference/dir_inclusion.md)
  with the nine directed neighbourhood-inclusion criteria of Marmulla
  and Brandes (2026).
- [`pos_dominance()`](https://anespinosa.github.io/netmem/reference/pos_dominance.md),
  [`indirect_rel()`](https://anespinosa.github.io/netmem/reference/indirect_rel.md)
  and
  [`dominance_ranks()`](https://anespinosa.github.io/netmem/reference/dominance_ranks.md).

Roles, positions and macro structure:

- [`block_density()`](https://anespinosa.github.io/netmem/reference/block_density.md),
  [`concor()`](https://anespinosa.github.io/netmem/reference/concor.md)
  and [`rege()`](https://anespinosa.github.io/netmem/reference/rege.md).
- [`krackhardt_index()`](https://anespinosa.github.io/netmem/reference/krackhardt_index.md)
  with the four dimensions of Krackhardt (1994) and the condition
  recommended by Everett and Krackhardt (2012).
- [`core_periphery()`](https://anespinosa.github.io/netmem/reference/core_periphery.md)
  and
  [`clique_max()`](https://anespinosa.github.io/netmem/reference/clique_max.md)
  (maximal cliques of any size).

Communities:

- [`modularity_score()`](https://anespinosa.github.io/netmem/reference/modularity_score.md)
  (Newman and Girvan, Arenas et al. for directed networks, and the
  LinkRank of Kim, Son and Jeong).
- [`leiden()`](https://anespinosa.github.io/netmem/reference/leiden.md)
  (Traag et al., 2019), which with `refine = FALSE` is the algorithm of
  Louvain,
  [`leading_eigen()`](https://anespinosa.github.io/netmem/reference/leading_eigen.md),
  [`community_greedy()`](https://anespinosa.github.io/netmem/reference/communities.md),
  [`community_label()`](https://anespinosa.github.io/netmem/reference/communities.md)
  and
  [`community_betweenness()`](https://anespinosa.github.io/netmem/reference/communities.md).

Inference:

- [`cug_test()`](https://anespinosa.github.io/netmem/reference/cug_test.md)
  for conditional uniform graphs, and
  [`qap_cor()`](https://anespinosa.github.io/netmem/reference/qap_cor.md)
  and
  [`qap_lm()`](https://anespinosa.github.io/netmem/reference/qap_lm.md)
  for the QAP correlation and the MRQAP regressions, linear and
  logistic.

Dynamics and generators:

- [`social_influence()`](https://anespinosa.github.io/netmem/reference/social_influence.md)
  with the rules of assimilation, bounded confidence, repulsion and
  Friedkin-Johnsen, and
  [`threshold_diffusion()`](https://anespinosa.github.io/netmem/reference/threshold_diffusion.md).
- [`small_world()`](https://anespinosa.github.io/netmem/reference/small_world.md)
  and
  [`pref_attachment()`](https://anespinosa.github.io/netmem/reference/pref_attachment.md).

Segregation:

- [`segregation()`](https://anespinosa.github.io/netmem/reference/segregation.md)
  with the five measures reviewed by Bojanowski and Corten (2014).

Overlapping categories (Everett and Borgatti, 2026):

- [`alter_composition()`](https://anespinosa.github.io/netmem/reference/alter_composition.md),
  [`alter_heterogeneity()`](https://anespinosa.github.io/netmem/reference/alter_heterogeneity.md),
  [`alter_homophily()`](https://anespinosa.github.io/netmem/reference/alter_homophily.md)
  (E-I index and Yule’s Q),
  [`brokerage_roles()`](https://anespinosa.github.io/netmem/reference/brokerage_roles.md)
  (Gould and Fernandez) and
  [`partition_centrality()`](https://anespinosa.github.io/netmem/reference/partition_centrality.md)
  for nodes that belong to several categories, given as a membership
  matrix that is made row-stochastic, or as a vector when the categories
  are a partition.
  [`alter_homophily()`](https://anespinosa.github.io/netmem/reference/alter_homophily.md)
  takes `similarity` for the three definitions of the similarity of two
  memberships (product, minimum and cosine).
- [`structural_holes()`](https://anespinosa.github.io/netmem/reference/structural_holes.md)
  with the effective size, efficiency and constraint of Burt for every
  node of a valued or directed network, computed within the ego networks
  (as UCINET) or the whole network (as igraph), and with the option of
  treating the alters of the same category as redundant (`B` and
  `beta`).
- The results agree with Tables 1, 3, 4, 5, 6 and 10 of the article,
  with `sna::brokerage()` and
  [`igraph::constraint()`](https://r.igraph.org/reference/constraint.html).
  Tables 8 and 9 are reproduced within rounding when Chuck spends 12 of
  51 hours on the third task instead of 11 of 44 as printed in Table 2,
  which suggests that they were computed with those hours.
- The dataset `campnet`, the Camp 92 network with the gender and role of
  each person.

Citation networks:

- [`traversal_weights()`](https://anespinosa.github.io/netmem/reference/traversal_weights.md)
  with the search path count (SPC), the search path link count (SPLC)
  and the search path node pair (SPNP) of each arc, and the weighted
  in-degree and out-degree of each paper (Kuan, 2020). The weights agree
  with the published values of Liu and Lu (2012, Fig. 1) and Kuan (2020,
  Tables 3 and 4).
- [`main_path()`](https://anespinosa.github.io/netmem/reference/main_path.md)
  with the global, local and key-route main paths (Liu and Lu, 2012),
  [`main_path_diag()`](https://anespinosa.github.io/netmem/reference/main_path_diag.md),
  [`citation_decay()`](https://anespinosa.github.io/netmem/reference/citation_decay.md),
  and
  [`dag_check()`](https://anespinosa.github.io/netmem/reference/dag.md)
  and
  [`dag_sort()`](https://anespinosa.github.io/netmem/reference/dag.md)
  to remove the cycles of a citation network and to order it.

### Notes

- The documentation of every exported function is checked by
  `dev/audit/03_documentation.R`: that each one says what it returns,
  that the value is not copied from another function, that the arguments
  of the documentation and of the function are the same, that the
  cross-references point to topics that exist, and that every DOI
  resolves and belongs to the reference that cites it. It corrected the
  value of
  [`ind_rand_matrix()`](https://anespinosa.github.io/netmem/reference/ind_rand_matrix.md),
  which said that it returned a dyad census, and made the value of
  [`neigh_inclusion()`](https://anespinosa.github.io/netmem/reference/neigh_inclusion.md),
  [`dir_inclusion()`](https://anespinosa.github.io/netmem/reference/dir_inclusion.md)
  and
  [`pos_dominance()`](https://anespinosa.github.io/netmem/reference/pos_dominance.md)
  say what their matrices mean.
- Two new vignettes: *Getting started with netmem*, with the standard
  analysis of a network, and *What netmem adds*, with the measures that
  are not available elsewhere (neighbourhood-inclusion dominance,
  overlapping categories, Q-analysis, main paths, fractional counting
  and hyper-event dominance). The vignette *Multilayer networks* uses
  the new
  [`multiplex_census()`](https://anespinosa.github.io/netmem/reference/multiplex_census.md)
  and corrects two references.
- netmem no longer imports igraph.
  [`k_core()`](https://anespinosa.github.io/netmem/reference/k_core.md)
  and
  [`zone_sample()`](https://anespinosa.github.io/netmem/reference/zone_sample.md)
  were the only functions that used it. igraph is in Suggests, for the
  plots of the vignette.
- The new functions do not use igraph. Their results are compared with
  igraph, sna, netrankr and netseg in `dev/validation`, which is not
  part of the package.
- [`leiden()`](https://anespinosa.github.io/netmem/reference/leiden.md),
  [`community_label()`](https://anespinosa.github.io/netmem/reference/communities.md)
  and
  [`core_periphery()`](https://anespinosa.github.io/netmem/reference/core_periphery.md)
  start from random partitions, so they need a seed to be reproducible.
- Functions that could mask those of other packages were named to avoid
  it:
  [`clique_max()`](https://anespinosa.github.io/netmem/reference/clique_max.md),
  [`pos_dominance()`](https://anespinosa.github.io/netmem/reference/pos_dominance.md),
  [`indirect_rel()`](https://anespinosa.github.io/netmem/reference/indirect_rel.md),
  [`dominance_ranks()`](https://anespinosa.github.io/netmem/reference/dominance_ranks.md),
  [`segregation()`](https://anespinosa.github.io/netmem/reference/segregation.md)
  and
  [`centrality_centralization()`](https://anespinosa.github.io/netmem/reference/centrality_centralization.md)
  (`sna::centralization()`).

## netmem 1.0-3

CRAN release: 2026-04-16

- Version on CRAN.
