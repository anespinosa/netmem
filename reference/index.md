# Package index

## Utilities

Read, transform and describe the matrices

- [`matrix_report()`](https://anespinosa.github.io/netmem/reference/matrix_report.md)
  : Matrix report
- [`matrix_adjlist()`](https://anespinosa.github.io/netmem/reference/matrix_adjlist.md)
  : Transform a matrix to an adjacency list
- [`matrix_projection()`](https://anespinosa.github.io/netmem/reference/matrix_projection.md)
  : Unipartite projections
- [`matrix_to_edgelist()`](https://anespinosa.github.io/netmem/reference/matrix_to_edgelist.md)
  : Transform a square matrix to an edge-list
- [`edgelist_to_matrix()`](https://anespinosa.github.io/netmem/reference/edgelist_to_matrix.md)
  : Transform an edgelist to a matrix
- [`adj_to_matrix()`](https://anespinosa.github.io/netmem/reference/adj_to_matrix.md)
  : Transform an adjacency list into a matrix
- [`adj_to_incidence()`](https://anespinosa.github.io/netmem/reference/adj_to_incidence.md)
  : Convert an Adjacency Matrix to an Incidence Matrix
- [`expand_matrix()`](https://anespinosa.github.io/netmem/reference/expand_matrix.md)
  : Expand Matrix
- [`extract_component()`](https://anespinosa.github.io/netmem/reference/extract_component.md)
  : Extract components
- [`cumulativeSumMatrices()`](https://anespinosa.github.io/netmem/reference/cumulativeSumMatrices.md)
  : Cumulative sum of matrices
- [`power_function()`](https://anespinosa.github.io/netmem/reference/power_function.md)
  : Power matrix
- [`perm_matrix()`](https://anespinosa.github.io/netmem/reference/perm_matrix.md)
  : Permutation matrix
- [`perm_label()`](https://anespinosa.github.io/netmem/reference/perm_label.md)
  : Permute labels of a matrix
- [`minmax_overlap()`](https://anespinosa.github.io/netmem/reference/minmax_overlap.md)
  : Minimum/maximum overlap
- [`hypergraph()`](https://anespinosa.github.io/netmem/reference/hypergraph.md)
  : Hypergraphs
- [`simplicial_complexes()`](https://anespinosa.github.io/netmem/reference/simplicial_complexes.md)
  : Simplicial complexes
- [`structural_na()`](https://anespinosa.github.io/netmem/reference/structural_na.md)
  : Structural Missing Data
- [`ego_net()`](https://anespinosa.github.io/netmem/reference/ego_net.md)
  : Ego network
- [`zone_sample()`](https://anespinosa.github.io/netmem/reference/zone_sample.md)
  : Zone-2 sampling from second-mode

## Multilevel and multiplex networks

Matrices of several levels or several relations analysed together

- [`meta_matrix()`](https://anespinosa.github.io/netmem/reference/meta_matrix.md)
  : Meta matrix for multilevel networks
- [`supra_adjacency()`](https://anespinosa.github.io/netmem/reference/supra_adjacency.md)
  : Supra-adjacency matrix
- [`aggregate_layers()`](https://anespinosa.github.io/netmem/reference/aggregate_layers.md)
  : Aggregation of the layers
- [`mixed_census()`](https://anespinosa.github.io/netmem/reference/mixed_census.md)
  : Multilevel triad and quadrilateral census
- [`multiplex_census()`](https://anespinosa.github.io/netmem/reference/multiplex_census.md)
  : Multiplex triad census
- [`k_core()`](https://anespinosa.github.io/netmem/reference/k_core.md)
  : Generalized k-core

## Centrality

How central every node is, and how centralized the network is

- [`gen_degree()`](https://anespinosa.github.io/netmem/reference/gen_degree.md)
  : Generalized degree
- [`multilevel_degree()`](https://anespinosa.github.io/netmem/reference/multilevel_degree.md)
  : Degree centrality for multilevel networks
- [`closeness_centrality()`](https://anespinosa.github.io/netmem/reference/closeness_centrality.md)
  : Closeness centrality
- [`betweenness_centrality()`](https://anespinosa.github.io/netmem/reference/betweenness_centrality.md)
  : Betweenness centrality
- [`eigenvector_centrality()`](https://anespinosa.github.io/netmem/reference/eigenvector_centrality.md)
  : Eigenvector centrality
- [`katz_centrality()`](https://anespinosa.github.io/netmem/reference/katz_centrality.md)
  : Katz centrality
- [`bonacich_power()`](https://anespinosa.github.io/netmem/reference/bonacich_power.md)
  : Bonacich power centrality
- [`page_rank_centrality()`](https://anespinosa.github.io/netmem/reference/page_rank_centrality.md)
  : PageRank centrality
- [`centrality_centralization()`](https://anespinosa.github.io/netmem/reference/centrality_centralization.md)
  : Centralization
- [`partition_centrality()`](https://anespinosa.github.io/netmem/reference/partition_centrality.md)
  : Partition of centrality by category

## Distances and paths

- [`geo_distances()`](https://anespinosa.github.io/netmem/reference/geodesics.md)
  [`geo_summary()`](https://anespinosa.github.io/netmem/reference/geodesics.md)
  : Geodesic distances
- [`bfs_ugraph()`](https://anespinosa.github.io/netmem/reference/distances.md)
  [`count_geodesics()`](https://anespinosa.github.io/netmem/reference/distances.md)
  [`short_path()`](https://anespinosa.github.io/netmem/reference/distances.md)
  [`wlocal_distances()`](https://anespinosa.github.io/netmem/reference/distances.md)
  [`wall_distances()`](https://anespinosa.github.io/netmem/reference/distances.md)
  : Path distances
- [`compound_relation()`](https://anespinosa.github.io/netmem/reference/compound_relation.md)
  : Relational composition

## Positions and dominance

Neighbourhood inclusion and the partial rankings it induces

- [`neigh_inclusion()`](https://anespinosa.github.io/netmem/reference/neigh_inclusion.md)
  : Neighbourhood inclusion
- [`dir_inclusion()`](https://anespinosa.github.io/netmem/reference/dir_inclusion.md)
  : Neighbourhood inclusion in directed networks
- [`pos_dominance()`](https://anespinosa.github.io/netmem/reference/pos_dominance.md)
  : Positional dominance on indirect relations
- [`indirect_rel()`](https://anespinosa.github.io/netmem/reference/indirect_rel.md)
  : Indirect relations
- [`set_inclusion()`](https://anespinosa.github.io/netmem/reference/set_inclusion.md)
  : Set inclusion of neighbourhoods
- [`pareto_dominance()`](https://anespinosa.github.io/netmem/reference/pareto_dominance.md)
  : Pareto dominance
- [`hyperevent_dominance()`](https://anespinosa.github.io/netmem/reference/hyperevent_dominance.md)
  : Hyper-event dominance
- [`dominance_pairs()`](https://anespinosa.github.io/netmem/reference/dominance_pairs.md)
  : Dominance pairs
- [`dominance_layers()`](https://anespinosa.github.io/netmem/reference/dominance_layers.md)
  : Dominance layers
- [`dominance_ranks()`](https://anespinosa.github.io/netmem/reference/dominance_ranks.md)
  : Rank intervals
- [`preserved_order()`](https://anespinosa.github.io/netmem/reference/preserved_order.md)
  : Preserved order

## Roles and positions

- [`block_density()`](https://anespinosa.github.io/netmem/reference/block_density.md)
  : Block densities and image matrix
- [`concor()`](https://anespinosa.github.io/netmem/reference/concor.md)
  : CONCOR
- [`rege()`](https://anespinosa.github.io/netmem/reference/rege.md) :
  Regular equivalence
- [`dist_sim_matrix()`](https://anespinosa.github.io/netmem/reference/dist_sim_matrix.md)
  : Structural similarities
- [`core_periphery()`](https://anespinosa.github.io/netmem/reference/core_periphery.md)
  : Core-periphery structure

## Cohesive subgroups and communities

- [`clique_max()`](https://anespinosa.github.io/netmem/reference/clique_max.md)
  : Maximal cliques
- [`clique_table()`](https://anespinosa.github.io/netmem/reference/clique_table.md)
  : Clique table
- [`percolation_clique()`](https://anespinosa.github.io/netmem/reference/percolation_clique.md)
  : Clique percolation
- [`dyad_triad_table()`](https://anespinosa.github.io/netmem/reference/dyad_triad_table.md)
  : Forbidden triad table
- [`shared_partners()`](https://anespinosa.github.io/netmem/reference/shared_partners.md)
  : Shared partners
- [`q_analysis()`](https://anespinosa.github.io/netmem/reference/q_analysis.md)
  : Q-analysis
- [`leiden()`](https://anespinosa.github.io/netmem/reference/leiden.md)
  : Leiden communities
- [`leading_eigen()`](https://anespinosa.github.io/netmem/reference/leading_eigen.md)
  : Communities with the leading eigenvector
- [`community_greedy()`](https://anespinosa.github.io/netmem/reference/communities.md)
  [`community_label()`](https://anespinosa.github.io/netmem/reference/communities.md)
  [`community_betweenness()`](https://anespinosa.github.io/netmem/reference/communities.md)
  : Communities with agglomeration, label propagation or edge
  betweenness
- [`modularity_score()`](https://anespinosa.github.io/netmem/reference/modularity_score.md)
  : Modularity

## Structure of the network

- [`gen_density()`](https://anespinosa.github.io/netmem/reference/gen_density.md)
  : Generalized density
- [`recip_coef()`](https://anespinosa.github.io/netmem/reference/recip_coef.md)
  : Reciprocity
- [`trans_coef()`](https://anespinosa.github.io/netmem/reference/trans_coef.md)
  : Transitivity
- [`trans_matrix()`](https://anespinosa.github.io/netmem/reference/trans_matrix.md)
  : Transitivity matrix
- [`components_id()`](https://anespinosa.github.io/netmem/reference/components_id.md)
  : Components
- [`krackhardt_index()`](https://anespinosa.github.io/netmem/reference/krackhardt_index.md)
  : Krackhardt's dimensions of informal organisations
- [`dyadic_census()`](https://anespinosa.github.io/netmem/reference/dyadic_census.md)
  : Dyad census

## Ego networks and structural holes

Composition of the alters and brokerage, also with overlapping
categories

- [`eb_constraint()`](https://anespinosa.github.io/netmem/reference/eb_constraint.md)
  : Constraint
- [`redundancy()`](https://anespinosa.github.io/netmem/reference/redundancy.md)
  : Redundancy measures
- [`structural_holes()`](https://anespinosa.github.io/netmem/reference/structural_holes.md)
  : Structural holes
- [`alter_composition()`](https://anespinosa.github.io/netmem/reference/alter_composition.md)
  : Alter composition
- [`alter_heterogeneity()`](https://anespinosa.github.io/netmem/reference/alter_heterogeneity.md)
  : Alter heterogeneity
- [`brokerage_roles()`](https://anespinosa.github.io/netmem/reference/brokerage_roles.md)
  : Brokerage roles

## Homophily and segregation

- [`segregation()`](https://anespinosa.github.io/netmem/reference/segregation.md)
  : Segregation measures
- [`ei_index()`](https://anespinosa.github.io/netmem/reference/ei_index.md)
  : Krackhardt and Stern's E-I index
- [`alter_homophily()`](https://anespinosa.github.io/netmem/reference/alter_homophily.md)
  : Alter homophily
- [`mix_matrix()`](https://anespinosa.github.io/netmem/reference/mix_matrix.md)
  : Mixing matrix
- [`heterogeneity()`](https://anespinosa.github.io/netmem/reference/heterogeneity.md)
  : Blau's and IQV index

## Signed networks

- [`struc_balance()`](https://anespinosa.github.io/netmem/reference/struc_balance.md)
  : Structural balance
- [`posneg_index()`](https://anespinosa.github.io/netmem/reference/posneg_index.md)
  : Positive-negative centrality

## Citation networks

Main path analysis and the weights of the search paths

- [`main_path()`](https://anespinosa.github.io/netmem/reference/main_path.md)
  : Main path extraction from a citation network
- [`main_path_diag()`](https://anespinosa.github.io/netmem/reference/main_path_diag.md)
  : Diagnostics for main path analysis
- [`traversal_weights()`](https://anespinosa.github.io/netmem/reference/traversal_weights.md)
  : Traversal weights for main path analysis
- [`citation_decay()`](https://anespinosa.github.io/netmem/reference/citation_decay.md)
  : Temporal decay weighting for citation edges
- [`dag_check()`](https://anespinosa.github.io/netmem/reference/dag.md)
  [`dag_sort()`](https://anespinosa.github.io/netmem/reference/dag.md) :
  DAG validation and topological ordering for citation networks
- [`fractional_approach()`](https://anespinosa.github.io/netmem/reference/fractional_approach.md)
  : Fractional approach
- [`co_occurrence()`](https://anespinosa.github.io/netmem/reference/co_occurrence.md)
  : Co‐occurrence
- [`bonacich_norm()`](https://anespinosa.github.io/netmem/reference/bonacich_norm.md)
  : Bonacich normalization
- [`jaccard()`](https://anespinosa.github.io/netmem/reference/jaccard.md)
  : Jaccard similarity

## Social influence and diffusion

- [`social_influence()`](https://anespinosa.github.io/netmem/reference/social_influence.md)
  : Social influence
- [`threshold_diffusion()`](https://anespinosa.github.io/netmem/reference/threshold_diffusion.md)
  : Threshold diffusion

## Inference and random networks

- [`cug_test()`](https://anespinosa.github.io/netmem/reference/cug_test.md)
  : Conditional uniform graph test
- [`qap_cor()`](https://anespinosa.github.io/netmem/reference/qap_cor.md)
  : QAP correlation
- [`qap_lm()`](https://anespinosa.github.io/netmem/reference/qap_lm.md)
  : QAP regression
- [`triad_uman()`](https://anespinosa.github.io/netmem/reference/triad_uman.md)
  : Triad census analysis assuming U\|MAN
- [`kp_reciprocity()`](https://anespinosa.github.io/netmem/reference/kp_reciprocity.md)
  : Reciprocity of Katz and Powell
- [`z_arctest()`](https://anespinosa.github.io/netmem/reference/z_arctest.md)
  : Z test of the number of arcs
- [`ind_rand_matrix()`](https://anespinosa.github.io/netmem/reference/ind_rand_matrix.md)
  : Independent random matrix
- [`small_world()`](https://anespinosa.github.io/netmem/reference/small_world.md)
  : Small world network
- [`pref_attachment()`](https://anespinosa.github.io/netmem/reference/pref_attachment.md)
  : Preferential attachment network

## Geographic information

- [`dist_geographic()`](https://anespinosa.github.io/netmem/reference/dist_geographic.md)
  : Geographical distances
- [`spatial_cor()`](https://anespinosa.github.io/netmem/reference/spatial_cor.md)
  : Spatial autocorrelation

## Data

- [`FIFAego`](https://anespinosa.github.io/netmem/reference/FIFAego.md)
  : Ego FIFA
- [`FIFAex`](https://anespinosa.github.io/netmem/reference/FIFAex.md) :
  Outside FIFA
- [`FIFAin`](https://anespinosa.github.io/netmem/reference/FIFAin.md) :
  Inside FIFA
- [`krackhardt_friends`](https://anespinosa.github.io/netmem/reference/krackhardt_friends.md)
  : Krackhardt friends
- [`lazega_lawfirm`](https://anespinosa.github.io/netmem/reference/lazega_lawfirm.md)
  : Lazega law firm
- [`campnet`](https://anespinosa.github.io/netmem/reference/campnet.md)
  : Camp 92 network
