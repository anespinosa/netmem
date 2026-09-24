# Audit of netmem 1.1-0

September 2026. Branch `centrality-dominance`. Every exported function (114)
was checked in four layers.

1. **Stress** (`01_stress.R`): every function on awkward inputs (undirected,
   directed, valued, isolate, empty, two nodes, loop, no names, missing
   value, two-mode), recording errors, warnings, `NaN` and `Inf`; and every
   node-level result checked to follow the nodes when they are relabelled,
   also with `digraph = FALSE` on directed input.
2. **Definitions**: each function read against its reference and compared,
   argument by argument, with another implementation (igraph, sna, netseg,
   netrankr, signnet, ape, geosphere, the Python package q-analysis) or,
   when there is none, with the definition itself: enumeration of every
   triple or path, hand-worked cases, or the tables of the publication.
3. **Documentation** (`03_documentation.R`): every exported function says what
   it returns, in a sentence that is not copied from another function; the
   arguments of the documentation and of the function are the same; the
   cross-references point to topics that exist; and every DOI resolves and
   belongs to the reference that cites it.
4. **Record**: this file, `inventory.csv` (`02_inventory.R`),
   `documentation.csv`, the regression tests in `tests/testthat/test_audit.R`
   and `test_coverage.R`, and the validation scripts in `dev/validation`.

## Result

| | Before | After |
|---|---|---|
| Exported functions with tests | 95 of 112 | 114 of 114 |
| Tests | 568 | 759 |
| Coverage of the tests | 84.9% | 86.1% |
| Problems in the documentation | 25 | 0 |
| Stress calls ending in an obscure error, a crash or a hang | 25 | 0 |
| Node-level results that change when the nodes are relabelled | several | 0 of 125 |

The remaining 59 errors of the stress layer are informative messages
("No label assigned to the rows", "The network has no ties", ...).

## Bugs found and fixed

| Function | Bug | Found by | Verified with |
|---|---|---|---|
| `k_core` | Looped forever on a matrix with `NA` | Stress | Terminates, same as with 0 |
| `k_core` (weighted, multilevel) | Returned the round of removal, not the core value; a path gave 1 0 1 | Definitions | Definition of generalized cores (Batagelj & Zaversnik, 2011) on 15 networks, α = 1 and 0.5; unit weights = binary k-core (15/15) |
| `k_core` (binary) | Counted loops with `loops = FALSE` | Definitions | `igraph::coreness` (20/20 × 7 settings) |
| `bfs_ugraph`, `count_geodesics`, `gen_degree`, `matrix_to_edgelist`, `multiplex_census`, `short_path`, `wall_distances`, `wlocal_distances`, `ego_net` | Crashed on `NA` | Stress | `NA` treated as absent, as in the rest of the package |
| `wall_distances`, `wlocal_distances` | Refused binary matrices | Stress | igraph distances on binary and valued networks (20/20) |
| `short_path` | Obscure error without names | Stress | Informative error |
| `gen_degree`, `eb_constraint`, `redundancy`, `clique_table`, `dyad_triad_table`, `struc_balance`, `eigenvector_centrality` (signed) | With `digraph = FALSE`, copied the upper triangle over the lower one: ties only in the lower triangle were lost and the result depended on the order of the nodes | Definitions | Underlying graph (`pmax`; for signed ties, the largest absolute value, negative on ties); relabelling check |
| `edgelist_to_matrix` | `digraph = FALSE` dropped every edge listed as (b, a); failed on an empty list | Definitions | Round trips on 40 random networks |
| `matrix_to_edgelist` | `valued = TRUE` failed or dropped ties with non-integer or negative values, and listed undirected ties twice; a single edge returned a vector | Definitions | Round trips; igraph edge counts |
| `matrix_adjlist` | Dropped ties below 1 and negative ties; empty without names | Definitions | Hand examples |
| `adj_to_matrix` | `type = "adjacency"` gave one row per line of the list (duplicated nodes) and dropped the alters not in the first column | Definitions | The weighted matrix made binary |
| `adj_to_incidence` | Failed without ties; `directed = FALSE` lost lower-triangle ties; no names | Stress | Hand examples |
| `ego_net` | One alter returned its name instead of a matrix; a loop made ego its own alter | Definitions | Hand examples |
| `redundancy` | An ego with one alter was called an isolate | Definitions | Effective size 1 |
| `minmax_overlap` | Failed with one row | Stress | Hand example |
| `extract_component` | Components of equal size returned wrong matrices and `NULL` | Definitions | Hand examples; `components_id` |
| `meta_matrix` | B2 only above the diagonal; B3 dropped B2 | Definitions | Block structure [[A1,B1,B3ᵀ],[B1ᵀ,A2,B2],[B3,B2ᵀ,A3]] |
| `mixed_census` (quadrilateral) | "201" multiplied two sums instead of adding them | Definitions | Enumeration of every pair × node × node (15/15) |
| `multiplex_census` | Added counts of the two networks instead of counting joint configurations (two empty networks gave 5 of 10 triples, total 70) | Definitions | Classes of Figure 12 (Espinosa-Rada, 2021); margins = sna triad censuses (20/20); invariant to relabelling (20/20) |
| `kp_reciprocity` | L computed as arcs − 2M; ties below 1 dropped; loops counted | Definitions | Wasserman & Faust (1994) formula |
| `posneg_index` | `select = "all"` returned the out-index and `"out"` the all-index | Definitions | `signnet::pn_index` (15/15 × 3 modes) |
| `struc_balance` | Triangles matched by pasted names (possible confusion of "1","12") | Definitions | Enumeration of triangles (18/18) |
| `clique_table`, `dyad_triad_table` | Triads keyed by pasted names without separator | Definitions | Separator; `igraph::count_triangles` (15/15) |
| `dist_sim_matrix` | Two-mode Hamming looped over columns; Jaccard failed on rows without ties; no names | Definitions | `stats::dist` (both orientations) |
| `co_occurrence` | `occurrence = FALSE` failed (object not defined) | Stress | Hand example |
| `jaccard` | 2 × 2 table read by position; name check compared B with B | Definitions | Matrices of only ones |
| `ind_rand_matrix` | `type = "edges"` undirected sampled only n of the n(n−1)/2 cells: looped forever when l > n | Stress | Exactly l ties in every case |
| `gen_density` | Removed the diagonal of two-mode matrices; directed matrices in a list used the lower triangle; `directed = FALSE` did not use the underlying graph | Definitions | `igraph::edge_density` (25/25 × 7 cases) |
| `eb_constraint` | Maximum with 7 alters; names lost | Definitions | Everett & Borgatti (2020), Tables 1 and 2 |
| `q_analysis`, `simplicial_complexes` | Skipped levels; triangles instead of maximal cliques; `zero_simplex` wrong | Definitions | Freeman (1980); Python q-analysis (40/40) |
| `traversal_weights` | SPLC and SPNP were not the published measures | Definitions | Liu & Lu (2012), Kuan (2020), enumeration |

## Decisions taken with the author

1. **`multiplex_census`** (used for the "mixed triad census" of the goodness of
   fit of Espinosa-Rada et al., 2024). Implemented as the classes of Figure 12
   of the thesis up to the symmetries of the directed triad (104 classes, the
   natural extension of the triad census, whose types are the classes of
   triads up to relabelling), with `merge = "overlap"` merging the classes
   that give the same overlapped triad. Figure 12 and Figure A1 of the
   supplement are not fully consistent (listed in the history of this file),
   so the exact grouping of the thesis is not reproduced where it contradicts
   itself; every group of the figure is a sum of the 104 classes.
2. **`fractional_approach`**: rewritten from Batagelj (2020), with the comment
   of Prathap and Mukherjee (2020) on which count each protocol conserves.
   Newman's normalization (Newman 2001, 2004; Perianes-Rodriguez et al. 2016)
   could be added as an option once checked against those papers.
3. **`dyad_triad_table`**: now lists the type of each triad (201 forbidden,
   300 closed, 102 dyad, 003 isolate); `min` and `max` bound the number of
   forbidden triads centred on each node.
4. **`gen_degree`, `gen_density`**: the warning for symmetric matrices was
   removed.
5. **`insertion_sort`**: moved to `dev/`. It was not used by any function of
   the package.

## An outside comparison

netmem is one of the libraries in the benchmark of multilayer libraries of
Panayiotou et al. (2024, *Applied Network Science*, \doi{10.1007/s41109-024-00686-4},
scripts at <https://github.com/giorgospanay/sd-mln-engineering-challenges>).
The paper leaves netmem out of the measurements because it has no native
multilayer structure, and their `netmem-util.R` records three complaints. Two
of them no longer hold:

| Complaint of the benchmark | State in 1.1-0 |
|---|---|
| "Taking absurdly long time to generate network" (`ind_rand_matrix`) | The rewrite of `fixed_ties()` made it about 40 times faster: with 2000 nodes, 2.0 s before and 0.1 s now; with 5000 nodes, 32.7 s and 0.8 s. A case that looped forever was also fixed |
| "plus unnecessary prints" | The prints were removed when 1.0-3 was prepared for CRAN, after they ran the benchmark (August 2023) |
| "Native reading not available", aggregation "not available" | `supra_adjacency()` and `aggregate_layers()` arrange and aggregate the layers. Reading a file is still left to the user |

The tests of `random_networks.R` went from covering half of the file (49.7%) to
86.0%, and they check the properties that each model guarantees.

What remains of the criticism is the memory of dense matrices: 200 MB for 5000
nodes, and their case of 100,000 actor-layer pairs would need about 80 GB.
`ind_rand_matrix(sparse = TRUE)` is a first answer, as it builds a sparse
matrix without the dense one (0.2 MB for 10,000 nodes), and `supra_adjacency()`
takes `sparse` as well, but the measures themselves still work on dense
matrices.

## Verification of each function

Compared with another implementation: `betweenness_centrality`,
`closeness_centrality`, `eigenvector_centrality`, `katz_centrality`,
`bonacich_power`, `page_rank_centrality`, `centrality_centralization`,
`geo_distances`, `geo_summary`, `bfs_ugraph`, `count_geodesics`,
`short_path`, `wall_distances`, `wlocal_distances` (igraph);
`components_id`, `k_core`, `gen_density`, `trans_coef`, `recip_coef`,
`clique_max`, `clique_table`, `constraint` via `structural_holes`
(igraph); `dyadic_census`, `triad_uman`, `cug_test`, `qap_cor`, `qap_lm`,
`brokerage_roles`, `multiplex_census` margins (sna); `ei_index`,
`segregation`, `mix_matrix` (netseg); `pos_dominance`, `indirect_rel`,
`dominance_ranks` (netrankr); `posneg_index`, `eigenvector_centrality`
signed (signnet); `spatial_cor` (ape); `dist_geographic` (geosphere);
`q_analysis`, `simplicial_complexes` (Python q-analysis); `dist_sim_matrix`
(stats::dist); `modularity_score`, `leiden`, `leading_eigen`,
`community_greedy`, `community_label`, `community_betweenness` (igraph);
`krackhardt_index`, `core_periphery`, `concor`, `rege`, `block_density`,
`small_world`, `pref_attachment`, `social_influence`,
`threshold_diffusion` (see `dev/validation`).

Compared with the tables of a publication: `traversal_weights`, `main_path`
(Liu & Lu 2012; Kuan 2020); `eb_constraint` (Everett & Borgatti 2020);
`alter_composition`, `alter_heterogeneity`, `alter_homophily`,
`brokerage_roles`, `partition_centrality`, `structural_holes` (Everett &
Borgatti 2026, Tables 3–10); `hyperevent_dominance` (Espinosa-Rada 2026,
Section 6 and Table 1); `q_analysis` (Freeman 1980); `triad_uman`
(Wasserman & Faust 1994, Table 14.3).

No other implementation; checked against the definition by enumeration or
hand-worked cases: `mixed_census`, `multiplex_census`, `multilevel_degree`,
`k_core` (weighted, multilevel), `zone_sample`, `meta_matrix`,
`percolation_clique`, `struc_balance`, `kp_reciprocity`, `z_arctest`,
`dag_check`, `dag_sort`, `citation_decay`, `main_path_diag`,
`neigh_inclusion`, `dir_inclusion`, `set_inclusion`, `pareto_dominance`,
`dominance_pairs`, `dominance_layers`, `preserved_order`, `compound_relation`,
`bonacich_norm`, `heterogeneity`, `shared_partners`, `trans_matrix`,
`power_function`, `ind_rand_matrix`, `gen_degree`, `hypergraph`,
`co_occurrence`, `jaccard`, `extract_component`, `ego_net`, `redundancy`,
and the conversions (`matrix_to_edgelist`, `edgelist_to_matrix`,
`matrix_adjlist`, `adj_to_matrix`, `adj_to_incidence`, `matrix_projection`,
`minmax_overlap`, `expand_matrix`, `structural_na`, `perm_matrix`,
`perm_label`, `cumulativeSumMatrices`, `matrix_report`).

Checked against the definitions of the publications after the audit:
`fractional_approach` (Batagelj 2020), `dyad_triad_table` (Granovetter 1973).
