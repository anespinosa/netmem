# Validation of the new functions

Every function added to netmem is compared here with an implementation that
already existed, or with a result that can be derived by hand. The scripts are
not part of the package: they are run by hand, and `dev/` is in
`.Rbuildignore`, so it is in the repository but never in the tarball. See
`dev/README.md` for the map of the folder.

Run them all with:

```r
source(here::here("dev", "validation", "00_run_all.R"))
```

Each script prints a table where `agreements` should equal `comparisons`.

| Script | What it checks | Compared with |
|---|---|---|
| `01_centrality.R` | closeness, betweenness, eigenvector, Katz, Bonacich power, PageRank, centralization, distances, diameter | igraph |
| `02_dominance.R` | the nine directed criteria, neighbourhood inclusion, comparable pairs, preserved order, indirect relations, positional dominance, rank intervals, hyper-event dominance | Definition 1 of Marmulla and Brandes (2026) written with sets, netrankr, and the loop of `analysis_dominance/010dominance_coauthor_core_tau1.R` |
| `03_structure.R` | Krackhardt's dimensions, maximal cliques, core-periphery, CONCOR, block densities, regular equivalence | sna, igraph, every possible partition, and structures with a known answer |
| `04_inference.R` | the random networks of the CUG test, the coefficients of the QAP regressions | sna (`netlm`, `netlogit`, `cug.test`, `qaptest`) |
| `05_community.R` | modularity, Leiden, Louvain, leading eigenvector, fast greedy, edge betweenness | igraph |
| `06_dynamics.R` | the three rules of social influence, Friedkin-Johnsen, threshold diffusion, the two generators | `h3_flache_cohesion.R` and analytical results |
| `07_segregation_triads.R` | the five segregation measures, and the triad census under U\|MAN | netseg, Wasserman and Faust (1994: 582-583), and 20,000 networks drawn from the U\|MAN distribution |
| `08_existing_functions.R` | the functions that already existed: density, degree, k-core, reciprocity, censuses, shared partners, the E-I index, the mixing matrix, Blau's index, projections, compound relations, edge lists, signed centrality, Moran's I and geographic distances | igraph, sna, ergm, netseg, signnet, ape, geosphere and formulas by hand |
| `09_main_path_constraint.R` | the traversal weights (SPC, SPLC, SPNP) against Liu and Lu (2012, Fig. 1), Kuan (2020, Table 3) and the enumeration of every path; `dag_check()` and `dag_sort()`; `k_core()` of directed networks and loops; the directed and valued constraint against Everett and Borgatti (2020, Tables 1 and 2) | igraph and published values |
| `10_q_analysis.R` | `q_analysis()`: the three structure vectors, the family eccentricity and the maximal cliques of the clique complex, on 20 incidence matrices and 20 networks | the Python package q-analysis (Smirnov et al., 2025), called through `10_q_analysis.py` |
| `11_overlapping_categories.R` | alter composition, heterogeneity, homophily, brokerage roles, partitioned betweenness and structural holes with overlapping categories: Tables 3 to 10 of Everett and Borgatti (2026), Tables 5 and 6 with `campnet`, and random networks | the published tables, sna, igraph, `eb_constraint()` and `redundancy()` |

## What is not compared with another package

These functions have no implementation to compare with, so they are only
checked against structures whose answer is known, or against the scripts that
were used in the analyses:

- `hyperevent_dominance()`: the loop of the analysis scripts, on the data of
  the article (`astro_qap_citations/data_hyperevent/ties.Rda`, not in this
  repository): identical matrices for tau = 1, 2 and 3, the 52 and 77 maximal
  authors of Section 6, and the eight values of Table 1 with `max_authors = 20`.
  The scripts and the text of Section 3.5 do not agree on what makes the
  dominance strict, and on which papers of the author close the neighbourhood
  of cited papers, which is why the function has the arguments `strict` and
  `closure_papers`. On the data of the article both closures give the same
  dominance.
- `krackhardt_index(lubness = )`: `sna::lubness` gives lower values than either
  of the two published definitions, so it is not used as a reference.
- `concor()`, `rege()`, `block_density()`, `core_periphery()`: structures with a
  known partition, and, for the core-periphery, every possible partition of
  networks of up to eleven nodes.
- `social_influence()`: the simulation script of the loneliness project.

## Results that depend on the seed

`leiden()`, `community_label()` and `core_periphery()` start from random
partitions, so they need a seed to be reproducible.
