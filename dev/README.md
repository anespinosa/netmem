# Development of netmem

Everything here is for working on the package, and nothing of it is built into
it: `dev/` is in `.Rbuildignore`, so it never reaches CRAN. It is in the
repository, because the vignettes and NEWS point to it as the evidence that the
functions were checked.

| Folder | What it holds |
|---|---|
| `validation/` | The comparison of each function with another package or with the tables of the publication that defines it. `README.md` there has the table of scripts, and `00_run_all.R` runs them all |
| `audit/` | The three scripts of the audit (stress, inventory, documentation) and `audit.md`, which records the bugs found, the decisions taken and the comparison with the multilayer benchmark |
| `submission/` | The tarball built for CRAN. It is ignored by git, and rebuilt for each submission |
| `algorithms.R` | The insertion sort of the teaching material, moved out of the package because nothing used it |

`issue_comments.md` (drafts of the answers to the GitHub issues) and
`netmem_diagnostic.md` (the comparison with sna and igraph) are ignored by git
and stay on your machine.

## Everyday commands

```r
devtools::load_all()                     # the package, without installing it
devtools::test()                         # the 759 tests
testthat::test_file("tests/testthat/test_centrality.R")
roxygen2::roxygenise()                   # documentation and NAMESPACE from the comments
devtools::check()                        # R CMD check
lintr::lint_package()                    # the style set in .lintr
```

The two long checks, which are worth running before a release or after touching
many functions:

```r
source(here::here("dev", "validation", "00_run_all.R"))  # every comparison
```

```bash
Rscript dev/audit/01_stress.R         # every function on awkward inputs
Rscript dev/audit/02_inventory.R      # which functions have tests and validation
Rscript dev/audit/03_documentation.R  # the documentation and the DOIs
```

## Adding a function

1. **Write it** in the file of `R/` of its theme (`centrality.R`, `dominance.R`,
   `cohesive_subgroups.R`, ...), with matrix algebra and explicit loops, and
   without adding a dependency: the package imports only `Matrix` and `stats`.
   Missing values are treated as absent ties, as in the rest of the package:
   `if (any(is.na(A) == TRUE)) A <- ifelse(is.na(A), 0, A)`.
2. **Document it** in the roxygen comments, with `@details` that say what the
   measure is, `@references` with the `\doi{}` of each one, `@author Alejandro
   Espinosa-Rada`, an `@examples` block that runs in a couple of seconds, and a
   `@return` that says what comes back. Then `roxygen2::roxygenise()`.
3. **When a publication and another implementation disagree**, add an argument
   with the alternatives instead of choosing for the user, and say in
   `@details` what each one does.
4. **Test it** in `tests/testthat/test_<theme>.R`. The values should come from
   the tables of a publication, from another package, or from a case worked by
   hand; never from the function itself.
5. **Compare it** in a script of `dev/validation/`, when another package
   computes the same thing.
6. **Add it** to `dev/audit/01_stress.R`, so it is called on the awkward inputs,
   and to `_pkgdown.yml`, `README.Rmd` (then knit it) and `NEWS.md`.

## Releasing a version

1. `DESCRIPTION`: version and date. `NEWS.md`: a section for the version,
   starting with the changes that give different results than the previous one.
2. Run the tests, the validation scripts and the three audit scripts.
3. Build the tarball and check it as CRAN does:

   ```bash
   cd dev/submission && R CMD build ../.. && R CMD check --as-cran netmem_1.1-0.tar.gz
   ```

4. Check it on the other platforms: `devtools::check_win_devel()` answers by
   email, and the macOS builder needs the address given explicitly on this
   network (see the note in `dev/submission`, or use
   `curl --resolve mac.r-project.org:443:169.60.149.197 -F "pkgfile=@netmem_1.1-0.tar.gz" https://mac.r-project.org/macbuilder/v1/submit`).
5. Write `cran-comments.md` (ignored by git) with the environments, the notes
   and why each one is not a problem.
6. Submit the tarball at <https://cran.r-project.org/submit.html>, with your
   name and the maintainer address. CRAN sends a confirmation email that has to
   be answered.
7. Merge the branch into `main`, which publishes the website with the
   vignettes, and tag the version.
