# ============================================================
# 02_inventory.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Lists every exported function, the file where it is defined,
# and whether the tests and the validation scripts call it.
#
# Output (dev/audit/):
#   inventory.csv   one row per exported function
# ============================================================

rm(list = ls())

library(here)

exported <- sub("^export[(](.*)[)]$", "\\1", grep("^export[(]", readLines(here::here("NAMESPACE")), value = TRUE))
tests <- paste(unlist(lapply(list.files(here::here("tests", "testthat"), full.names = TRUE, pattern = "[.]R$"), readLines)), collapse = "\n")
validation <- paste(unlist(lapply(list.files(here::here("dev", "validation"), full.names = TRUE, pattern = "[.]R$"), readLines)), collapse = "\n")
stress <- paste(readLines(here::here("dev", "audit", "01_stress.R")), collapse = "\n")

inventory <- data.frame(fn = exported, file = NA, tests = FALSE, validation = FALSE, stress = FALSE)
for (k in seq_along(exported)) {
  f <- exported[k]
  for (path in list.files(here::here("R"), full.names = TRUE)) {
    if (any(grepl(paste0("^", f, " <- function"), readLines(path)))) inventory$file[k] <- basename(path)
  }
  call <- paste0("(^|[^A-Za-z0-9_.])", f, "\\(")
  inventory$tests[k] <- grepl(call, tests)
  inventory$validation[k] <- grepl(call, validation)
  inventory$stress[k] <- grepl(call, stress)
}
write.csv(inventory, here::here("dev", "audit", "inventory.csv"), row.names = FALSE)

table(tests = inventory$tests, validation = inventory$validation)
inventory[!inventory$tests, ]
