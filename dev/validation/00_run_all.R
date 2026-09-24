# ============================================================
# 00_run_all.R
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Runs every validation script of this folder. Each of them prints
# a table in which the agreements should equal the comparisons.
# Requires igraph, sna, netrankr and netseg, and for 10_q_analysis.R
# a Python with q-analysis (see 10_q_analysis.py).
#
# Output: the tables of the scripts, in order
# ============================================================

rm(list = ls())

library(here)

scripts <- list.files(here::here("dev", "validation"), pattern = "^(0[1-9]|[1-9][0-9])_.*\\.R$", full.names = TRUE)

for (script in scripts) {
  cat("\n\n#### ", basename(script), " ####\n\n", sep = "")
  source(script, echo = FALSE, print.eval = TRUE, local = new.env())
}
