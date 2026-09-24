# ============================================================
# 10_q_analysis.py
# Author: Alejandro Espinosa-Rada, Instituto de Sociología,
#         P. Universidad Católica de Chile
# Last update: 2026-09-18
#
# Called by 10_q_analysis.R. Reads the complexes written by the R
# script and computes the structure vectors and the family
# eccentricity with q-analysis (Smirnov et al., 2025), which needs
# Python 3.8 to 3.10:
#   uv venv --python 3.10 q310
#   uv pip install --python q310/bin/python git+https://github.com/pakrentos/q-analysis
#
# Output (the folder given in QDIR): python.json
# ============================================================

import glob, os, json
import numpy as np
import pandas as pd
from q_analysis.simplicial_complex import SimplicialComplex

folder = os.environ["QDIR"]
out = {}
for f in sorted(glob.glob(os.path.join(folder, "*.csv"))):
    M = pd.read_csv(f, index_col=0).values
    if "graph" in f:
        sc = SimplicialComplex.from_adjacency_matrix(M)
        simplices = [sorted(s) for s in sc.simplices]
    else:
        simplices = [list(np.nonzero(row)[0]) for row in M]
        sc = SimplicialComplex(simplices)
    out[os.path.basename(f)] = {
        "FSV": sc.first_structure_vector().values.tolist(),
        "SSV": sc.second_structure_vector().values.tolist(),
        "TSV": sc.third_structure_vector().values.tolist(),
        "ecc": [sc.family_eccentricity(i) for i in range(sc.num_simplices)],
        "simplices": [[int(v) for v in s] for s in simplices],
    }
json.dump(out, open(os.path.join(folder, "python.json"), "w"))
