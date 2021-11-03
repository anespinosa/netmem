# tips para los reemplazos
# ctrl + f + opcion regex en rstudio
# man[0-9] -> busca cualquier cosa de la forma man1, man2, etc...
# man[0-9][0-9] -> busca cualquier cosa de la forma man11, man12, etc...
# man[0-9][0-9][0-9]_man[0-9][0-9][0-9]b -> reemplaza todos los de la forma
#     man123_man123b, etc

COVAR <- c(
  # ----------- #
  ## 1
  # 012 - 003
  COVAR012_003 <- (-g3 * d1_out$d1p1t012 * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man012_man003b - d2_out$man012_man003) +
    g3 * (g3 - 1) * (d2_out$man012_man003 - (d1_out$d1p1t012 * d1_out$d1p1t003)),
  
  # ----------- #
  ## 2
  # 102 - 003
  COVAR102_003 <- (-g3 * d1_out$d1p1t102 * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man102_man003b - d2_out$man102_man003) +
    g3 * (g3 - 1) * (d2_out$man102_man003 - (d1_out$d1p1t102 * d1_out$d1p1t003)),
  
  # 102 - 012
  COVAR102_012 <- (-g3 * d1_out$d1p1t102 * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man102_man012b - d2_out$man102_man012) +
    g3 * (g3 - 1) * (d2_out$man102_man012 - (d1_out$d1p1t102 * d1_out$d1p1t012)),
  
  # ----------- #
  ## 3
  # 021D - 003
  COVAR021D_003 <- (-g3 * d1_out$d1p1t021D * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man021D_man003b - d2_out$man021D_man003) +
    g3 * (g3 - 1) * (d2_out$man021D_man003 - (d1_out$d1p1t021D * d1_out$d1p1t003)),
  
  # 021D - 012
  COVAR021D_012 <- (-g3 * d1_out$d1p1t021D * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man021D_man012b - d2_out$man021D_man012) +
    g3 * (g3 - 1) * (d2_out$man021D_man012 - (d1_out$d1p1t021D * d1_out$d1p1t012)),
  
  # 021D - 102
  COVAR021D_102 <- (-g3 * d1_out$d1p1t021D * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man021D_man102b - d2_out$man021D_man102) +
    g3 * (g3 - 1) * (d2_out$man021D_man102 - (d1_out$d1p1t021D * d1_out$d1p1t102)),
  
  # ----------- #
  ## 4
  # 021U - 003
  COVAR021U_003 <- (-g3 * d1_out$d1p1t021U * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man021U_man003b - d2_out$man021U_man003) +
    g3 * (g3 - 1) * (d2_out$man021U_man003 - (d1_out$d1p1t021U * d1_out$d1p1t003)),
  
  # 021U - 012
  COVAR021U_012 <- (-g3 * d1_out$d1p1t021U * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man021U_man012b - d2_out$man021U_man012) +
    g3 * (g3 - 1) * (d2_out$man021U_man012 - (d1_out$d1p1t021U * d1_out$d1p1t012)),
  
  # 021U - 102
  COVAR021U_102 <- (-g3 * d1_out$d1p1t021U * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man021U_man102b - d2_out$man021U_man102) +
    g3 * (g3 - 1) * (d2_out$man021U_man102 - (d1_out$d1p1t021U * d1_out$d1p1t102)),
  
  # 021U - 021D
  COVAR021U_021D <- (-g3 * d1_out$d1p1t021U * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man021U_man021Db - d2_out$man021U_man021D) +
    g3 * (g3 - 1) * (d2_out$man021U_man021D - (d1_out$d1p1t021U * d1_out$d1p1t021D)),
  
  # ----------- #
  ## 5
  # 021C - 003
  COVAR021C_003 <- (-g3 * d1_out$d1p1t021C * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man021C_man003b - d2_out$man021C_man003) +
    g3 * (g3 - 1) * (d2_out$man021C_man003 - (d1_out$d1p1t021C * d1_out$d1p1t003)),
  
  # 021C - 012
  COVAR021C_012 <- (-g3 * d1_out$d1p1t021C * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man021C_man012b - d2_out$man021C_man012) +
    g3 * (g3 - 1) * (d2_out$man021C_man012 - (d1_out$d1p1t021C * d1_out$d1p1t012)),
  
  # 021C - 102
  COVAR021C_102 <- (-g3 * d1_out$d1p1t021C * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man021C_man102b - d2_out$man021C_man102) +
    g3 * (g3 - 1) * (d2_out$man021C_man102 - (d1_out$d1p1t021C * d1_out$d1p1t102)),
  
  # 021C - 021D
  COVAR021C_021D <- (-g3 * d1_out$d1p1t021C * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man021C_man021Db - d2_out$man021C_man021D) +
    g3 * (g3 - 1) * (d2_out$man021C_man021D - (d1_out$d1p1t021C * d1_out$d1p1t021D)),
  
  # 021C - 021U
  COVAR021C_021U <- (-g3 * d1_out$d1p1t021C * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man021C_man021Ub - d2_out$man021C_man021U) +
    g3 * (g3 - 1) * (d2_out$man021C_man021U - (d1_out$d1p1t021C * d1_out$d1p1t021U)),
  
  # ----------- #
  ## 6
  # 111D - 003
  COVAR111D_003 <- (-g3 * d1_out$d1p1t111D * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man111D_man003b - d2_out$man111D_man003) +
    g3 * (g3 - 1) * (d2_out$man111D_man003 - (d1_out$d1p1t111D * d1_out$d1p1t003)),
  
  # 111D - 012
  COVAR111D_012 <- (-g3 * d1_out$d1p1t111D * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man111D_man012b - d2_out$man111D_man012) +
    g3 * (g3 - 1) * (d2_out$man111D_man012 - (d1_out$d1p1t111D * d1_out$d1p1t012)),
  
  # 111D - 102
  COVAR111D_102 <- (-g3 * d1_out$d1p1t111D * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man111D_man102b - d2_out$man111D_man102) +
    g3 * (g3 - 1) * (d2_out$man111D_man102 - (d1_out$d1p1t111D * d1_out$d1p1t102)),
  
  # 111D - 021D
  COVAR111D_021D <- (-g3 * d1_out$d1p1t111D * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man111D_man021Db - d2_out$man111D_man021D) +
    g3 * (g3 - 1) * (d2_out$man111D_man021D - (d1_out$d1p1t111D * d1_out$d1p1t021D)),
  
  # 111D - 021U
  COVAR111D_021U <- (-g3 * d1_out$d1p1t111D * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man111D_man021Ub - d2_out$man111D_man021U) +
    g3 * (g3 - 1) * (d2_out$man111D_man021U - (d1_out$d1p1t111D * d1_out$d1p1t021U)),
  
  # 111D - 021C
  COVAR111D_021C <- (-g3 * d1_out$d1p1t111D * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man111D_man021Cb - d2_out$man111D_man021C) +
    g3 * (g3 - 1) * (d2_out$man111D_man021C - (d1_out$d1p1t111D * d1_out$d1p1t021C)),
  
  # ----------- #
  ## 7
  # 111U - 003
  COVAR111U_003 <- (-g3 * d1_out$d1p1t111U * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man111U_man003b - d2_out$man111U_man003) +
    g3 * (g3 - 1) * (d2_out$man111U_man003 - (d1_out$d1p1t111U * d1_out$d1p1t003)),
  
  # 111U - 012
  COVAR111U_012 <- (-g3 * d1_out$d1p1t111U * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man111U_man012b - d2_out$man111U_man012) +
    g3 * (g3 - 1) * (d2_out$man111U_man012 - (d1_out$d1p1t111U * d1_out$d1p1t012)),
  
  # 111U - 102
  COVAR111U_102 <- (-g3 * d1_out$d1p1t111U * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man111U_man102b - d2_out$man111U_man102) +
    g3 * (g3 - 1) * (d2_out$man111U_man102 - (d1_out$d1p1t111U * d1_out$d1p1t102)),
  
  # 111U - 021D
  COVAR111U_021D <- (-g3 * d1_out$d1p1t111U * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man111U_man021Db - d2_out$man111U_man021D) +
    g3 * (g3 - 1) * (d2_out$man111U_man021D - (d1_out$d1p1t111U * d1_out$d1p1t021D)),
  
  # 111U - 021U
  COVAR111U_021U <- (-g3 * d1_out$d1p1t111U * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man111U_man021Ub - d2_out$man111U_man021U) +
    g3 * (g3 - 1) * (d2_out$man111U_man021U - (d1_out$d1p1t111U * d1_out$d1p1t021U)),
  
  # 111U - 021C
  COVAR111U_021C <- (-g3 * d1_out$d1p1t111U * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man111U_man021Cb - d2_out$man111U_man021C) +
    g3 * (g3 - 1) * (d2_out$man111U_man021C - (d1_out$d1p1t111U * d1_out$d1p1t021C)),
  
  # 111U - 111D
  COVAR111U_111D <- (-g3 * d1_out$d1p1t111U * d1_out$d1p1t111D) +
    3 * (g - 3) * g3 * (d3_out$man111U_man111Db - d2_out$man111U_man111D) +
    g3 * (g3 - 1) * (d2_out$man111U_man111D - (d1_out$d1p1t111U * d1_out$d1p1t111D)),
  
  # ----------- #
  ## 8
  # 030T - 003
  COVAR030T_003 <- (-g3 * d1_out$d1p1t030T * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man030T_man003b - d2_out$man030T_man003) +
    g3 * (g3 - 1) * (d2_out$man030T_man003 - (d1_out$d1p1t030T * d1_out$d1p1t003)),
  
  # 030T - 012
  COVAR030T_012 <- (-g3 * d1_out$d1p1t030T * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man030T_man012b - d2_out$man030T_man012) +
    g3 * (g3 - 1) * (d2_out$man030T_man012 - (d1_out$d1p1t030T * d1_out$d1p1t012)),
  
  # 030T - 102
  COVAR030T_102 <- (-g3 * d1_out$d1p1t030T * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man030T_man102b - d2_out$man030T_man102) +
    g3 * (g3 - 1) * (d2_out$man030T_man102 - (d1_out$d1p1t030T * d1_out$d1p1t102)),
  
  # 030T - 021D
  COVAR030T_021D <- (-g3 * d1_out$d1p1t030T * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man030T_man021Db - d2_out$man030T_man021D) +
    g3 * (g3 - 1) * (d2_out$man030T_man021D - (d1_out$d1p1t030T * d1_out$d1p1t021D)),
  
  # 030T - 021U
  COVAR030T_021U <- (-g3 * d1_out$d1p1t030T * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man030T_man021Ub - d2_out$man030T_man021U) +
    g3 * (g3 - 1) * (d2_out$man030T_man021U - (d1_out$d1p1t030T * d1_out$d1p1t021U)),
  
  # 030T - 021C
  COVAR030T_021C <- (-g3 * d1_out$d1p1t030T * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man030T_man021Cb - d2_out$man030T_man021C) +
    g3 * (g3 - 1) * (d2_out$man030T_man021C - (d1_out$d1p1t030T * d1_out$d1p1t021C)),
  
  # 030T - 111D
  COVAR030T_111D <- (-g3 * d1_out$d1p1t030T * d1_out$d1p1t111D) +
    3 * (g - 3) * g3 * (d3_out$man030T_man111Db - d2_out$man030T_man111D) +
    g3 * (g3 - 1) * (d2_out$man030T_man111D - (d1_out$d1p1t030T * d1_out$d1p1t111D)),
  
  # 030T - 111U
  COVAR030T_111U <- (-g3 * d1_out$d1p1t030T * d1_out$d1p1t111U) +
    3 * (g - 3) * g3 * (d3_out$man030T_man111Ub - d2_out$man030T_man111U) +
    g3 * (g3 - 1) * (d2_out$man030T_man111U - (d1_out$d1p1t030T * d1_out$d1p1t111U)),
  
  # ----------- #
  ## 9
  # 030C - 003
  COVAR030C_003 <- (-g3 * d1_out$d1p1t030C * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man030C_man003b - d2_out$man030C_man003) +
    g3 * (g3 - 1) * (d2_out$man030C_man003 - (d1_out$d1p1t030C * d1_out$d1p1t003)),
  
  # 030C - 012
  COVAR030C_012 <- (-g3 * d1_out$d1p1t030C * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man030C_man012b - d2_out$man030C_man012) +
    g3 * (g3 - 1) * (d2_out$man030C_man012 - (d1_out$d1p1t030C * d1_out$d1p1t012)),
  
  # 030C - 102
  COVAR030C_102 <- (-g3 * d1_out$d1p1t030C * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man030C_man102b - d2_out$man030C_man102) +
    g3 * (g3 - 1) * (d2_out$man030C_man102 - (d1_out$d1p1t030C * d1_out$d1p1t102)),
  
  # 030C - 021D
  COVAR030C_021D <- (-g3 * d1_out$d1p1t030C * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man030C_man021Db - d2_out$man030C_man021D) +
    g3 * (g3 - 1) * (d2_out$man030C_man021D - (d1_out$d1p1t030C * d1_out$d1p1t021D)),
  
  # 030C - 021U
  COVAR030C_021U <- (-g3 * d1_out$d1p1t030C * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man030C_man021Ub - d2_out$man030C_man021U) +
    g3 * (g3 - 1) * (d2_out$man030C_man021U - (d1_out$d1p1t030C * d1_out$d1p1t021U)),
  
  # 030C - 021C
  COVAR030C_021C <- (-g3 * d1_out$d1p1t030C * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man030C_man021Cb - d2_out$man030C_man021C) +
    g3 * (g3 - 1) * (d2_out$man030C_man021C - (d1_out$d1p1t030C * d1_out$d1p1t021C)),
  
  # 030C - 111D
  COVAR030C_111D <- (-g3 * d1_out$d1p1t030C * d1_out$d1p1t111D) +
    3 * (g - 3) * g3 * (d3_out$man030C_man111Db - d2_out$man030C_man111D) +
    g3 * (g3 - 1) * (d2_out$man030C_man111D - (d1_out$d1p1t030C * d1_out$d1p1t111D)),
  
  # 030C - 111U
  COVAR030C_111U <- (-g3 * d1_out$d1p1t030C * d1_out$d1p1t111U) +
    3 * (g - 3) * g3 * (d3_out$man030C_man111Ub - d2_out$man030C_man111U) +
    g3 * (g3 - 1) * (d2_out$man030C_man111U - (d1_out$d1p1t030C * d1_out$d1p1t111U)),
  
  # 030C - 030T
  COVAR030C_030T <- (-g3 * d1_out$d1p1t030C * d1_out$d1p1t030T) +
    3 * (g - 3) * g3 * (d3_out$man030C_man030Tb - d2_out$man030C_man030T) +
    g3 * (g3 - 1) * (d2_out$man030C_man030T - (d1_out$d1p1t030C * d1_out$d1p1t030T)),
  
  # ----------- #
  ## 10
  # 201 - 003
  COVAR201_003 <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man201_man003b - d2_out$man201_man003) +
    g3 * (g3 - 1) * (d2_out$man201_man003 - (d1_out$d1p1t201 * d1_out$d1p1t003)),
  
  # 201 - 012
  COVAR201_012 <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man201_man012b - d2_out$man201_man012) +
    g3 * (g3 - 1) * (d2_out$man201_man012 - (d1_out$d1p1t201 * d1_out$d1p1t012)),
  
  # COVARIANCE DIFFER WITH WASSERMAN AND FAUST
  # 1994 (pp. 582-583)
  # (i.e. TRIADS computer program of Walker and Wasserman, 1987)
  # 201-102: we use the equation of Leinhardt and Holland (1975)
  # P0(u,v): 9m^(3)n^(3); P1(u,v): 2m^(2)m^(2)(m+4-n)
  # results: W-F: -18.4; here: 7.06
  # 300-030T: original: -0.01; here:0.011
  
  # 201 - 102
  COVAR201_102 <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man201_man102b - d2_out$man201_man102) +
    g3 * (g3 - 1) * (d2_out$man201_man102 - (d1_out$d1p1t201 * d1_out$d1p1t102)),
  
  # 201 - 021D
  COVAR201_021D <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man201_man021Db - d2_out$man201_man021D) +
    g3 * (g3 - 1) * (d2_out$man201_man021D - (d1_out$d1p1t201 * d1_out$d1p1t021D)),
  
  # 201 - 021U
  COVAR201_021U <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man201_man021Ub - d2_out$man201_man021U) +
    g3 * (g3 - 1) * (d2_out$man201_man021U - (d1_out$d1p1t201 * d1_out$d1p1t021U)),
  
  # 201 - 021C
  COVAR201_021C <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man201_man021Cb - d2_out$man201_man021C) +
    g3 * (g3 - 1) * (d2_out$man201_man021C - (d1_out$d1p1t201 * d1_out$d1p1t021C)),
  
  # 201 - 111D
  COVAR201_111D <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t111D) +
    3 * (g - 3) * g3 * (d3_out$man201_man111Db - d2_out$man201_man111D) +
    g3 * (g3 - 1) * (d2_out$man201_man111D - (d1_out$d1p1t201 * d1_out$d1p1t111D)),
  
  # 201 - 111U
  COVAR201_111U <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t111U) +
    3 * (g - 3) * g3 * (d3_out$man201_man111Ub - d2_out$man201_man111U) +
    g3 * (g3 - 1) * (d2_out$man201_man111U - (d1_out$d1p1t201 * d1_out$d1p1t111U)),
  
  # 201 - 030T
  COVAR201_030T <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t030T) +
    3 * (g - 3) * g3 * (d3_out$man201_man030Tb - d2_out$man201_man030T) +
    g3 * (g3 - 1) * (d2_out$man201_man030T - (d1_out$d1p1t201 * d1_out$d1p1t030T)),
  
  # 201 - 030C
  COVAR201_030C <- (-g3 * d1_out$d1p1t201 * d1_out$d1p1t030C) +
    3 * (g - 3) * g3 * (d3_out$man201_man030Cb - d2_out$man201_man030C) +
    g3 * (g3 - 1) * (d2_out$man201_man030C - (d1_out$d1p1t201 * d1_out$d1p1t030C)),
  
  # ----------- #
  ## 11
  # 120D - 003
  COVAR120D_003 <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man120D_man003b - d2_out$man120D_man003) +
    g3 * (g3 - 1) * (d2_out$man120D_man003 - (d1_out$d1p1t120D * d1_out$d1p1t003)),
  
  # 120D - 012
  COVAR120D_012 <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man120D_man012b - d2_out$man120D_man012) +
    g3 * (g3 - 1) * (d2_out$man120D_man012 - (d1_out$d1p1t120D * d1_out$d1p1t012)),
  
  # 120D - 102
  COVAR120D_102 <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man120D_man102b - d2_out$man120D_man102) +
    g3 * (g3 - 1) * (d2_out$man120D_man102 - (d1_out$d1p1t120D * d1_out$d1p1t102)),
  
  # 120D - 021D
  COVAR120D_021D <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man120D_man021Db - d2_out$man120D_man021D) +
    g3 * (g3 - 1) * (d2_out$man120D_man021D - (d1_out$d1p1t120D * d1_out$d1p1t021D)),
  
  # 120D - 021U
  COVAR120D_021U <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man120D_man021Ub - d2_out$man120D_man021U) +
    g3 * (g3 - 1) * (d2_out$man120D_man021U - (d1_out$d1p1t120D * d1_out$d1p1t021U)),
  
  # 120D - 021C
  COVAR120D_021C <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man120D_man021Cb - d2_out$man120D_man021C) +
    g3 * (g3 - 1) * (d2_out$man120D_man021C - (d1_out$d1p1t120D * d1_out$d1p1t021C)),
  
  # 120D - 111D
  COVAR120D_111D <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t111D) +
    3 * (g - 3) * g3 * (d3_out$man120D_man111Db - d2_out$man120D_man111D) +
    g3 * (g3 - 1) * (d2_out$man120D_man111D - (d1_out$d1p1t120D * d1_out$d1p1t111D)),
  
  # 120D - 111U
  COVAR120D_111U <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t111U) +
    3 * (g - 3) * g3 * (d3_out$man120D_man111Ub - d2_out$man120D_man111U) +
    g3 * (g3 - 1) * (d2_out$man120D_man111U - (d1_out$d1p1t120D * d1_out$d1p1t111U)),
  
  # 120D - 030T
  COVAR120D_030T <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t030T) +
    3 * (g - 3) * g3 * (d3_out$man120D_man030Tb - d2_out$man120D_man030T) +
    g3 * (g3 - 1) * (d2_out$man120D_man030T - (d1_out$d1p1t120D * d1_out$d1p1t030T)),
  
  # 120D - 030C
  COVAR120D_030C <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t030C) +
    3 * (g - 3) * g3 * (d3_out$man120D_man030Cb - d2_out$man120D_man030C) +
    g3 * (g3 - 1) * (d2_out$man120D_man030C - (d1_out$d1p1t120D * d1_out$d1p1t030C)),
  
  # 120D - 201
  COVAR120D_201 <- (-g3 * d1_out$d1p1t120D * d1_out$d1p1t201) +
    3 * (g - 3) * g3 * (d3_out$man120D_man201b - d2_out$man120D_man201) +
    g3 * (g3 - 1) * (d2_out$man120D_man201 - (d1_out$d1p1t120D * d1_out$d1p1t201)),
  
  # ----------- #
  ## 12
  # 120U - 003
  COVAR120U_003 <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man120U_man003b - d2_out$man120U_man003) +
    g3 * (g3 - 1) * (d2_out$man120U_man003 - (d1_out$d1p1t120U * d1_out$d1p1t003)),
  
  # 120U - 012
  COVAR120U_012 <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man120U_man012b - d2_out$man120U_man012) +
    g3 * (g3 - 1) * (d2_out$man120U_man012 - (d1_out$d1p1t120U * d1_out$d1p1t012)),
  # 120U - 102
  COVAR120U_102 <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man120U_man102b - d2_out$man120U_man102) +
    g3 * (g3 - 1) * (d2_out$man120U_man102 - (d1_out$d1p1t120U * d1_out$d1p1t102)),
  
  # 120U - 021D
  COVAR120U_021D <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man120U_man021Db - d2_out$man120U_man021D) +
    g3 * (g3 - 1) * (d2_out$man120U_man021D - (d1_out$d1p1t120U * d1_out$d1p1t021D)),
  
  # 120U - 021U
  COVAR120U_021U <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man120U_man021Ub - d2_out$man120U_man021U) +
    g3 * (g3 - 1) * (d2_out$man120U_man021U - (d1_out$d1p1t120U * d1_out$d1p1t021U)),
  
  # 120U - 021C
  COVAR120U_021C <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man120U_man021Cb - d2_out$man120U_man021C) +
    g3 * (g3 - 1) * (d2_out$man120U_man021C - (d1_out$d1p1t120U * d1_out$d1p1t021C)),
  
  # 120U - 111D
  COVAR120U_111D <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t111D) +
    3 * (g - 3) * g3 * (d3_out$man120U_man111Db - d2_out$man120U_man111D) +
    g3 * (g3 - 1) * (d2_out$man120U_man111D - (d1_out$d1p1t120U * d1_out$d1p1t111D)),
  
  # 120U - 111U
  COVAR120U_111U <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t111U) +
    3 * (g - 3) * g3 * (d3_out$man120U_man111Ub - d2_out$man120U_man111U) +
    g3 * (g3 - 1) * (d2_out$man120U_man111U - (d1_out$d1p1t120U * d1_out$d1p1t111U)),
  
  # 120U - 030T
  COVAR120U_030T <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t030T) +
    3 * (g - 3) * g3 * (d3_out$man120U_man030Tb - d2_out$man120U_man030T) +
    g3 * (g3 - 1) * (d2_out$man120U_man030T - (d1_out$d1p1t120U * d1_out$d1p1t030T)),
  
  # 120U - 030C
  COVAR120U_030C <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t030C) +
    3 * (g - 3) * g3 * (d3_out$man120U_man030Cb - d2_out$man120U_man030C) +
    g3 * (g3 - 1) * (d2_out$man120U_man030C - (d1_out$d1p1t120U * d1_out$d1p1t030C)),
  
  # 120U - 201
  COVAR120U_201 <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t201) +
    3 * (g - 3) * g3 * (d3_out$man120U_man201b - d2_out$man120U_man201) +
    g3 * (g3 - 1) * (d2_out$man120U_man201 - (d1_out$d1p1t120U * d1_out$d1p1t201)),
  
  # 120U - 120D
  COVAR120U_120D <- (-g3 * d1_out$d1p1t120U * d1_out$d1p1t120D) +
    3 * (g - 3) * g3 * (d3_out$man120U_man120Db - d2_out$man120U_man120D) +
    g3 * (g3 - 1) * (d2_out$man120U_man120D - (d1_out$d1p1t120U * d1_out$d1p1t120D)),
  
  # ----------- #
  ## 13
  # 120C - 003
  COVAR120C_003 <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man120C_man003b - d2_out$man120C_man003) +
    g3 * (g3 - 1) * (d2_out$man120C_man003 - (d1_out$d1p1t120C * d1_out$d1p1t003)),
  
  # 120C - 012
  COVAR120C_012 <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man120C_man012b - d2_out$man120C_man012) +
    g3 * (g3 - 1) * (d2_out$man120C_man012 - (d1_out$d1p1t120C * d1_out$d1p1t012)),
  
  # 120C - 102
  COVAR120C_102 <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man120C_man102b - d2_out$man120C_man102) +
    g3 * (g3 - 1) * (d2_out$man120C_man102 - (d1_out$d1p1t120C * d1_out$d1p1t102)),
  
  # 120C - 021D
  COVAR120C_021D <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man120C_man021Db - d2_out$man120C_man021D) +
    g3 * (g3 - 1) * (d2_out$man120C_man021D - (d1_out$d1p1t120C * d1_out$d1p1t021D)),
  
  # 120C - 021U
  COVAR120C_021U <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man120C_man021Ub - d2_out$man120C_man021U) +
    g3 * (g3 - 1) * (d2_out$man120C_man021U - (d1_out$d1p1t120C * d1_out$d1p1t021U)),
  
  # 120C - 021C
  COVAR120C_021C <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man120C_man021Cb - d2_out$man120C_man021C) +
    g3 * (g3 - 1) * (d2_out$man120C_man021C - (d1_out$d1p1t120C * d1_out$d1p1t021C)),
  
  # 120C - 111D
  COVAR120C_111D <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t111D) +
    3 * (g - 3) * g3 * (d3_out$man120C_man111Db - d2_out$man120C_man111D) +
    g3 * (g3 - 1) * (d2_out$man120C_man111D - (d1_out$d1p1t120C * d1_out$d1p1t111D)),
  
  # 120C - 111U
  COVAR120C_111U <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t111U) +
    3 * (g - 3) * g3 * (d3_out$man120C_man111Ub - d2_out$man120C_man111U) +
    g3 * (g3 - 1) * (d2_out$man120C_man111U - (d1_out$d1p1t120C * d1_out$d1p1t111U)),
  
  # 120C - 030T
  COVAR120C_030T <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t030T) +
    3 * (g - 3) * g3 * (d3_out$man120C_man030Tb - d2_out$man120C_man030T) +
    g3 * (g3 - 1) * (d2_out$man120C_man030T - (d1_out$d1p1t120C * d1_out$d1p1t030T)),
  
  # 120C - 030C
  COVAR120C_030C <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t030C) +
    3 * (g - 3) * g3 * (d3_out$man120C_man030Cb - d2_out$man120C_man030C) +
    g3 * (g3 - 1) * (d2_out$man120C_man030C - (d1_out$d1p1t120C * d1_out$d1p1t030C)),
  
  # 120C - 201
  COVAR120C_201 <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t201) +
    3 * (g - 3) * g3 * (d3_out$man120C_man201b - d2_out$man120C_man201) +
    g3 * (g3 - 1) * (d2_out$man120C_man201 - (d1_out$d1p1t120C * d1_out$d1p1t201)),
  
  # 120C - 120D
  COVAR120C_120D <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t120D) +
    3 * (g - 3) * g3 * (d3_out$man120C_man120Db - d2_out$man120C_man120D) +
    g3 * (g3 - 1) * (d2_out$man120C_man120D - (d1_out$d1p1t120C * d1_out$d1p1t120D)),
  
  # 120C - 120U
  COVAR120C_120U <- (-g3 * d1_out$d1p1t120C * d1_out$d1p1t120U) +
    3 * (g - 3) * g3 * (d3_out$man120C_man120Ub - d2_out$man120C_man120U) +
    g3 * (g3 - 1) * (d2_out$man120C_man120U - (d1_out$d1p1t120C * d1_out$d1p1t120U)),
  
  # ----------- #
  ## 14
  # 210 - 003
  COVAR210_003 <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man210_man003b - d2_out$man210_man003) +
    g3 * (g3 - 1) * (d2_out$man210_man003 - (d1_out$d1p1t210 * d1_out$d1p1t003)),
  
  # 210 - 012
  COVAR210_012 <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man210_man012b - d2_out$man210_man012) +
    g3 * (g3 - 1) * (d2_out$man210_man012 - (d1_out$d1p1t210 * d1_out$d1p1t012)),
  
  # 210 - 102
  COVAR210_102 <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man210_man102b - d2_out$man210_man102) +
    g3 * (g3 - 1) * (d2_out$man210_man102 - (d1_out$d1p1t210 * d1_out$d1p1t102)),
  
  # 210 - 021D
  COVAR210_021D <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man210_man021Db - d2_out$man210_man021D) +
    g3 * (g3 - 1) * (d2_out$man210_man021D - (d1_out$d1p1t210 * d1_out$d1p1t021D)),
  
  # 210 - 021U
  COVAR210_021U <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man210_man021Ub - d2_out$man210_man021U) +
    g3 * (g3 - 1) * (d2_out$man210_man021U - (d1_out$d1p1t210 * d1_out$d1p1t021U)),
  
  # 210 - 021C
  COVAR210_021C <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man210_man021Cb - d2_out$man210_man021C) +
    g3 * (g3 - 1) * (d2_out$man210_man021C - (d1_out$d1p1t210 * d1_out$d1p1t021C)),
  
  # 210 - 111D
  COVAR210_111D <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t111D) +
    3 * (g - 3) * g3 * (d3_out$man210_man111Db - d2_out$man210_man111D) +
    g3 * (g3 - 1) * (d2_out$man210_man111D - (d1_out$d1p1t210 * d1_out$d1p1t111D)),
  
  # 210 - 111U
  COVAR210_111U <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t111U) +
    3 * (g - 3) * g3 * (d3_out$man210_man111Ub - d2_out$man210_man111U) +
    g3 * (g3 - 1) * (d2_out$man210_man111U - (d1_out$d1p1t210 * d1_out$d1p1t111U)),
  
  # 210 - 030T
  COVAR210_030T <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t030T) +
    3 * (g - 3) * g3 * (d3_out$man210_man030Tb - d2_out$man210_man030T) +
    g3 * (g3 - 1) * (d2_out$man210_man030T - (d1_out$d1p1t210 * d1_out$d1p1t030T)),
  
  # 210 - 030C
  COVAR210_030C <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t030C) +
    3 * (g - 3) * g3 * (d3_out$man210_man030Cb - d2_out$man210_man030C) +
    g3 * (g3 - 1) * (d2_out$man210_man030C - (d1_out$d1p1t210 * d1_out$d1p1t030C)),
  
  # 210 - 201
  COVAR210_201 <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t201) +
    3 * (g - 3) * g3 * (d3_out$man210_man201b - d2_out$man210_man201) +
    g3 * (g3 - 1) * (d2_out$man210_man201 - (d1_out$d1p1t210 * d1_out$d1p1t201)),
  
  # 210 - 120D
  COVAR210_120D <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t120D) +
    3 * (g - 3) * g3 * (d3_out$man210_man120Db - d2_out$man210_man120D) +
    g3 * (g3 - 1) * (d2_out$man210_man120D - (d1_out$d1p1t210 * d1_out$d1p1t120D)),
  
  # 210 - 120U
  COVAR210_120U <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t120U) +
    3 * (g - 3) * g3 * (d3_out$man210_man120Ub - d2_out$man210_man120U) +
    g3 * (g3 - 1) * (d2_out$man210_man120U - (d1_out$d1p1t210 * d1_out$d1p1t120U)),
  
  # 210 - 120C
  COVAR210_120C <- (-g3 * d1_out$d1p1t210 * d1_out$d1p1t120C) +
    3 * (g - 3) * g3 * (d3_out$man210_man120Cb - d2_out$man210_man120C) +
    g3 * (g3 - 1) * (d2_out$man210_man120C - (d1_out$d1p1t210 * d1_out$d1p1t120C)),
  
  # ----------- #
  ## 15
  # 300 - 003
  COVAR300_003 <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t003) +
    3 * (g - 3) * g3 * (d3_out$man300_man003b - d2_out$man300_man003) +
    g3 * (g3 - 1) * (d2_out$man300_man003 - (d1_out$d1p1t300 * d1_out$d1p1t003)),
  
  # 300 - 012
  COVAR300_012 <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t012) +
    3 * (g - 3) * g3 * (d3_out$man300_man012b - d2_out$man300_man012) +
    g3 * (g3 - 1) * (d2_out$man300_man012 - (d1_out$d1p1t300 * d1_out$d1p1t012)),
  
  # 300 - 102
  COVAR300_102 <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t102) +
    3 * (g - 3) * g3 * (d3_out$man300_man102b - d2_out$man300_man102) +
    g3 * (g3 - 1) * (d2_out$man300_man102 - (d1_out$d1p1t300 * d1_out$d1p1t102)),
  
  # 300 - 021D
  COVAR300_021D <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t021D) +
    3 * (g - 3) * g3 * (d3_out$man300_man021Db - d2_out$man300_man021D) +
    g3 * (g3 - 1) * (d2_out$man300_man021D - (d1_out$d1p1t300 * d1_out$d1p1t021D)),
  
  # 300 - 021U
  COVAR300_021U <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t021U) +
    3 * (g - 3) * g3 * (d3_out$man300_man021Ub - d2_out$man300_man021U) +
    g3 * (g3 - 1) * (d2_out$man300_man021U - (d1_out$d1p1t300 * d1_out$d1p1t021U)),
  
  # 300 - 021C
  COVAR300_021C <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t021C) +
    3 * (g - 3) * g3 * (d3_out$man300_man021Cb - d2_out$man300_man021C) +
    g3 * (g3 - 1) * (d2_out$man300_man021C - (d1_out$d1p1t300 * d1_out$d1p1t021C)),
  
  # 300 - 111D
  COVAR300_111D <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t111D) +
    3 * (g - 3) * g3 * (d3_out$man300_man111Db - d2_out$man300_man111D) +
    g3 * (g3 - 1) * (d2_out$man300_man111D - (d1_out$d1p1t300 * d1_out$d1p1t111D)),
  
  # 300 - 111U
  COVAR300_111U <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t111U) +
    3 * (g - 3) * g3 * (d3_out$man300_man111Ub - d2_out$man300_man111U) +
    g3 * (g3 - 1) * (d2_out$man300_man111U - (d1_out$d1p1t300 * d1_out$d1p1t111U)),
  
  # COVARIANCE DIFFER WITH WASSERMAN AND FAUST
  # 1994 (pp. 582-583)
  # (i.e. TRIADS computer program of Walker and Wasserman, 1987)
  # 201-102: we use the equation of Leinhardt and Holland (1975)
  # P0(u,v): 9m^(3)n^(3); P1(u,v): 2m^(2)m^(2)(m+4-n)
  # results: W-F: -18.4; here: 7.06
  # 300-030T: original: -0.01; here:0.011
  
  # 300 - 030T
  COVAR300_030T <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t030T) +
    3 * (g - 3) * g3 * (d3_out$man300_man030Tb - d2_out$man300_man030T) +
    g3 * (g3 - 1) * (d2_out$man300_man030T - (d1_out$d1p1t300 * d1_out$d1p1t030T)),
  
  # 300 - 030C
  COVAR300_030C <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t030C) +
    3 * (g - 3) * g3 * (d3_out$man300_man030Cb - d2_out$man300_man030C) +
    g3 * (g3 - 1) * (d2_out$man300_man030C - (d1_out$d1p1t300 * d1_out$d1p1t030C)),
  
  # 300 - 201
  COVAR300_201 <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t201) +
    3 * (g - 3) * g3 * (d3_out$man300_man201b - d2_out$man300_man201) +
    g3 * (g3 - 1) * (d2_out$man300_man201 - (d1_out$d1p1t300 * d1_out$d1p1t201)),
  
  # 300 - 120D
  COVAR300_120D <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t120D) +
    3 * (g - 3) * g3 * (d3_out$man300_man120Db - d2_out$man300_man120D) +
    g3 * (g3 - 1) * (d2_out$man300_man120D - (d1_out$d1p1t300 * d1_out$d1p1t120D)),
  
  # 300 - 120U
  COVAR300_120U <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t120U) +
    3 * (g - 3) * g3 * (d3_out$man300_man120Ub - d2_out$man300_man120U) +
    g3 * (g3 - 1) * (d2_out$man300_man120U - (d1_out$d1p1t300 * d1_out$d1p1t120U)),
  
  # 300 - 120C
  COVAR300_120C <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t120C) +
    3 * (g - 3) * g3 * (d3_out$man300_man120Cb - d2_out$man300_man120C) +
    g3 * (g3 - 1) * (d2_out$man300_man120C - (d1_out$d1p1t300 * d1_out$d1p1t120C)),
  
  # 300 - 210
  COVAR300_210 <- (-g3 * d1_out$d1p1t300 * d1_out$d1p1t210) +
    3 * (g - 3) * g3 * (d3_out$man300_man210b - d2_out$man300_man210) +
    g3 * (g3 - 1) * (d2_out$man300_man210 - (d1_out$d1p1t300 * d1_out$d1p1t210))
)
