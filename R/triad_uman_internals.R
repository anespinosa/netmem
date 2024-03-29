# d1 ----
d1 <- function(g2, n, m, a) {
  d1p1 <- g2 * (g2 - 1) * (g2 - 2)
  man003 <- n * (n - 1) * (n - 2)
  d1p1t003 <- man003 / d1p1
  man012 <- 3 * a * n * (n - 1)
  d1p1t012 <- man012 / d1p1
  man102 <- 3 * m * n * (n - 1)
  d1p1t102 <- man102 / d1p1
  man021D <- n * a * (a - 1)
  d1p1t021D <- (3 / 4) * (man021D / d1p1)
  man021U <- n * a * (a - 1)
  d1p1t021U <- (3 / 4) * (man021U / d1p1)
  man021C <- n * a * (a - 1)
  d1p1t021C <- (3 / 2) * (man021C / d1p1)
  man111D <- 3 * m * a * n
  d1p1t111D <- man111D / d1p1
  man111U <- 3 * m * a * n
  d1p1t111U <- man111U / d1p1
  man030T <- a * (a - 1) * (a - 2)
  d1p1t030T <- (3 / 4) * (man030T / d1p1)
  man030C <- a * (a - 1) * (a - 2)
  d1p1t030C <- (1 / 4) * (man030C / d1p1)
  man201 <- 3 * n * m * (m - 1)
  d1p1t201 <- (man201 / d1p1)
  man120D <- m * a * (a - 1)
  d1p1t120D <- (3 / 4) * (man120D / d1p1)
  man120U <- m * a * (a - 1)
  d1p1t120U <- (3 / 4) * (man120U / d1p1)
  man120C <- m * a * (a - 1)
  d1p1t120C <- (3 / 2) * (man120C / d1p1)
  man210 <- 3 * m * (m - 1) * a
  d1p1t210 <- man210 / d1p1
  man300 <- m * (m - 1) * (m - 2)
  d1p1t300 <- man300 / d1p1

  return(
    list(
      d1p1t003 = d1p1t003, d1p1t012 = d1p1t012, d1p1t102 = d1p1t102,
      d1p1t021D = d1p1t021D, d1p1t021U = d1p1t021U, d1p1t021C = d1p1t021C,
      d1p1t111D = d1p1t111D, d1p1t111U = d1p1t111U, d1p1t030T = d1p1t030T,
      d1p1t030C = d1p1t030C, d1p1t201 = d1p1t201, d1p1t120D = d1p1t120D,
      d1p1t120U = d1p1t120U, d1p1t120C = d1p1t120C, d1p1t210 = d1p1t210,
      d1p1t300 = d1p1t300
    )
  )
}

# d2 ----
d2 <- function(g2, n, m, a) {
  d2p0 <- g2 * (g2 - 1) * (g2 - 2) * (g2 - 3) * (g2 - 4) * (g2 - 5)
  n1t003 <- n * (n - 1) * (n - 2) * (n - 3) * (n - 4) * (n - 5)
  man003_man003 <- n1t003 / d2p0

  n1t012 <- 3 * a * n * (n - 1) * (n - 2) * (n - 3) * (n - 4)
  man012_man003 <- n1t012 / d2p0
  n2t012 <- 9 * a * (a - 1) * n * (n - 1) * (n - 2) * (n - 3)
  man012_man012 <- n2t012 / d2p0

  n1t102 <- 3 * m * n * (n - 1) * (n - 2) * (n - 3) * (n - 4)
  man102_man003 <- n1t102 / d2p0
  n2t102 <- 9 * m * a * n * (n - 1) * (n - 2) * (n - 3)
  man102_man012 <- n2t102 / d2p0
  n3t102_102 <- 9 * m * (m - 1) * n * (n - 1) * (n - 2) * (n - 3)
  man102_man102 <- n3t102_102 / d2p0

  n1t021D <- a * (a - 1) * n * (n - 1) * (n - 2) * (n - 3)
  man021D_man003 <- (3 / 4) * (n1t021D / d2p0)
  n2t021D <- a * (a - 1) * (a - 2) * n * (n - 1) * (n - 2)
  man021D_man012 <- (9 / 4) * (n2t021D / d2p0)
  n3t021D <- m * a * (a - 1) * n * (n - 1) * (n - 2)
  man021D_man102 <- (9 / 4) * (n3t021D / d2p0)
  n4t021D <- a * (a - 1) * (a - 2) * (a - 3) * n * (n - 1)
  man021D_man021D <- (9 / 16) * (n4t021D / d2p0)

  n1t021U <- a * (a - 1) * n * (n - 1) * (n - 2) * (n - 3)
  man021U_man003 <- (3 / 4) * (n1t021U / d2p0)
  n2t021U <- a * (a - 1) * (a - 2) * n * (n - 1) * (n - 2)
  man021U_man012 <- (9 / 4) * (n2t021U / d2p0)
  n3t021U <- m * a * (a - 1) * n * (n - 1) * (n - 2)
  man021U_man102 <- (9 / 4) * (n3t021U / d2p0)
  n4t021U <- a * (a - 1) * (a - 2) * (a - 3) * n * (n - 1)
  man021U_man021D <- (9 / 16) * (n4t021U / d2p0)
  n5t021U <- a * (a - 1) * (a - 2) * (a - 3) * n * (n - 1)
  man021U_man021U <- (9 / 16) * (n5t021U / d2p0)

  n1t021C <- a * (a - 1) * n * (n - 1) * (n - 2) * (n - 3)
  man021C_man003 <- (3 / 2) * (n1t021C / d2p0)
  n2t021C <- a * (a - 1) * (a - 2) * n * (n - 1) * (n - 2)
  man021C_man012 <- (9 / 2) * (n2t021C / d2p0)
  n3t021C <- m * a * (a - 1) * n * (n - 1) * (n - 2)
  man021C_man102 <- (9 / 2) * (n3t021C / d2p0)
  n4t021C <- a * (a - 1) * (a - 2) * (a - 3) * n * (n - 1)
  man021C_man021D <- (9 / 8) * (n4t021C / d2p0)
  n5t021C <- a * (a - 1) * (a - 2) * (a - 3) * n * (n - 1)
  man021C_man021U <- (9 / 8) * (n5t021C / d2p0)
  n6t021C <- a * (a - 1) * (a - 2) * (a - 3) * n * (n - 1) # n="nominator"
  man021C_man021C <- (9 / 4) * (n6t021C / d2p0) # !

  n1t111D <- 3 * m * a * n * (n - 1) * (n - 2) * (n - 3)
  man111D_man003 <- n1t111D / d2p0
  n2t111D <- 9 * m * a * (a - 1) * n * (n - 1) * (n - 2)
  man111D_man012 <- n2t111D / d2p0
  n3t111D <- 9 * m * (m - 1) * a * n * (n - 1) * (n - 2)
  man111D_man102 <- n3t111D / d2p0
  n4t111D <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man111D_man021D <- (9 / 4) * (n4t111D / d2p0)
  n5t111D <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man111D_man021U <- (9 / 4) * (n5t111D / d2p0)
  n6t111D <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man111D_man021C <- (9 / 2) * (n6t111D / d2p0)
  n7t111D <- 9 * m * (m - 1) * a * (a - 1) * n * (n - 1)
  man111D_man111D <- n7t111D / d2p0

  n1t111U <- 3 * m * a * n * (n - 1) * (n - 2) * (n - 3)
  man111U_man003 <- n1t111U / d2p0
  n2t111U <- 9 * m * a * (a - 1) * n * (n - 1) * (n - 2)
  man111U_man012 <- n2t111U / d2p0
  n3t111U <- 9 * m * (m - 1) * a * n * (n - 1) * (n - 2)
  man111U_man102 <- n3t111U / d2p0
  n4t111U <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man111U_man021D <- (9 / 4) * (n4t111U / d2p0)
  n5t111U <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man111U_man021U <- (9 / 4) * (n5t111U / d2p0)
  n6t111U <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man111U_man021C <- (9 / 2) * (n6t111U / d2p0)
  n7t111U <- 9 * m * (m - 1) * a * (a - 1) * n * (n - 1)
  man111U_man111D <- n7t111U / d2p0
  n8t111U <- 9 * m * (m - 1) * a * (a - 1) * n * (n - 1)
  man111U_man111U <- n8t111U / d2p0

  n1t030T <- a * (a - 1) * (a - 2) * n * (n - 1) * (n - 2)
  man030T_man003 <- (3 / 4) * (n1t030T / d2p0)
  n2t030T <- a * (a - 1) * (a - 2) * (a - 3) * n * (n - 1)
  man030T_man012 <- (9 / 4) * (n2t030T / d2p0)
  n3t030T <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man030T_man102 <- (9 / 4) * (n3t030T / d2p0)
  n4t030T <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4) * n
  man030T_man021D <- (9 / 16) * (n4t030T / d2p0)
  n5t030T <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4) * n
  man030T_man021U <- (9 / 16) * (n5t030T / d2p0)
  n6t030T <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4) * n
  man030T_man021C <- (9 / 8) * (n6t030T / d2p0)
  n7t030T <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man030T_man111D <- (9 / 4) * (n7t030T / d2p0)
  n8t030T <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man030T_man111U <- (9 / 4) * (n8t030T / d2p0)
  n9t030T <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4) * (a - 5)
  man030T_man030T <- (9 / 16) * (n9t030T / d2p0)

  n1t030C <- a * (a - 1) * (a - 2) * n * (n - 1) * (n - 2)
  man030C_man003 <- (1 / 4) * (n1t030C / d2p0)
  n2t030C <- a * (a - 1) * (a - 2) * (a - 3) * n * (n - 1)
  man030C_man012 <- (3 / 4) * (n2t030C / d2p0)
  n3t030C <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man030C_man102 <- (3 / 4) * (n3t030C / d2p0)
  n4t030C <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4) * n
  man030C_man021D <- (3 / 16) * (n4t030C / d2p0)
  n5t030C <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4) * n
  man030C_man021U <- (3 / 16) * (n5t030C / d2p0)
  n6t030C <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4) * n
  man030C_man021C <- (3 / 8) * (n6t030C / d2p0)
  n7t030C <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man030C_man111D <- (3 / 4) * (n7t030C / d2p0)
  n8t030C <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man030C_man111U <- (3 / 4) * (n8t030C / d2p0)
  n9t030C <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4) * (a - 5)
  man030C_man030T <- (3 / 16) * (n9t030C / d2p0)
  n10t030C <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4) * (a - 5)
  man030C_man030C <- (1 / 16) * (n10t030C / d2p0)

  n1t201 <- 3 * m * (m - 1) * n * (n - 1) * (n - 2) * (n - 3)
  man201_man003 <- n1t201 / d2p0
  n2t201 <- 9 * m * (m - 1) * a * n * (n - 1) * (n - 2)
  man201_man012 <- n2t201 / d2p0
  n3t201 <- 9 * m * (m - 1) * (m - 2) * n * (n - 1) * (n - 2)
  man201_man102 <- n3t201 / d2p0
  n4t201 <- m * (m - 1) * a * (a - 1) * n * (n - 1)
  man201_man021D <- (9 / 4) * (n4t201 / d2p0)
  n5t201 <- m * (m - 1) * a * (a - 1) * n * (n - 1)
  man201_man021U <- (9 / 4) * (n5t201 / d2p0)
  n6t201 <- m * (m - 1) * a * (a - 1) * n * (n - 1)
  man201_man021C <- (9 / 2) * (n6t201 / d2p0)
  n7t201 <- 9 * m * (m - 1) * (m - 2) * a * n * (n - 1)
  man201_man111D <- n7t201 / d2p0
  n8t201 <- 9 * m * (m - 1) * (m - 2) * a * n * (n - 1)
  man201_man111U <- n8t201 / d2p0
  n9t201 <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man201_man030T <- (9 / 4) * (n9t201 / d2p0)
  n10t201 <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man201_man030C <- (3 / 4) * (n10t201 / d2p0)
  n11t201 <- 9 * m * (m - 1) * (m - 2) * (m - 3) * n * (n - 1)
  man201_man201 <- n11t201 / d2p0

  n1t120D <- m * a * (a - 1) * n * (n - 1) * (n - 2)
  man120D_man003 <- (3 / 4) * (n1t120D / d2p0)
  n2t120D <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man120D_man012 <- (9 / 4) * (n2t120D / d2p0)
  n3t120D <- m * (m - 1) * a * (a - 1) * n * (n - 1)
  man120D_man102 <- (9 / 4) * (n3t120D / d2p0)
  n4t120D <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man120D_man021D <- (9 / 16) * (n4t120D / d2p0)
  n5t120D <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man120D_man021U <- (9 / 16) * (n5t120D / d2p0)
  n6t120D <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man120D_man021C <- (9 / 8) * (n6t120D / d2p0)
  n7t120D <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man120D_man111D <- (9 / 4) * (n7t120D / d2p0)
  n8t120D <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man120D_man111U <- (9 / 4) * (n8t120D / d2p0)
  n9t120D <- m * a * (a - 1) * (a - 2) * (a - 3) * (a - 4)
  man120D_man030T <- (9 / 16) * (n9t120D / d2p0)
  n10t120D <- m * a * (a - 1) * (a - 2) * (a - 3) * (a - 4)
  man120D_man030C <- (3 / 16) * n10t120D / d2p0
  n11t120D <- m * (m - 1) * (m - 2) * a * (a - 1) * n
  man120D_man201 <- (9 / 4) * (n11t120D / d2p0)
  n12t120D <- m * (m - 1) * a * (a - 1) * (a - 2) * (a - 3)
  man120D_man120D <- (9 / 16) * (n12t120D / d2p0)

  n1t120U <- m * a * (a - 1) * n * (n - 1) * (n - 2)
  man120U_man003 <- (3 / 4) * (n1t120U / d2p0)
  n2t120U <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man120U_man012 <- (9 / 4) * (n2t120U / d2p0)
  n3t120U <- m * (m - 1) * a * (a - 1) * n * (n - 1)
  man120U_man102 <- (9 / 4) * (n3t120U / d2p0)
  n4t120U <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man120U_man021D <- (9 / 16) * (n4t120U / d2p0)
  n5t120U <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man120U_man021U <- (9 / 16) * (n5t120U / d2p0)
  n6t120U <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man120U_man021C <- (9 / 8) * (n6t120U / d2p0)
  n7t120U <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man120U_man111D <- (9 / 4) * (n7t120U / d2p0)
  n8t120U <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man120U_man111U <- (9 / 4) * (n8t120U / d2p0)
  n9t120U <- m * a * (a - 1) * (a - 2) * (a - 3) * (a - 4)
  man120U_man030T <- (9 / 16) * (n9t120U / d2p0)
  n10t120U <- m * a * (a - 1) * (a - 2) * (a - 3) * (a - 4)
  man120U_man030C <- (3 / 16) * n10t120U / d2p0
  n11t120U <- m * (m - 1) * (m - 2) * a * (a - 1) * n
  man120U_man201 <- (9 / 4) * (n11t120U / d2p0)
  n12t120U <- m * (m - 1) * a * (a - 1) * (a - 2) * (a - 3)
  man120U_man120D <- (9 / 16) * (n12t120U / d2p0)
  n13t120U <- m * (m - 1) * a * (a - 1) * (a - 2) * (a - 3)
  man120U_man120U <- (9 / 16) * (n13t120U / d2p0)

  n1t120C <- m * a * (a - 1) * n * (n - 1) * (n - 2)
  man120C_man003 <- (3 / 2) * (n1t120C / d2p0)
  n2t120C <- m * a * (a - 1) * (a - 2) * n * (n - 1)
  man120C_man012 <- (9 / 2) * (n2t120C / d2p0)
  n3t120C <- m * (m - 1) * a * (a - 1) * n * (n - 1)
  man120C_man102 <- (9 / 2) * (n3t120C / d2p0)
  n4t120C <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man120C_man021D <- (9 / 8) * (n4t120C / d2p0)
  n5t120C <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man120C_man021U <- (9 / 8) * (n5t120C / d2p0)
  n6t120C <- m * a * (a - 1) * (a - 2) * (a - 3) * n
  man120C_man021C <- (9 / 4) * (n6t120C / d2p0)
  n7t120C <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man120C_man111D <- (9 / 2) * (n7t120C / d2p0)
  n8t120C <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man120C_man111U <- (9 / 2) * (n8t120C / d2p0)
  n9t120C <- m * a * (a - 1) * (a - 2) * (a - 3) * (a - 4)
  man120C_man030T <- (9 / 8) * (n9t120C / d2p0)
  n10t120C <- m * a * (a - 1) * (a - 2) * (a - 3) * (a - 4)
  man120C_man030C <- (3 / 8) * (n10t120C / d2p0)
  n11t120C <- m * (m - 1) * (m - 2) * a * (a - 1) * n
  man120C_man201 <- (9 / 2) * (n11t120C / d2p0)
  n12t120C <- m * (m - 1) * a * (a - 1) * (a - 2) * (a - 3)
  man120C_man120D <- (9 / 8) * (n12t120C / d2p0)
  n13t120C <- m * (m - 1) * a * (a - 1) * (a - 2) * (a - 3)
  man120C_man120U <- (9 / 8) * (n13t120C / d2p0)
  n14t120C <- m * (m - 1) * a * (a - 1) * (a - 2) * (a - 3)
  man120C_man120C <- (9 / 4) * (n14t120C / d2p0)

  n1t210 <- 3 * m * (m - 1) * a * n * (n - 1) * (n - 2)
  man210_man003 <- n1t210 / d2p0
  n2t210 <- 9 * m * (m - 1) * a * (a - 1) * n * (n - 1)
  man210_man012 <- n2t210 / d2p0
  n3t210 <- 9 * m * (m - 1) * (m - 2) * a * n * (n - 1)
  man210_man102 <- n3t210 / d2p0
  n4t210 <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man210_man021D <- (9 / 4) * (n4t210 / d2p0)
  n5t210 <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man210_man021U <- (9 / 4) * (n5t210 / d2p0)
  n6t210 <- m * (m - 1) * a * (a - 1) * (a - 2) * n
  man210_man021C <- (9 / 2) * (n6t210 / d2p0)
  n7t210 <- 9 * m * (m - 1) * (m - 2) * a * (a - 1) * n
  man210_man111D <- n7t210 / d2p0
  n8t210 <- 9 * m * (m - 1) * (m - 2) * a * (a - 1) * n
  man210_man111U <- n8t210 / d2p0
  n9t210 <- m * (m - 1) * a * (a - 1) * (a - 2) * (a - 3)
  man210_man030T <- (9 / 4) * (n9t210 / d2p0)
  n10t210 <- m * (m - 1) * a * (a - 1) * (a - 2) * (a - 3)
  man210_man030C <- (3 / 4) * (n10t210 / d2p0)
  n11t210 <- 9 * m * (m - 1) * (m - 2) * (m - 3) * a * n
  man210_man201 <- (n11t210 / d2p0)
  n12t210 <- m * (m - 1) * (m - 2) * a * (a - 1) * (a - 2)
  man210_man120D <- (9 / 4) * (n12t210 / d2p0)
  n13t210 <- m * (m - 1) * (m - 2) * a * (a - 1) * (a - 2)
  man210_man120U <- (9 / 4) * (n13t210 / d2p0)
  n14t210 <- m * (m - 1) * (m - 2) * a * (a - 1) * (a - 2)
  man210_man120C <- (9 / 2) * (n14t210 / d2p0)
  n15t210 <- 9 * m * (m - 1) * (m - 2) * (m - 3) * a * (a - 1)
  man210_man210 <- n15t210 / d2p0

  n1t300 <- m * (m - 1) * (m - 2) * n * (n - 1) * (n - 2)
  man300_man003 <- n1t300 / d2p0
  n2t300 <- 3 * m * (m - 1) * (m - 2) * a * n * (n - 1)
  man300_man012 <- n2t300 / d2p0
  n3t300 <- 3 * m * (m - 1) * (m - 2) * (m - 3) * n * (n - 1)
  man300_man102 <- n3t300 / d2p0
  n4t300 <- m * (m - 1) * (m - 2) * a * (a - 1) * n
  man300_man021D <- (3 / 4) * (n4t300 / d2p0)
  n5t300 <- m * (m - 1) * (m - 2) * a * (a - 1) * n
  man300_man021U <- (3 / 4) * (n5t300 / d2p0)
  n6t300 <- m * (m - 1) * (m - 2) * a * (a - 1) * n
  man300_man021C <- (3 / 2) * (n6t300 / d2p0)
  n7t300 <- 3 * m * (m - 1) * (m - 2) * (m - 3) * a * n
  man300_man111D <- n7t300 / d2p0
  n8t300 <- 3 * m * (m - 1) * (m - 2) * (m - 3) * a * n
  man300_man111U <- n8t300 / d2p0
  n9t300 <- m * (m - 1) * (m - 2) * a * (a - 1) * (a - 2)
  man300_man030T <- (3 / 4) * (n9t300 / d2p0)
  n10t300 <- m * (m - 1) * (m - 2) * a * (a - 1) * (a - 2)
  man300_man030C <- (1 / 4) * (n10t300 / d2p0)
  n11t300 <- 3 * m * (m - 1) * (m - 2) * (m - 3) * (m - 4) * n
  man300_man201 <- n11t300 / d2p0
  n12t300 <- m * (m - 1) * (m - 2) * (m - 3) * a * (a - 1)
  man300_man120D <- (3 / 4) * (n12t300 / d2p0)
  n13t300 <- m * (m - 1) * (m - 2) * (m - 3) * a * (a - 1)
  man300_man120U <- (3 / 4) * (n13t300 / d2p0)
  n14t300 <- m * (m - 1) * (m - 2) * (m - 3) * a * (a - 1)
  man300_man120C <- (3 / 2) * (n14t300 / d2p0)
  n15t300 <- 3 * m * (m - 1) * (m - 2) * (m - 3) * (m - 4) * a
  man300_man210 <- n15t300 / d2p0
  n16t300 <- m * (m - 1) * (m - 2) * (m - 3) * (m - 4) * (m - 5)
  man300_man300 <- n16t300 / d2p0

  return(
    list(
      # VAR
      man003_man003 = man003_man003, man012_man012 = man012_man012,
      man102_man102 = man102_man102, man021D_man021D = man021D_man021D,
      man021U_man021U = man021U_man021U, man021C_man021C = man021C_man021C,
      man111D_man111D = man111D_man111D, man111U_man111U = man111U_man111U,
      man030T_man030T = man030T_man030T, man030C_man030C = man030C_man030C,
      man201_man201 = man201_man201, man120D_man120D = man120D_man120D,
      man120U_man120U = man120U_man120U, man120C_man120C = man120C_man120C,
      man210_man210 = man210_man210, man300_man300 = man300_man300,

      # COV
      man012_man003 = man012_man003, man102_man003 = man102_man003,
      man102_man012 = man102_man012, man021D_man003 = man021D_man003,
      man021D_man012 = man021D_man012, man021D_man102 = man021D_man102,
      man021U_man003 = man021U_man003, man021U_man012 = man021U_man012,
      man021U_man102 = man021U_man102, man021U_man021D = man021U_man021D,
      man021C_man003 = man021C_man003, man021C_man012 = man021C_man012,
      man021C_man102 = man021C_man102, man021C_man021D = man021C_man021D,
      man021C_man021U = man021C_man021U, man111D_man003 = man111D_man003,
      man111D_man012 = man111D_man012, man111D_man102 = man111D_man102,
      man111D_man021D = man111D_man021D, man111D_man021U = man111D_man021U,
      man111D_man021C = man111D_man021C, man111U_man003 = man111U_man003,
      man111U_man012 = man111U_man012, man111U_man102 = man111U_man102,
      man111U_man021D = man111U_man021D, man111U_man021U = man111U_man021U,
      man111U_man021C = man111U_man021C, man111U_man111D = man111U_man111D,
      man030T_man003 = man030T_man003, man030T_man012 = man030T_man012,
      man030T_man102 = man030T_man102, man030T_man021D = man030T_man021D,
      man030T_man021U = man030T_man021U, man030T_man021C = man030T_man021C,
      man030T_man111D = man030T_man111D, man030T_man111U = man030T_man111U,
      man030C_man003 = man030C_man003, man030C_man012 = man030C_man012,
      man030C_man102 = man030C_man102, man030C_man021D = man030C_man021D,
      man030C_man021U = man030C_man021U, man030C_man021C = man030C_man021C,
      man030C_man111D = man030C_man111D, man030C_man111U = man030C_man111U,
      man030C_man030T = man030C_man030T, man201_man003 = man201_man003,
      man201_man012 = man201_man012,

      # COV, NOTE 1
      man201_man102 = man201_man102, man201_man021D = man201_man021D,
      man201_man021U = man201_man021U, man201_man021C = man201_man021C,
      man201_man111D = man201_man111D, man201_man111U = man201_man111U,
      man201_man030T = man201_man030T, man201_man030C = man201_man030C,
      man120D_man003 = man120D_man003, man120D_man012 = man120D_man012,
      man120D_man102 = man120D_man102, man120D_man021D = man120D_man021D,
      man120D_man021U = man120D_man021U, man120D_man021C = man120D_man021C,
      man120D_man111D = man120D_man111D, man120D_man111U = man120D_man111U,
      man120D_man030T = man120D_man030T, man120D_man030C = man120D_man030C,
      man120D_man201 = man120D_man201, man120U_man003 = man120U_man003,
      man120U_man012 = man120U_man012, man120U_man102 = man120U_man102,
      man120U_man021D = man120U_man021D, man120U_man021U = man120U_man021U,
      man120U_man021C = man120U_man021C, man120U_man111D = man120U_man111D,
      man120U_man111U = man120U_man111U, man120U_man030T = man120U_man030T,
      man120U_man030C = man120U_man030C, man120U_man201 = man120U_man201,
      man120U_man120D = man120U_man120D, man120C_man003 = man120C_man003,
      man120C_man012 = man120C_man012, man120C_man102 = man120C_man102,
      man120C_man021D = man120C_man021D, man120C_man021U = man120C_man021U,
      man120C_man021C = man120C_man021C, man120C_man111D = man120C_man111D,
      man120C_man111U = man120C_man111U, man120C_man030T = man120C_man030T,
      man120C_man030C = man120C_man030C, man120C_man201 = man120C_man201,
      man120C_man120D = man120C_man120D, man120C_man120U = man120C_man120U,
      man210_man003 = man210_man003, man210_man012 = man210_man012,
      man210_man102 = man210_man102, man210_man021D = man210_man021D,
      man210_man021U = man210_man021U, man210_man021C = man210_man021C,
      man210_man111D = man210_man111D, man210_man111U = man210_man111U,
      man210_man030T = man210_man030T, man210_man030C = man210_man030C,
      man210_man201 = man210_man201, man210_man120D = man210_man120D,
      man210_man120U = man210_man120U, man210_man120C = man210_man120C,
      man300_man003 = man300_man003, man300_man012 = man300_man012,
      man300_man102 = man300_man102, man300_man021D = man300_man021D,
      man300_man021U = man300_man021U, man300_man021C = man300_man021C,
      man300_man111D = man300_man111D, man300_man111U = man300_man111U,

      # COV, NOTE 2
      man300_man030T = man300_man030T, man300_man030C = man300_man030C,
      man300_man201 = man300_man201, man300_man120D = man300_man120D,
      man300_man120U = man300_man120U, man300_man120C = man300_man120C,
      man300_man210 = man300_man210
    )
  )
}

# d3 ----
d3 <- function(g2, n, m, a) {
  d3p2 <- g2 * (g2 - 1) * (g2 - 2) * (g2 - 3) * (g2 - 4)
  n1t003b <- n * (n - 1) * (n - 2) * (n - 3) * (n - 4)
  man003_man003b <- n1t003b / d3p2

  n1t012b <- 2 * a * n * (n - 1) * (n - 2) * (n - 3)
  man012_man003b <- n1t012b / d3p2
  n2t012b <- a * n * (n - 1) * (n - 2) * ((4 * a) + n - 7)
  man012_man012b <- n2t012b / d3p2

  n1t102b <- 2 * m * n * (n - 1) * (n - 2) * (n - 3)
  man102_man003b <- n1t102b / d3p2
  n2t102b <- 4 * m * a * n * (n - 1) * (n - 2)
  man102_man012b <- n2t102b / d3p2
  n3t102b <- m * n * (n - 1) * (n - 2) * ((4 * m) + n - 7)
  man102_man102b <- n3t102b / d3p2

  n1t021Db <- a * (a - 1) * n * (n - 1) * (n - 2)
  man021D_man003b <- (1 / 4) * (n1t021Db / d3p2)
  n2t021Db <- a * (a - 1) * n * (n - 1) * (a + n - 4)
  man021D_man012b <- (1 / 2) * (n2t021Db / d3p2)
  n3t021Db <- m * a * (a - 1) * n * (n - 1)
  man021D_man102b <- (1 / 2) * (n3t021Db / d3p2)
  n4t021Db <- a * (a - 1) * (a - 2) * n * (a + (4 * n) - 7)
  man021D_man021Db <- (1 / 16) * (n4t021Db / d3p2)

  n1t021Ub <- a * (a - 1) * n * (n - 1) * (n - 2)
  man021U_man003b <- (1 / 4) * (n1t021Ub / d3p2)
  n2t021Ub <- a * (a - 1) * n * (n - 1) * (a + n - 4)
  man021U_man012b <- (1 / 2) * (n2t021Ub / d3p2)
  n3t021Ub <- m * a * (a - 1) * n * (n - 1)
  man021U_man102b <- (1 / 2) * (n3t021Ub / d3p2)
  n4t021Ub <- a * (a - 1) * (a - 2) * n * (a + (4 * n) - 7)
  man021U_man021Db <- (1 / 16) * (n4t021Db / d3p2)
  n5t021Ub <- a * (a - 1) * (a - 2) * n * (a + (4 * n) - 7)
  man021U_man021Ub <- (1 / 16) * (n5t021Ub / d3p2)

  n1t021Cb <- a * (a - 1) * n * (n - 1) * (n - 2)
  man021C_man003b <- (1 / 2) * (n1t021Cb / d3p2)
  n2t021Cb <- a * (a - 1) * n * (n - 1) * (a + n - 4)
  man021C_man012b <- n2t021Cb / d3p2
  n3t021Cb <- m * a * (a - 1) * n * (n - 1)
  man021C_man102b <- (n3t021Cb / d3p2)
  n4t021Cb <- a * (a - 1) * (a - 2) * n * (a + (4 * n) - 7)
  man021C_man021Db <- (1 / 8) * (n4t021Cb / d3p2)
  n5t021Cb <- a * (a - 1) * (a - 2) * n * (a + (4 * n) - 7)
  man021C_man021Ub <- (1 / 8) * (n5t021Cb / d3p2)
  n6t021Cb <- a * (a - 1) * (a - 2) * n * (a + (4 * n) - 7)
  man021C_man021Cb <- (1 / 4) * (n6t021Cb / d3p2)

  n1t111Db <- m * a * n * (n - 1) * (n - 2)
  man111D_man003b <- n1t111Db / d3p2
  n2t111Db <- m * a * n * (n - 1) * ((2 * a) + n - 4)
  man111D_man012b <- n2t111Db / d3p2
  n3t111Db <- m * a * n * (n - 1) * ((2 * m) + n - 4)
  man111D_man102b <- n3t111Db / d3p2
  n4t111Db <- m * a * (a - 1) * n * (a + (2 * n) - 4)
  man111D_man021Db <- (1 / 4) * (n4t111Db / d3p2)
  n5t111Db <- m * a * (a - 1) * n * (a + (2 * n) - 4)
  man111D_man021Ub <- (1 / 4) * (n5t111Db / d3p2)
  n6t111Db <- m * a * (a - 1) * n * (a + (2 * n) - 4)
  man111D_man021Cb <- (1 / 2) * (n6t111Db / d3p2)
  n7t111Db <- (m * a * (a - 1) * n * (n - 1)) + (m * (m - 1) * a * n * (n - 1)) + (m * (m - 1) * a * (a - 1) * n)
  man111D_man111Db <- n7t111Db / d3p2

  n1t111Ub <- m * a * n * (n - 1) * (n - 2)
  man111U_man003b <- n1t111Ub / d3p2
  n2t111Ub <- m * a * n * (n - 1) * ((2 * a) + n - 4)
  man111U_man012b <- n2t111Ub / d3p2
  n3t111Ub <- m * a * n * (n - 1) * ((2 * m) + n - 4)
  man111U_man102b <- n3t111Ub / d3p2
  n4t111Ub <- m * a * (a - 1) * n * (a + (2 * n) - 4)
  man111U_man021Db <- (1 / 4) * (n4t111Ub / d3p2)
  n5t111Ub <- m * a * (a - 1) * n * (a + (2 * n) - 4)
  man111U_man021Ub <- (1 / 4) * (n5t111Ub / d3p2)
  n6t111Ub <- m * a * (a - 1) * n * (a + (2 * n) - 4)
  man111U_man021Cb <- (1 / 2) * (n6t111Ub / d3p2)
  n7t111Ub <- (m * a * (a - 1) * n * (n - 1)) + (m * (m - 1) * a * n * (n - 1)) + (m * (m - 1) * a * (a - 1) * n)
  man111U_man111Db <- n7t111Ub / d3p2
  n8t111Ub <- (m * a * (a - 1) * n * (n - 1)) + (m * (m - 1) * a * n * (n - 1)) + (m * (m - 1) * a * (a - 1) * n) # WARNING: table 4 in H-L 1976 is wrong?
  man111U_man111Ub <- n8t111Ub / d3p2

  n1t030Tb <- 0
  man030T_man003b <- n1t030Tb / d3p2
  n2t030Tb <- a * (a - 1) * (a - 2) * n * (n - 1)
  man030T_man012b <- (3 / 4) * (n2t030Tb / d3p2)
  n3t030Tb <- 0
  man030T_man102b <- n3t030Tb / d3p2
  n4t030Tb <- a * (a - 1) * (a - 2) * (a - 3) * n
  man030T_man021Db <- (3 / 8) * (n4t030Tb / d3p2)
  n5t030Tb <- a * (a - 1) * (a - 2) * (a - 3) * n
  man030T_man021Ub <- (3 / 8) * (n5t030Tb / d3p2)
  n6t030Tb <- a * (a - 1) * (a - 2) * (a - 3) * n
  man030T_man021Cb <- (3 / 4) * (n6t030Tb / d3p2)
  n7t030Tb <- m * a * (a - 1) * (a - 2) * n
  man030T_man111Db <- (3 / 4) * (n7t030Tb / d3p2)
  n8t030Tb <- m * a * (a - 1) * (a - 2) * n
  man030T_man111Ub <- (3 / 4) * (n8t030Tb / d3p2)
  n9t030Tb <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4)
  man030T_man030Tb <- (9 / 16) * (n9t030Tb / d3p2)

  n1t030Cb <- 0
  man030C_man003b <- (1 / 8) * (n1t030Cb / d3p2)
  n2t030Cb <- a * (a - 1) * (a - 2) * n * (n - 1)
  man030C_man012b <- (1 / 4) * (n2t030Cb / d3p2)
  n3t030Cb <- 0
  man030C_man102b <- (1 / 4) * n3t030Cb / d3p2
  n4t030Cb <- a * (a - 1) * (a - 2) * (a - 3) * n
  man030C_man021Db <- (1 / 8) * (n4t030Cb / d3p2)
  n5t030Cb <- a * (a - 1) * (a - 2) * (a - 3) * n
  man030C_man021Ub <- (1 / 8) * (n5t030Cb / d3p2)
  n6t030Cb <- a * (a - 1) * (a - 2) * (a - 3) * n
  man030C_man021Cb <- (1 / 4) * (n6t030Cb / d3p2)
  n7t030Cb <- m * a * (a - 1) * (a - 2) * n
  man030C_man111Db <- (1 / 4) * (n7t030Cb / d3p2)
  n8t030Cb <- m * a * (a - 1) * (a - 2) * n
  man030C_man111Ub <- (1 / 4) * (n8t030Cb / d3p2)
  n9t030Cb <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4)
  man030C_man030Tb <- (3 / 16) * (n9t030Cb / d3p2)
  n10t030Cb <- a * (a - 1) * (a - 2) * (a - 3) * (a - 4)
  man030C_man030Cb <- (1 / 16) * (n10t030Cb / d3p2)

  n1t201b <- m * (m - 1) * n * (n - 1) * (n - 2)
  man201_man003b <- n1t201b / d3p2
  n2t201b <- 2 * m * (m - 1) * a * n * (n - 1)
  man201_man012b <- n2t201b / d3p2
  n3t201b <- 2 * m * (m - 1) * n * (n - 1) * (m + n + 4)
  man201_man102b <- n3t201b / d3p2
  n4t201b <- m * (m - 1) * a * (a - 1) * n
  man201_man021Db <- (1 / 4) * (n4t201b / d3p2)
  n5t201b <- m * (m - 1) * a * (a - 1) * n
  man201_man021Ub <- (1 / 4) * (n5t201b / d3p2)
  n6t201b <- m * (m - 1) * a * (a - 1) * n
  man201_man021Cb <- (1 / 2) * (n6t201b / d3p2)
  n7t201b <- m * (m - 1) * a * n * (m + (2 * n) - 4)
  man201_man111Db <- n7t201b / d3p2
  n8t201b <- m * (m - 1) * a * n * (m + (2 * n) - 4)
  man201_man111Ub <- n8t201b / d3p2
  n9t201b <- 0
  man201_man030Tb <- n9t201b / d3p2
  n10t201b <- 0
  man201_man030Cb <- n10t201b / d3p2
  n11t201b <- m * (m - 1) * (m - 2) * n * (m + (4 * n) - 7)
  man201_man201b <- n11t201b / d3p2

  n1t120Db <- 0
  man120D_man003b <- n1t120Db / d3p2
  n2t120Db <- m * a * (a - 1) * n * (n - 1)
  man120D_man012b <- (1 / 2) * (n2t120Db / d3p2)
  n3t120Db <- m * a * (a - 1) * n * (n - 1)
  man120D_man102b <- (1 / 4) * (n3t120Db / d3p2)
  n4t120Db <- m * a * (a - 1) * (a - 2) * n
  man120D_man021Db <- (1 / 4) * (n4t120Db / d3p2)
  n5t120Db <- m * a * (a - 1) * (a - 2) * n
  man120D_man021Ub <- (1 / 4) * (n5t120Db / d3p2)
  n6t120Db <- m * a * (a - 1) * (a - 2) * n
  man120D_man021Cb <- (1 / 2) * (n6t120Db / d3p2)
  n7t120Db <- m * a * (a - 1) * n * (a + (2 * m) - 4)
  man120D_man111Db <- (1 / 4) * (n7t120Db / d3p2)
  n8t120Db <- m * a * (a - 1) * n * (a + (2 * m) - 4)
  man120D_man111Ub <- (1 / 4) * (n8t120Db / d3p2)
  n9t120Db <- m * a * (a - 1) * (a - 2) * (a - 3)
  man120D_man030Tb <- (3 / 8) * (n9t120Db / d3p2)
  n10t120Db <- m * a * (a - 1) * (a - 2) * (a - 3)
  man120D_man030Cb <- (1 / 8) * (n10t120Db / d3p2)
  n11t120Db <- m * (m - 1) * a * (a - 1) * n
  man120D_man201b <- (1 / 2) * (n11t120Db / d3p2)
  n12t120Db <- m * a * (a - 1) * (a - 2) * (a + (4 * m) - 7)
  man120D_man120Db <- (1 / 16) * (n12t120Db / d3p2)

  n1t120Ub <- 0
  man120U_man003b <- n1t120Ub / d3p2
  n2t120Ub <- m * a * (a - 1) * n * (n - 1)
  man120U_man012b <- (1 / 2) * (n2t120Ub / d3p2)
  n3t120Ub <- m * a * (a - 1) * n * (n - 1)
  man120U_man102b <- (1 / 4) * (n3t120Ub / d3p2)
  n4t120Ub <- m * a * (a - 1) * (a - 2) * n
  man120U_man021Db <- (1 / 4) * (n4t120Ub / d3p2)
  n5t120Ub <- m * a * (a - 1) * (a - 2) * n
  man120U_man021Ub <- (1 / 4) * (n5t120Ub / d3p2)
  n6t120Ub <- m * a * (a - 1) * (a - 2) * n
  man120U_man021Cb <- (1 / 2) * (n6t120Ub / d3p2)
  n7t120Ub <- m * a * (a - 1) * n * (a + (2 * m) - 4)
  man120U_man111Db <- (1 / 4) * (n7t120Ub / d3p2)
  n8t120Ub <- m * a * (a - 1) * n * (a + (2 * m) - 4)
  man120U_man111Ub <- (1 / 4) * (n8t120Ub / d3p2)
  n9t120Ub <- m * a * (a - 1) * (a - 2) * (a - 3)
  man120U_man030Tb <- (3 / 8) * (n9t120Ub / d3p2)
  n10t120Ub <- m * a * (a - 1) * (a - 2) * (a - 3)
  man120U_man030Cb <- (1 / 8) * (n10t120Ub / d3p2)
  n11t120Ub <- m * (m - 1) * a * (a - 1) * n
  man120U_man201b <- (1 / 2) * (n11t120Ub / d3p2)
  n12t120Ub <- m * a * (a - 1) * (a - 2) * (a + (4 * m) - 7)
  man120U_man120Db <- (1 / 16) * (n12t120Ub / d3p2)
  n13t120Ub <- m * a * (a - 1) * (a - 2) * (a + (4 * m) - 7)
  man120U_man120Ub <- (1 / 16) * (n13t120Ub / d3p2)

  n1t120Cb <- 0
  man120C_man003b <- n1t120Cb / d3p2
  n2t120Cb <- m * a * (a - 1) * n * (n - 1)
  man120C_man012b <- n2t120Cb / d3p2
  n3t120Cb <- m * a * (a - 1) * n * (n - 1)
  man120C_man102b <- (1 / 2) * (n3t120Cb / d3p2)
  n4t120Cb <- m * a * (a - 1) * (a - 2) * n
  man120C_man021Db <- (1 / 2) * (n4t120Cb / d3p2)
  n5t120Cb <- m * a * (a - 1) * (a - 2) * n
  man120C_man021Ub <- (1 / 2) * (n5t120Cb / d3p2)
  n6t120Cb <- m * a * (a - 1) * (a - 2) * n
  man120C_man021Cb <- n6t120Cb / d3p2
  n7t120Cb <- m * a * (a - 1) * n * (a + (2 * m) - 4)
  man120C_man111Db <- (1 / 2) * (n7t120Cb / d3p2)
  n8t120Cb <- m * a * (a - 1) * n * (a + (2 * m) - 4)
  man120C_man111Ub <- (1 / 2) * (n8t120Cb / d3p2)
  n9t120Cb <- m * a * (a - 1) * (a - 2) * (a - 3)
  man120C_man030Tb <- (3 / 4) * (n9t120Cb / d3p2)
  n10t120Cb <- m * a * (a - 1) * (a - 2) * (a - 3)
  man120C_man030Cb <- (1 / 4) * (n10t120Cb / d3p2)
  n11t120Cb <- m * (m - 1) * a * (a - 1) * n
  man120C_man201b <- n11t120Cb / d3p2
  n12t120Cb <- m * a * (a - 1) * (a - 2) * (a + (4 * m) - 7)
  man120C_man120Db <- (1 / 8) * (n12t120Cb / d3p2)
  n13t120Cb <- m * a * (a - 1) * (a - 2) * (a + (4 * m) - 7)
  man120C_man120Ub <- (1 / 8) * (n13t120Cb / d3p2)
  n14t120Cb <- m * a * (a - 1) * (a - 2) * (a + (4 * m) - 7)
  man120C_man120Cb <- (1 / 4) * (n14t120Cb / d3p2)

  n1t210b <- 0
  man210_man003b <- n1t210b / d3p2
  n2t210b <- m * (m - 1) * a * n * (n - 1)
  man210_man012b <- n2t210b / d3p2
  n3t210b <- 2 * m * (m - 1) * a * n * (n - 1)
  man210_man102b <- n3t210b / d3p2
  n4t210b <- m * (m - 1) * a * (a - 1) * n
  man210_man021Db <- (1 / 2) * (n4t210b / d3p2)
  n5t210b <- m * (m - 1) * a * (a - 1) * n
  man210_man021Ub <- (1 / 2) * (n5t210b / d3p2)
  n6t210b <- m * (m - 1) * a * (a - 1) * n
  man210_man021Cb <- n6t210b / d3p2
  n7t210b <- m * (m - 1) * a * n * (m + (2 * a) - 4)
  man210_man111Db <- n7t210b / d3p2
  n8t210b <- m * (m - 1) * a * n * (m + (2 * a) - 4)
  man210_man111Ub <- n8t210b / d3p2
  n9t210b <- m * (m - 1) * a * (a - 1) * (a - 2)
  man210_man030Tb <- (3 / 4) * (n9t210b / d3p2)
  n10t210b <- m * (m - 1) * a * (a - 1) * (a - 2)
  man210_man030Cb <- (1 / 4) * (n10t210b / d3p2)
  n11t210b <- 4 * m * (m - 1) * (m - 2) * a * n
  man210_man201b <- n11t210b / d3p2
  n12t210b <- m * (m - 1) * a * (a - 1) * (m + a - 4)
  man210_man120Db <- (1 / 2) * (n12t210b / d3p2)
  n13t210b <- m * (m - 1) * a * (a - 1) * (m + a - 4)
  man210_man120Ub <- (1 / 2) * (n13t210b / d3p2) # WRONG IN H-L? 1/2 rather than 1/8
  n14t210b <- m * (m - 1) * a * (a - 1) * (m + a - 4)
  man210_man120Cb <- n14t210b / d3p2
  n15t210b <- m * (m - 1) * (m - 2) * a * (m + (4 * a) - 7)
  man210_man210b <- n15t210b / d3p2

  n1t300b <- 0
  man300_man003b <- n1t300b / d3p2
  n2t300b <- 0
  man300_man012b <- n2t300b / d3p2
  n3t300b <- m * (m - 1) * (m - 2) * n * (n - 1)
  man300_man102b <- n3t300b / d3p2
  n4t300b <- 0
  man300_man021Db <- n4t300b / d3p2
  n5t300b <- 0
  man300_man021Ub <- n5t300b / d3p2
  n6t300b <- 0
  man300_man021Cb <- n6t300b / d3p2
  n7t300b <- m * (m - 1) * (m - 2) * a * n
  man300_man111Db <- n7t300b / d3p2
  n8t300b <- m * (m - 1) * (m - 2) * a * n
  man300_man111Ub <- n8t300b / d3p2
  n9t300b <- 0
  man300_man030Tb <- n9t300b / d3p2
  n10t300b <- 0
  man300_man030Cb <- n10t300b / d3p2
  n11t300b <- 2 * m * (m - 1) * (m - 2) * (m - 3) * n
  man300_man201b <- n11t300b / d3p2
  n12t300b <- m * (m - 1) * (m - 2) * a * (a - 1)
  man300_man120Db <- (1 / 4) * (n12t300b / d3p2)
  n13t300b <- m * (m - 1) * (m - 2) * a * (a - 1)
  man300_man120Ub <- (1 / 4) * (n13t300b / d3p2)
  n14t300b <- m * (m - 1) * (m - 2) * a * (a - 1)
  man300_man120Cb <- (1 / 2) * (n14t300b / d3p2)
  n15t300b <- 2 * m * (m - 1) * (m - 2) * (m - 3) * a
  man300_man210b <- n15t300b / d3p2
  n16t300b <- m * (m - 1) * (m - 2) * (m - 3) * (m - 4)
  man300_man300b <- n16t300b / d3p2

  return(
    list(
      # VAR
      man003_man003b = man003_man003b, man012_man012b = man012_man012b,
      man102_man102b = man102_man102b, man021D_man021Db = man021D_man021Db,
      man021U_man021Ub = man021U_man021Ub, man021C_man021Cb = man021C_man021Cb,
      man111D_man111Db = man111D_man111Db, man111U_man111Ub = man111U_man111Ub,
      man030T_man030Tb = man030T_man030Tb, man030C_man030Cb = man030C_man030Cb,
      man201_man201b = man201_man201b, man120D_man120Db = man120D_man120Db,
      man120U_man120Ub = man120U_man120Ub, man120C_man120Cb = man120C_man120Cb,
      man210_man210b = man210_man210b, man300_man300b = man300_man300b,

      # COV
      man012_man003b = man012_man003b, man102_man003b = man102_man003b,
      man102_man012b = man102_man012b, man021D_man003b = man021D_man003b,
      man021D_man012b = man021D_man012b, man021D_man102b = man021D_man102b,
      man021U_man003b = man021U_man003b, man021U_man012b = man021U_man012b,
      man021U_man102b = man021U_man102b, man021U_man021Db = man021U_man021Db,
      man021C_man003b = man021C_man003b, man021C_man012b = man021C_man012b,
      man021C_man102b = man021C_man102b, man021C_man021Db = man021C_man021Db,
      man021C_man021Ub = man021C_man021Ub, man111D_man003b = man111D_man003b,
      man111D_man012b = man111D_man012b, man111D_man102b = man111D_man102b,
      man111D_man021Db = man111D_man021Db, man111D_man021Ub = man111D_man021Ub,
      man111D_man021Cb = man111D_man021Cb, man111U_man003b = man111U_man003b,
      man111U_man012b = man111U_man012b, man111U_man102b = man111U_man102b,
      man111U_man021Db = man111U_man021Db, man111U_man021Ub = man111U_man021Ub,
      man111U_man021Cb = man111U_man021Cb, man111U_man111Db = man111U_man111Db,
      man030T_man003b = man030T_man003b, man030T_man012b = man030T_man012b,
      man030T_man102b = man030T_man102b, man030T_man021Db = man030T_man021Db,
      man030T_man021Ub = man030T_man021Ub, man030T_man021Cb = man030T_man021Cb,
      man030T_man111Db = man030T_man111Db, man030T_man111Ub = man030T_man111Ub,
      man030C_man003b = man030C_man003b, man030C_man012b = man030C_man012b,
      man030C_man102b = man030C_man102b, man030C_man021Db = man030C_man021Db,
      man030C_man021Ub = man030C_man021Ub, man030C_man021Cb = man030C_man021Cb,
      man030C_man111Db = man030C_man111Db, man030C_man111Ub = man030C_man111Ub,
      man030C_man030Tb = man030C_man030Tb, man201_man003b = man201_man003b,
      man201_man012b = man201_man012b,

      # COV, NOTE 1
      man201_man102b = man201_man102b, man201_man021Db = man201_man021Db,
      man201_man021Ub = man201_man021Ub, man201_man021Cb = man201_man021Cb,
      man201_man111Db = man201_man111Db, man201_man111Ub = man201_man111Ub,
      man201_man030Tb = man201_man030Tb, man201_man030Cb = man201_man030Cb,
      man120D_man003b = man120D_man003b, man120D_man012b = man120D_man012b,
      man120D_man102b = man120D_man102b, man120D_man021Db = man120D_man021Db,
      man120D_man021Ub = man120D_man021Ub, man120D_man021Cb = man120D_man021Cb,
      man120D_man111Db = man120D_man111Db, man120D_man111Ub = man120D_man111Ub,
      man120D_man030Tb = man120D_man030Tb, man120D_man030Cb = man120D_man030Cb,
      man120D_man201b = man120D_man201b, man120U_man003b = man120U_man003b,
      man120U_man012b = man120U_man012b, man120U_man102b = man120U_man102b,
      man120U_man021Db = man120U_man021Db, man120U_man021Ub = man120U_man021Ub,
      man120U_man021Cb = man120U_man021Cb, man120U_man111Db = man120U_man111Db,
      man120U_man111Ub = man120U_man111Ub, man120U_man030Tb = man120U_man030Tb,
      man120U_man030Cb = man120U_man030Cb, man120U_man201b = man120U_man201b,
      man120U_man120Db = man120U_man120Db, man120C_man003b = man120C_man003b,
      man120C_man012b = man120C_man012b, man120C_man102b = man120C_man102b,
      man120C_man021Db = man120C_man021Db, man120C_man021Ub = man120C_man021Ub,
      man120C_man021Cb = man120C_man021Cb, man120C_man111Db = man120C_man111Db,
      man120C_man111Ub = man120C_man111Ub, man120C_man030Tb = man120C_man030Tb,
      man120C_man030Cb = man120C_man030Cb, man120C_man201b = man120C_man201b,
      man120C_man120Db = man120C_man120Db, man120C_man120Ub = man120C_man120Ub,
      man210_man003b = man210_man003b, man210_man012b = man210_man012b,
      man210_man102b = man210_man102b, man210_man021Db = man210_man021Db,
      man210_man021Ub = man210_man021Ub, man210_man021Cb = man210_man021Cb,
      man210_man111Db = man210_man111Db, man210_man111Ub = man210_man111Ub,
      man210_man030Tb = man210_man030Tb, man210_man030Cb = man210_man030Cb,
      man210_man201b = man210_man201b, man210_man120Db = man210_man120Db,
      man210_man120Ub = man210_man120Ub, man210_man120Cb = man210_man120Cb,
      man300_man003b = man300_man003b, man300_man012b = man300_man012b,
      man300_man102b = man300_man102b, man300_man021Db = man300_man021Db,
      man300_man021Ub = man300_man021Ub, man300_man021Cb = man300_man021Cb,
      man300_man111Db = man300_man111Db, man300_man111Ub = man300_man111Ub,

      # COV, NOTE 2
      man300_man030Tb = man300_man030Tb, man300_man030Cb = man300_man030Cb,
      man300_man201b = man300_man201b, man300_man120Db = man300_man120Db,
      man300_man120Ub = man300_man120Ub, man300_man120Cb = man300_man120Cb,
      man300_man210b = man300_man210b
    )
  )
}
