## code to prepare `campnet` dataset

# Camp 92 (Borgatti, Bernard, Pelto and Ryan): rank of each person by how much
# the respondent (row) interacted with them since the beginning of the course,
# collected at the end of the second week (1 = most interaction, 17 = least).
# Campnet keeps the three highest ranks of each respondent, as in UCINET. The
# network reproduces the betweenness of Everett and Borgatti (2026, Table 5).

nodes <- c(
  "HOLLY", "BRAZEY", "CAROL", "PAM", "PAT", "JENNIE", "PAULINE", "ANN", "MICHAEL", "BILL", "LEE", "DON", "JOHN", "HARRY", "GERY", "STEVE", "BERT", "RUSS"
)

ranks <- matrix(
  c(
     0,  7, 17,  1,  3, 13,  8,  6,  4, 12, 15,  2, 14,  5, 16, 11,  9, 10,
     4,  0,  8,  5, 10,  9,  7,  6, 15, 17,  1, 11, 13, 16, 14,  2,  3, 12,
    16, 10,  0,  1,  2,  4,  3, 11,  7, 17, 13, 14,  5, 15, 12,  6,  8,  9,
     4,  7,  5,  0,  6,  3,  1,  2,  9, 16, 13, 10,  8, 11, 15, 14, 12, 17,
     2,  6,  3, 13,  0,  1,  5,  4, 14, 17,  9, 15, 12, 16, 10,  8,  7, 11,
     9,  8,  5,  3,  1,  0,  4,  2, 14, 17, 12, 11, 16, 15, 13,  6,  7, 10,
    10,  6,  3,  1,  2,  5,  0,  4, 13, 15, 11, 16,  8, 17, 14,  9,  7, 12,
     5,  8,  4,  2,  7,  1,  3,  0,  9, 15, 14, 12,  6, 10, 16, 11, 13, 17,
     3, 15,  5,  6,  7, 13, 11, 10,  0, 14, 17,  1, 12,  2,  4,  9,  8, 16,
     8,  9, 14, 15, 11, 17, 13, 16,  1,  0, 10,  2, 12,  3,  7,  5,  6,  4,
    10,  3,  6, 13,  4, 14, 15, 17, 11, 16,  0,  9,  8,  7, 12,  1,  2,  5,
     2, 13, 15,  5,  6,  7,  8, 12,  1, 17, 10,  0, 14,  3,  4, 11,  9, 16,
    16,  5,  4,  8, 12, 14,  1,  9, 10, 13, 17, 11,  0,  7,  2,  6, 15,  3,
     2, 16, 15, 10, 11, 17, 14,  8,  1,  5,  6,  3,  4,  0,  9,  7, 12, 13,
    13,  6,  9, 14, 11, 15, 12, 16,  3, 17,  8,  7,  5, 10,  0,  2,  4,  1,
    14,  5,  6, 15,  9, 11,  8, 13, 12, 17,  2,  7, 10, 16,  4,  0,  1,  3,
     5,  4, 10,  7, 15, 11,  8,  9, 14, 13,  2, 12, 17, 16,  6,  1,  0,  3,
     4,  8,  9, 15, 13,  7, 17, 16, 12, 11,  5, 10,  6, 14,  1,  3,  2,  0
  ),
  ncol = 18, byrow = TRUE, dimnames = list(nodes, nodes)
)

# Top three choices of each respondent
network <- 1 * (ranks >= 1 & ranks <= 3)

# gender: 1 = woman, 2 = man; role: 1 = participant, 2 = instructor
attributes <- data.frame(
  gender = c(1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2),
  role = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2),
  row.names = nodes
)

campnet <- list(
  network = network,
  ranks = ranks,
  attributes = attributes
)

usethis::use_data(campnet, overwrite = TRUE)
