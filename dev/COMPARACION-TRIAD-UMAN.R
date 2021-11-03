library(microbenchmark)

microbenchmark(
  triad_uman(krackhardt_friends),
  triad_uman_2(krackhardt_friends),
  times = 100
)
