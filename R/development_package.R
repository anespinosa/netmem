# https://bookdown.org/rdpeng/RProgDA/
library(pryr)
mem_used()
ls()
object_size()
library(magrittr)
sapply(ls(), function(x) object.size(get(x))) %>% sort %>% tail(5)

str(.Machine)
log2(2147483647) # 31 bits to encode
gc()

## Generate a sequence based on length of 'x'
x <- c("a", "b", "c", "d")
for(i in seq_along(x)) {   
  print(x[i])
}

for(letter in x) {
  print(letter)
}

for(i in 1:4) print(x[i]) # only for one line loop

# Nested for loops 
x <- matrix(1:6, 2, 3)
x
for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }   
}


# https://bookdown.org/rdpeng/RProgDA/functional-programming.html