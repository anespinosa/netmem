### CONNECTEDNESS (WHOLE NETWORK: UCINET):
(sum((sna::reachability(network::as.network(SC)))))/(vcount(g1sc)*(vcount(g1sc)-1)) # same as UCINET!

### FRAGMENTATION:
1-(sum((sna::reachability(network::as.network(SC)))))/(vcount(g1sc)*(vcount(g1sc)-1))
#1-(sum(components(g1sc, mode=c("weak"))$csize*(components(g1sc, mode=c("weak"))$csize-1))/(vcount(g1sc)*(vcount(g1sc)-1))) # BORGATTI??
#1-sna::connectedness(network::as.network(SC)) 

# Hirschman-Herfindahl index, or the Herfindahl index?? Borgatti
#a <- 1-sum(components(g1sc, mode=c("strong"))$csize/vcount(g1sc)^2)
#b <- ((1-(vcount(g1sc))^-1))
#a/b

h_index = function(cites) {
  if(max(cites) == 0) return(0) # assuming this is reasonable
  cites = cites[order(cites, decreasing = TRUE)]
  tail(which(cites >= seq_along(cites)), 1)
}
h_index(degree(g1sc, mode=c("in")))
h_index(rowSums(SC)) # weighted

### CONNECTEDNESS (KRACKHARDT GTD MEASURES: UCINET)
sna::connectedness(network::as.network(SC)) # same as Borgatti equation for components, but using reachability? Problems with directionality or missing data?
(sum(components(g1sc, mode=c("weak"))$csize*(components(g1sc, mode=c("weak"))$csize-1))/(vcount(g1sc)*(vcount(g1sc)-1))) # BORGATTI??
# krackhardt measures:
sna::hierarchy(network::as.network(SC), measure=c("reciprocity"))
sna::hierarchy(network::as.network(SC), measure=c("krackhardt")) # same than UCINET!
sna::efficiency(network::as.network(SC)) # slighly different from UCINET!
sna::lubness(network::as.network(SC)) #different from UCINET!
#mean(sna::infocent(network::as.network(SC), rescale = T))
