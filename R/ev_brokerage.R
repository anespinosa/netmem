Sys.getlocale('LC_CTYPE')
Sys.setlocale(category = 'LC_ALL', 'es_ES.UTF-8')

rm(list=ls())
setwd("/Users/anespinosa/Downloads/")
library(igraph)

#----------------------------------------------------------------------------#
# GRANOVETTER'S HYPOTHETICAL NETWORK
weakgrano <- graph.formula(1-2,1-3, 1-24, 2-3, 2-4, 3-4,3-5, 4-5, 4-6, 5-6,
                           6-7, 7-8, 8-9, 8-10, 8-11, 8-14, 9-10, 10-11, 11-12,
                           11-12, 11-13, 11-14, 12-14, 12-13, 13-14, 13-15,
                           15-16, 15-17, 16-17, 17-18, 18-19, 18-20, 18-21,
                           19-20, 20-21, 20-22, 20-25, 22-25, 22-23, 23-25, 
                           23-24, 24-25)
plot(weakgrano, vertex.size=4)

vcount(weakgrano) %% 2 == 0

# CREATE EV BROKERAGE FOR UNDIRECTED GRAPHS
library(igraph)
ev_brokerage = function(graph) 
{
  if(!is_igraph(graph)) {
    stop("Not a graph object")
  }
  g1 = graph
  g1$bet <- (betweenness(g1)*2)
  g1$deg <- degree(g1)
  vf = ifelse(g1$deg>1, (g1$bet + vcount(g1)-1), g1$bet) #IF 0/0 = NA; should g1$bet=0? in the ifelse condition
  vf/g1$deg 
}
ev_brokerage(weakgrano)


#----------------------------------------------------------------------------#
# ADD NORMALIZATION TO THE EV BROKERAGE FOR UNDIRECTED GRAPHS
ev_brokerage = function(graph, normalization=TRUE) 
{
  if(!is_igraph(graph)) {
    stop("Not a graph object")
  }
  g1 = graph
  g1$bet <- (betweenness(g1)*2)
  g1$deg <- degree(g1)
  vf = ifelse(g1$deg>1, (g1$bet + vcount(g1)-1), g1$bet)
  vf/g1$deg
  
  if(normalization){
    vmax = ifelse(vcount(graph) %% 2 != 0,
           (((vcount(graph)^2)-1)/4), # odd
           (((vcount(graph)^2)-2)/4)) # even
    ifelse(g1$deg > 2, ((vf/g1$deg)/vmax)*100, 0)
  }
  else{vf/g1$deg}
}

# CHECK VERTEX 9, 12, 16, 19, 21 
ev_brokerage(weakgrano, normalization=T)
ev_brokerage(weakgrano, normalization=F) # G IS CONNECTED!

# IN CASE IS DISCONNECTED IT SHOULD BE ADDED A NEW MEASURE!

#----------------------------------------------------------------------------#
# CAMPNET DATASET
camp= graph_from_literal(LEE++BERT, LEE++BRAZEY, LEE++STEVE,
                         BRAZEY-+BERT, BERT++STEVE, BRAZEY-+STEVE,
                         BERT++STEVE, BERT++RUSS, RUSS++STEVE,
                         GERY-+STEVE, GERY++RUSS, JOHN-+RUSS,
                         JOHN-+GERY, GERY-+MICHAEL, BILL-+MICHAEL,
                         MICHAEL++DON, MICHAEL++HARRY, MICHAEL-+HOLLY,
                         BILL-+DON, BILL-+HARRY, HARRY++DON, DON++HARRY,
                         HARRY-+HOLLY, HOLLY++PAT, HOLLY-+PAM, PAT++CAROL,
                         PAT++JENNIE, CAROL-+PAM, CAROL++PAULINE, JOHN-+PAULINE,
                         PAULINE-+PAT, PAULINE++PAM, ANN-+PAULINE, ANN++PAM,
                         ANN++JENNIE, PAM++JENNIE, DON++HOLLY)
plot(camp, verte.size=0.7, edge.arrow.size=0.2, vertex.label.cex=0.5)

degree(camp, mode=c("in"))
degree(camp, mode=c("out"))

#----------------------------------------------------------------------------#
# DIRECTED GRAPH
ev_brokerage = function(graph, directed=TRUE) 
{
  if(!is_igraph(graph)) {
    stop("Not a graph object")
  }
  g1 = graph
  g1$bet <- (betweenness(g1)*2)
  g1$deg <- degree(g1)
  vf = ifelse(g1$deg>1, (g1$bet + vcount(g1)-1), g1$bet)
  vf/g1$deg
  
  if(directed){
    if(!is.directed(graph)){
      stop("Graph is not directed")
    }
    g1 = graph
    g1$bet <- (betweenness(g1, directed = TRUE)*2)
    g1$deg1 <- degree(g1, mode=c("in"))
    g1$deg2 <- degree(g1, mode=c("out"))
    vf1 <- ifelse(g1$deg1>1, (g1$bet + vcount(g1)-1), g1$bet) #  number of vertices that can reach v.
    vf2 <- ifelse(g1$deg2>1, (g1$bet + vcount(g1)-1), g1$bet)
    list(vf1/g1$deg1,vf2/g1$deg2) # Divide each non-zero sum by the in-degree/out-degree of v.
  }
  else{vf/g1$deg}
  
}
ev_brokerage(weakgrano, directed=T)

ev_brokerage = function(graph, directed=TRUE)  #REVIEW
{
  if(!is_igraph(graph)) {
    stop("Not a graph object")
  }
  g1 = graph
  g1$bet <- (betweenness(g1)*2)
  g1$deg <- degree(g1)
  vf = ifelse(g1$deg>1, (g1$bet + vcount(g1)-1), g1$bet)
  vf/g1$deg
  
  if(directed){
    if(!is.directed(graph)){
      stop("Graph is not directed")
    }
    g1 = graph
    g1$bet <- (betweenness(g1, directed = TRUE)*2)
    g1$deg1 <- degree(g1, mode=c("in"))
    g1$deg2 <- degree(g1, mode=c("out"))
    vf1 <- ifelse(g1$deg1>1, (g1$bet + vcount(g1)-1), g1$bet)

    graph$in_ev = vf1/g1$deg1
    
    vf2 <- ifelse(g1$deg2>1, (g1$bet + vcount(g1)-1), g1$bet)
    
    graph$ou_ev = vf2/g1$deg2
  }
  else{vf/g1$deg}
  
}

ob <- ev_brokerage(camp, directed=T)
ob

#----------------------------------------------------------------------------#
# ERROR NORMALIZATION
ev_brokerage = function(graph, directed=TRUE, normalization=TRUE) # REVIEW
{
  if(!is_igraph(graph)) {
    stop("Not a graph object")
  }
  g1 = graph
  g1$bet <- (betweenness(g1)*2)
  g1$deg <- degree(g1)
  vf = ifelse(g1$deg>1, (g1$bet + vcount(g1)-1), g1$bet)
  vf/g1$deg
  
  if(normalization){
    vmax = ifelse(vcount(graph) %% 2 != 0,
                  (((vcount(graph)^2)-1)/4),
                  (((vcount(graph)^2)-2)/4))
    ((vf/g1$deg)/vmax)*100
  }
  else{vf/g1$deg} # ERROR
  
  if(directed){
    if(!is.directed(graph)){
      stop("Graph is not directed")
    }
    g1 = graph
    g1$bet <- (betweenness(g1, directed = TRUE)*2)
    g1$deg1 <- degree(g1, mode=c("in"))
    g1$deg2 <- degree(g1, mode=c("out"))
    vf1 <- ifelse(g1$deg1>1, (g1$bet + vcount(g1)-1), g1$bet)
    vf2 <- ifelse(g1$deg2>1, (g1$bet + vcount(g1)-1), g1$bet)
    list(vf1/g1$deg1,vf2/g1$deg2)
  }
  else{vf/g1$deg}
  
}

# DEFAULT
ev_brokerage(weakgrano, directed=F, normalization=F) # ok
ev_brokerage(camp, directed=F, normalization=F) # ok

# DIRECTED
ev_brokerage(weakgrano, directed=T, normalization=F) # ok
ev_brokerage(camp, directed=T, normalization=F) #ok

# NORMALIZED
ev_brokerage(weakgrano, directed=F, normalization=T) # ERROR
ev_brokerage(camp, directed=F, normalization=T) # ERROR


#----------------------------------------------------------------------------#
# ERROR NORMALIZATION
ev_brokerage = function(graph, directed=TRUE, normalization=TRUE) # REVIEW
{
  if(!is_igraph(graph)) {
    stop("Not a graph object")
  }
  g1 = graph
  g1$bet <- (betweenness(g1)*2)
  g1$deg <- degree(g1)
  vf = ifelse(g1$deg>1, (g1$bet + vcount(g1)-1), g1$bet)
  final = vf/g1$deg
  final
  
  if(directed){
    if(!is.directed(graph)){
      stop("Graph is not directed")
    }
    g1 = graph
    g1$bet <- (betweenness(g1, directed = TRUE)*2)
    g1$deg1 <- degree(g1, mode=c("in"))
    g1$deg2 <- degree(g1, mode=c("out"))
    vf1 <- ifelse(g1$deg1>1, (g1$bet + vcount(g1)-1), g1$bet)
    vf2 <- ifelse(g1$deg2>1, (g1$bet + vcount(g1)-1), g1$bet)
    list(vf1/g1$deg1,vf2/g1$deg2)
  }
  else{final}
  
  if(normalization){
    vmax = ifelse(vcount(graph) %% 2 != 0,
                  (((vcount(graph)^2)-1)/4),
                  (((vcount(graph)^2)-2)/4))
    
    ifelse(directed==T, 
           list(((vf1/g1$deg1)/vmax)*100,
                ((vf2/g1$deg2)/vmax)*100),
           ((vf/g1$deg)/vmax)*100)
    
  }
  else{ifelse(directed==T, list(vf1/g1$deg1,vf2/g1$deg2), final)} # ERROR
  
}

# DEFAULT
ev_brokerage(weakgrano, normalization=F, directed=F) # ok
ev_brokerage(camp, normalization=F, directed=F) # ok

# DIRECTED
ev_brokerage(weakgrano, directed=T, normalization=F) # ok
ev_brokerage(camp, normalization=F, directed=T) # ERROR

# NORMALIZED
ev_brokerage(weakgrano, directed=T, normalization=T) # ERROR
ev_brokerage(camp, directed=T, normalization=T) # ERROR

# biblioNetwork

#----------------------------------------------------------------------------#
# OTHER PRACTICAL NETWORK
load("/Users/anespinosa/Dropbox/phd_manchester/005data/008database_ma/002edges/networks.Rda")
load("/Users/anespinosa/Dropbox/phd_manchester/005data/008database_ma/001vertex/vertex.Rda")

library(igraph)
g2005 <- graph.adjacency(matrix2005)
g2006 <- graph.adjacency(matrix2006)

largest_component <- function(igraph) {
  comps <- components(igraph, mode = "weak") #conexion directa V1 %->% V2
  gr <- groups(comps)
  sizes <- vapply(gr, length, 1L)
  induced_subgraph(igraph, gr[[ which.max(sizes) ]])
}
gcomp <- largest_component(g2006)
gcomp
