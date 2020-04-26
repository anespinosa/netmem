L <- sum(SC)/(dim(SC)[1]*(dim(SC)[1]-1)) # density (directed)

round(mean(SC,na.rm=TRUE), 3) # Grand mean degree
# represent the overall level of relations and heterogeneity among the nodes
round(sd(SC,na.rm=TRUE), 3)

sum(SC,na.rm=TRUE)/ ( nrow(SC)*(nrow(SC)-1) ) # directed density
sum( SC[upper.tri(SC)] )/( nrow(SC)*(nrow(SC)-1)/2 ) # undirected density

# Nodal Heterogeneity can be explored with row and column means
rmean <- rowMeans(SC, na.rm=TRUE)
cmean <- colMeans(SC, na.rm=TRUE)
mean(rmean) # mean outdegree
sd(rmean)
mean(cmean) # mean indegree
sd(cmean)
# standard deviations, histograms or tables of means or degrees
# correlations and scatterplots of row versus column means or degrees.
cor(rmean, cmean) # correlation indegree-outdegree

# ANOVA-style decomposition of a sociomatrix known as the social 
# relations model (SRM) (Warner et al., 1979; Wong, 1982)
# Additive decomposition of a sociomatrix
# correspond to the well-known ANOVA decompostion of two-way data
mu <- mean(SC,na.rm=TRUE)
a <- rowMeans(SC, na.rm=TRUE) - mu
b <- colMeans(SC, na.rm=TRUE) - mu
Yadd <- mu + outer(a,b,"+")
mean( (SC-Yadd)^2,na.rm=TRUE ) / mean( (SC - mu)^2,na.rm=TRUE )
# A good model if variability is approximately additive/εˆi,j’s 
# are small or patternless.
# A bad model otherwise.
