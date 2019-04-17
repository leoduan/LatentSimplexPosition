library(tensorflow)
# library(reticulate)

setwd("~/git/SimplexEmbedding/")

Y = cbind( c(rnorm(200), rnorm(200)+1), c(rnorm(200), rnorm(200)+1))

D<- as.matrix(dist(Y))

S<- exp(-D)

#first stage: similarity matrix
image(S)

source("LatentSimplex.r")

SC_fit<- SimplexClust(S,lambda = 0)

require(ggplot2)

df =  data.frame(Y1=Y[,1],Y2=Y[,2], prob_assigning_to_cluster1 =SC_fit$P[,1] )

#estimated co-assignment probability matrix
image(SC_fit$A)

#plot the uncertainty
ggplot(df)+ geom_point(aes(Y1,Y2,col=prob_assigning_to_cluster1))+
  scale_colour_gradientn(colours = rev(rainbow(2)))




