Illustration of the Simplex Clustering
================
Leo Duan

## Generate Data

Simulate two clusters of data

``` r
library(tensorflow)
library(ggplot2)

setwd("~/git/SimplexEmbedding/")

Y = cbind( c(rnorm(200), rnorm(200)+1), c(rnorm(200), rnorm(200)+1))

ground_truth= rep(c(1:2),each=200)
df1 =  data.frame(Y1=Y[,1],Y2=Y[,2] , label= as.factor(ground_truth))

#plot the uncertainty
ggplot(df1)+ geom_point(aes(Y1,Y2,col=label))
```

![](demo_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

## Compute the similarity

``` r
D<- as.matrix(dist(Y))

#empirical selection of bandwidth based on quantile in each row
b = sqrt(apply(D,1, function(x) quantile(x,prob=0.3)))

S<- exp(-t(D/b)/b)

#first stage: similarity matrix
image(S)
```

![](demo_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

## Fit the model

``` r
source("LatentSimplex.r")

SC_fit<- SimplexClust(S,lambda = 0)
```

    ## 20 - 110431.1 
    ## 40 - 109671.8 
    ## 60 - 108943.3 
    ## 80 - 108554 
    ## 100 - 108264.6 
    ## 120 - 107904 
    ## 140 - 107489.9 
    ## 160 - 107135.1 
    ## 180 - 106901.5 
    ## 200 - 106772 
    ## 220 - 106706.3 
    ## 240 - 106674.5 
    ## 260 - 106660 
    ## 280 - 106653.5 
    ## 300 - 106650.9 
    ## 320 - 106649.8 
    ## 340 - 106649.5 
    ## 360 - 106649.3 
    ## 380 - 106649.3 
    ## 400 - 106649.3 
    ## 420 - 106649.3 
    ## 440 - 106649.3 
    ## 460 - 106649.3 
    ## 480 - 106649.3 
    ## 500 - 106649.3 
    ## 520 - 106649.3 
    ## 540 - 106649.3 
    ## 560 - 106649.3 
    ## 580 - 106649.3 
    ## 600 - 106649.3 
    ## 620 - 106649.3 
    ## 640 - 106649.3 
    ## 660 - 106649.3 
    ## 680 - 106649.3 
    ## 700 - 106649.3 
    ## 720 - 106649.3 
    ## 740 - 106649.3 
    ## 760 - 106649.3 
    ## 780 - 106649.3 
    ## 800 - 106649.3 
    ## 820 - 106649.3 
    ## 840 - 106649.3 
    ## 860 - 106649.3 
    ## 880 - 106649.3 
    ## 900 - 106649.3 
    ## 920 - 106649.3 
    ## 940 - 106649.3 
    ## 960 - 106649.3 
    ## 980 - 106649.3 
    ## 1000 - 106649.3 
    ## 1020 - 106649.3 
    ## 1040 - 106649.3 
    ## 1060 - 106649.3 
    ## 1080 - 106649.3 
    ## 1100 - 106649.3 
    ## 1120 - 106649.3 
    ## 1140 - 106649.3 
    ## 1160 - 106649.3 
    ## 1180 - 106649.3 
    ## 1200 - 106649.3 
    ## 1220 - 106649.3 
    ## 1240 - 106649.3 
    ## 1260 - 106649.3 
    ## 1280 - 106649.3 
    ## 1300 - 106649.3 
    ## 1320 - 106649.3 
    ## 1340 - 106649.3 
    ## 1360 - 106649.3 
    ## 1380 - 106649.3 
    ## 1400 - 106649.3 
    ## 1420 - 106649.3 
    ## 1440 - 106649.3 
    ## 1460 - 106649.3 
    ## 1480 - 106649.3 
    ## 1500 - 106649.3 
    ## 1520 - 106649.3 
    ## 1540 - 106649.3 
    ## 1560 - 106649.3 
    ## 1580 - 106649.3 
    ## 1600 - 106649.3 
    ## 1620 - 106649.3 
    ## 1640 - 106649.3 
    ## 1660 - 106649.3 
    ## 1680 - 106649.3 
    ## 1700 - 106649.3 
    ## 1720 - 106649.3 
    ## 1740 - 106649.3 
    ## 1760 - 106649.3 
    ## 1780 - 106649.3 
    ## 1800 - 106649.3 
    ## 1820 - 106649.3 
    ## 1840 - 106649.3 
    ## 1860 - 106649.3 
    ## 1880 - 106649.3 
    ## 1900 - 106649.3 
    ## 1920 - 106649.3 
    ## 1940 - 106649.3 
    ## 1960 - 106649.3 
    ## 1980 - 106649.3 
    ## 2000 - 106649.3

``` r
require(ggplot2)

df =  data.frame(Y1=Y[,1],Y2=Y[,2], prob_assigning_to_cluster1 =SC_fit$P[,1] )

#estimated co-assignment probability matrix
image(SC_fit$A)
```

![](demo_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#plot the uncertainty
ggplot(df)+ geom_point(aes(Y1,Y2,col=prob_assigning_to_cluster1))+
  scale_colour_gradientn(colours = rev(rainbow(2)))
```

![](demo_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->
