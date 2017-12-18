# ex3
---
title: "EX3"
author: "Matan"
date: "December 15, 2017"
output: html_document
---


Setting a working directory:

```{r setup}
folder = 'C:/ex3'
setwd(folder)

#Or for all chuncks in this Rmarkdown:
knitr::opts_knit$set(root.dir = folder )


```

```{r}
getwd()
```
```{r}
library(igraph)
```



# Grey's Anatomy Network of Romance
## Read the data- nodes and edges into dataframe
## set graph
```{r}
ga.data <- read.csv('ga_edgelist.csv', header=TRUE, stringsAsFactors=FALSE)
ga.vrtx <- read.csv('ga_actors.csv', header=TRUE, stringsAsFactors=FALSE)
g <- graph.data.frame(ga.data, vertices=ga.vrtx, directed=FALSE)

```
##plot graph
```{r}
plot(g)
```
#1 a.i Calculate Betweenness
```{r}
calc.betweenness = betweenness(g)
calc.betweenness
```
     addison        adele       altman      arizona        avery       bailey          ben        chief        colin        denny        derek   ellis grey         finn 
    44.08333      0.00000     76.00000      0.00000      0.00000      1.00000      0.00000      3.00000      0.00000      0.00000     17.95000      4.00000      0.00000 
        grey         hank        izzie        karev       kepner         lexi mrs. seabury        nancy       olivia     o'malley         owen      preston        sloan 
    46.86667      0.00000     47.95000     95.26667      0.00000     36.00000      0.00000      0.00000      4.95000     54.41667     60.00000      0.00000    115.36667 
       steve   susan grey  thatch grey       torres       tucker         yang 
     0.00000      0.00000      3.00000     67.15000      0.00000     43.00000 
##Plot the maximal betweeness
```{r}
max.betweenness <- as.numeric(which(max(calc.betweenness) == calc.betweenness))
calc.betweenness[max.betweenness]
```
   sloan 
115.3667
#1 a.ii Calculate Closeness
```{r}
calc.closeness = closeness(g)
calc.closeness
```
     addison        adele       altman      arizona        avery       bailey          ben        chief        colin        denny        derek   ellis grey         finn 
 0.003174603  0.001144165  0.003039514  0.002985075  0.002915452  0.001075269  0.001074114  0.001148106  0.002597403  0.002881844  0.003039514  0.001149425  0.002816901 
        grey         hank        izzie        karev       kepner         lexi mrs. seabury        nancy       olivia     o'malley         owen      preston        sloan 
 0.003003003  0.002881844  0.003076923  0.003174603  0.002967359  0.003115265  0.002967359  0.002967359  0.003039514  0.003134796  0.002898551  0.002597403  0.003174603 
       steve   susan grey  thatch grey       torres       tucker         yang 
 0.002816901  0.001144165  0.001148106  0.003194888  0.001074114  0.002754821 
##Plot the maximal Closeness
```{r}
max.closeness <- as.numeric(which(max(calc.closeness) == calc.closeness))
calc.closeness[max.closeness]
```
     torres 
0.003194888 
#1 a.iii Calculate Eigenvector
```{r}
calc.eigenvector = eigen_centrality(g)
calc.eigenvector
```
$vector
     addison        adele       altman      arizona        avery       bailey          ben        chief        colin        denny        derek   ellis grey         finn 
5.537364e-01 1.980913e-17 2.077024e-01 2.101205e-01 1.538358e-01 0.000000e+00 1.267322e-17 4.029890e-18 7.009961e-03 1.654896e-01 2.500302e-01 0.000000e+00 8.795329e-02 
        grey         hank        izzie        karev       kepner         lexi mrs. seabury        nancy       olivia     o'malley         owen      preston        sloan 
3.004927e-01 1.654896e-01 5.653959e-01 1.000000e+00 2.926969e-01 5.255806e-01 2.926969e-01 1.878564e-01 4.685192e-01 6.006975e-01 6.780381e-02 7.009961e-03 6.418121e-01 
       steve   susan grey  thatch grey       torres       tucker         yang 
8.795329e-02 2.781837e-17 3.039265e-18 7.178773e-01 0.000000e+00 2.394956e-02 

$value
[1] 3.416503

$options
$options$bmat
[1] "I"

$options$n
[1] 32

$options$which
[1] "LA"

$options$nev
[1] 1

$options$tol
[1] 0

$options$ncv
[1] 0

$options$ldv
[1] 0

$options$ishift
[1] 1

$options$maxiter
[1] 1000

$options$nb
[1] 1

$options$mode
[1] 1

$options$start
[1] 1

$options$sigma
[1] 0

$options$sigmai
[1] 0

$options$info
[1] 0

$options$iter
[1] 3

$options$nconv
[1] 1

$options$numop
[1] 32

$options$numopb
[1] 0

$options$numreo
[1] 21
##Plot the maximal Eigenvector
```{r}

max.eigenvector <- as.numeric(which(max(calc.eigenvector$vector) == calc.eigenvector$vector))
calc.eigenvector$vector[max.eigenvector]
```
karev 
    1 
#1.b  Algorithms
##1.b.i
##First algorithm: The Grivan Newman alg
###Print the network up to the color code that match the communities
```{r}
alg.gri.new <- edge.betweenness.community(g)
plot(g, vertex.size=10, vertex.color=membership(alg.gri.new), asp=FALSE)
```
### There is seven different kinds of colors in the graph each one is a community. Five connected groups and two single groups
## Lets take a look over the community sizes
##1.b.ii
```{r}
sizes(alg.gri.new)
```
##1.b.iii - The modularity value
```{r}
modularity(alg.gri.new)
```

##1.b.i
##First algorithm: The Walktrap alg
###Print the network up to the color code that match the communities
```{r}
alg.walktrap <- walktrap.community(g)
plot(g, vertex.size=10, vertex.color=membership(alg.walktrap), asp=FALSE)
```
### There is seven different kinds of colors in the graph each one is a community. Four connected groups, one with double groups and one single group
## Lets take a look over the community sizes
##1.b.ii
```{r}
sizes(alg.walktrap)
```
##1.b.iii - The modularity value
```{r}
modularity(alg.walktrap)
```
