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
##Plot the maximal betweeness
```{r}
max.betweenness <- as.numeric(which(max(calc.betweenness) == calc.betweenness))
calc.betweenness[max.betweenness]
```
#1 a.ii Calculate Closeness
```{r}
calc.closeness = closeness(g)
calc.closeness
```
##Plot the maximal Closeness
```{r}
max.closeness <- as.numeric(which(max(calc.closeness) == calc.closeness))
calc.closeness[max.closeness]
```
#1 a.iii Calculate Eigenvector
```{r}
calc.eigenvector = eigen_centrality(g)
calc.eigenvector
```
##Plot the maximal Eigenvector
```{r}

max.eigenvector <- as.numeric(which(max(calc.eigenvector$vector) == calc.eigenvector$vector))
calc.eigenvector$vector[max.eigenvector]
```
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
