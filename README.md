# Data Collection & Network Analysis
---
title: "Data Collection & Network Analysis"
author: "Matan Yeshurun and Alon Galperin"
date: "December 15, 2017"
output: html_document
---

# Assignment 3 - Data Collection & Network Analysis

Setting a working directory:

```{r setup}
folder = 'C:/ex3'
setwd(folder)

#Or for all chuncks in this Rmarkdown:
knitr::opts_knit$set(root.dir = folder )
```
# Question 1 Grey's Anatomy Network of Romance

Import IGraph Library
```{r}
library(igraph)
```

## Read the data - nodes and edges into dataframe
```{r}
ga.data <- read.csv('ga_edgelist.csv', header=TRUE, stringsAsFactors=FALSE)
ga.vrtx <- read.csv('ga_actors.csv', header=TRUE, stringsAsFactors=FALSE)
```

### Set graph
```{r}
g <- graph.data.frame(ga.data, vertices=ga.vrtx, directed=FALSE)
```
### plot graph
```{r}
plot(g)
```
![str result Image](https://github.com/matan-yes/ex3/blob/master/images/1-graph.JPG)

## 1.a.i) Calculate Betweenness
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
     
### Calculate the maximal betweeness
```{r}
max.betweenness <- as.numeric(which(max(calc.betweenness) == calc.betweenness))
calc.betweenness[max.betweenness]
```
    ##    sloan 
    ## 115.3667

## 1.a.ii) Calculate Closeness
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
### Calculate the maximal Closeness
```{r}
max.closeness <- as.numeric(which(max(calc.closeness) == calc.closeness))
calc.closeness[max.closeness]
```
    ##      torres 
    ## 0.003194888
## 1.a.iii) Calculate Eigenvector
```{r}
calc.eigenvector = eigen_centrality(g)
calc.eigenvector
```
    ## addison        adele       altman      arizona        avery       bailey          ben        chief        colin        denny        derek   ellis grey         finn 
    ## 5.537364e-01 1.980913e-17 2.077024e-01 2.101205e-01 1.538358e-01 0.000000e+00 1.267322e-17 4.029890e-18 7.009961e-03 1.654896e-01 2.500302e-01 0.000000e+00 8.795329e-02 
    ##  grey         hank        izzie        karev       kepner         lexi mrs. seabury        nancy       olivia     o'malley         owen      preston        sloan 
    ## 3.004927e-01 1.654896e-01 5.653959e-01 1.000000e+00 2.926969e-01 5.255806e-01 2.926969e-01 1.878564e-01 4.685192e-01 6.006975e-01 6.780381e-02 7.009961e-03 6.418121e-01 
    ##   steve   susan grey  thatch grey       torres       tucker         yang 
    ## 8.795329e-02 2.781837e-17 3.039265e-18 7.178773e-01 0.000000e+00 2.394956e-02 
    
### Calculate the maximal Eigenvector
```{r}
max.eigenvector <- as.numeric(which(max(calc.eigenvector$vector) == calc.eigenvector$vector))
calc.eigenvector$vector[max.eigenvector]
```
    ## karev 
    ##     1
    
## 1.b  Algorithms
## First algorithm: The Grivan Newman Algorithm
### 1.b.i) Print the network up to the color code that match the communities
```{r}
alg.gri.new <- edge.betweenness.community(g)
plot(g, vertex.size=10, vertex.color=membership(alg.gri.new), asp=FALSE)
```
![str result Image](https://github.com/matan-yes/ex3/blob/master/images/2-graph.JPG)

### 1.b.ii) There are seven different kinds of colors in the graph each one is a community. Five connected groups and two single groups

### Lets take a look over the community sizes
```{r}
sizes(alg.gri.new)
```

    ## Community sizes
    ## 1 2 3 4 5 6 7 
    ## 8 5 5 4 3 3 4 

### 1.b.iii - The modularity value
```{r}
modularity(alg.gri.new)
```
    [1] 0.5774221

## Second algorithm: The Walktrap Algorithms
### 1.b.i) Print the network up to the color code that match the communities
```{r}
alg.walktrap <- walktrap.community(g)
plot(g, vertex.size=10, vertex.color=membership(alg.walktrap), asp=FALSE)
```
![str result Image](https://github.com/matan-yes/ex3/blob/master/images/3-graph.JPG)

### 1.b.ii) There are seven different kinds of colors in the graph each one is a community. Four connected groups, one with double groups and one single group

## Lets take a look over the community sizes
 
```{r}
sizes(alg.walktrap)
```

    ##  Community sizes
    ##  1  2  3  4  5  6  7 
    ##  5 13  3  3  2  3  3
    
### 1.b.iii - The modularity value
```{r}
modularity(alg.walktrap)
```
[1] 0.5147059


# Question 2 Use API to Fetch Data and Network Analysis

## 2.a) Data Collection and Cleaning
In this question we will fectch data from Facebook.
We will use the package RFacebook.  

**The steps of the process are:**  
1. We will create Facebook Developer account.  
2. In R we will download and require all the needed packages.  
3. Create auth file (following the tutorial mentioned above).  
4. We will fetch posts from a chosen facebook page
5. clean the posts texts  
6. Build a corpus and Term Document Matrix

The data we collect is posts from [Tasty](https://www.facebook.com/buzzfeedtasty/?fref=ts) Facebook page  
We will fetch 25 posts, without comments. It would be nice if we can discover what are the main ingredients in thier recipes.

#### Let's start:  
Import RFacebook  

Quotation from RFacebook github:  
This package provides a series of functions that allow R users to access Facebook's API to get information about public pages, groups, and posts, as well as some of the authenticated user's private data.


```{r}
require (Rfacebook)
```

Load the auth file with the Facebook authentication details.  
We used the following tutorial to learn how to create the authentication:  
[Analyzing-Facebook-with-R](http://thinktostart.com/analyzing-facebook-with-r/)
```{r}
load("fb_oauth")
```

Fetch 25 posts from the page Tasty.
We won't fetch comments of the posts, only the content of the posts.

```{r}
post_amount = 25
facebook_page = "buzzfeedtasty"
q2.fb_page <- getPage(page = facebook_page, token=fb_oauth, n = post_amount)
```

### Data Cleaning

__Stages of cleaning:__
1. Convert text to UTF-8  
2. Change characters to lowercase  
3. Remove URLS  
4. Delete all chars that are not english letter or numbers  
5. Fix spacing  

```{r}
# helper function for removing urls from posts content
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)

Clean_String <- function(string){
    # convert to UTF-8
    processed_text <- iconv(string, "", "UTF-8")
    # Lowercase
    processed_text <- tolower(processed_text)
    # remove urls
    processed_text <- removeURL(processed_text)
    #' Remove everything that is not a number or letter
    processed_text <- stringr::str_replace_all(processed_text,"[^a-zA-Z'0-9\\s]", "")
    # Shrink down to just one white space
    processed_text <- stringr::str_replace_all(processed_text,"[\\s]+", " ")
    # Get rid of trailing "" if necessary
    indexes <- which(processed_text == "")
    if(length(indexes) > 0){
      processed_text <- processed_text[-indexes]
    }
    return(processed_text)
}

clean_posts <- lapply(X=q2.fb_page$message,FUN=Clean_String)
```

Print the first 3 posts to see how the text looks after cleaning
```{r}
head(clean_posts, n = 3)
```

    [[1]] "gettin' hygge with it"

    [[2]] "from basic kitchen essentials to cutting edge devices these tools will help you become a culinary master in the new year"

    [[3]] "still shopping you can still get our tasty latest greatest cookbook by christmas order here "

### Create corpus
We use tm package, tm is a Text Mining package
```{r}
library(tm)

q2.corpus <- Corpus(VectorSource(clean_posts))
q2.corpus <- tm_map(q2.corpus, removeWords, stopwords("english"))

```

### Create Term Document Matrix
The term-document matrix will contain a binary weight, meaning '1' if term a is in document 1 or '0' otherwise.

```{r}
q2.td_matrix <- TermDocumentMatrix(q2.corpus, control = list(weighting=weightBin))
q2.td_matrix <- as.matrix(q2.td_matrix)
head(q2.td_matrix)
```

![str result Image](https://github.com/matan-yes/ex3/blob/master/images/terms_table.PNG)

This table will represent our adjacency table of the graph.  

### 2.b) Graph Definition  
__The graph will show connection between words in **same post**:__  
 Vertex = term from the post corpus  
 Edge  = represents co-occurence of the terms connected to it in the same post  
 Direction = the graph will be undirected  

### 2.c) Create the Graph:
```{r}
q2.graph <- graph.incidence(q2.td_matrix)
q2.project_bi_graph <- bipartite.projection(q2.graph)
q2.graph <- q2.project_bi_graph$proj1
q2.graph <- simplify(q2.graph)
summary(q2.graph)
```
    IGRAPH 1e9aff4 UNW- 183 1565 -- 
    + attr: name (v/c), weight (e/n)
    
**Discover Graph dimensions:**  
We have a 183 vertices and 1565 edges.  
We can see from "UNW"" that the graph is undirected (as we expected).

### Draw Graph
```{r}
q2.graph$layout <- layout.circle(q2.graph)
V(q2.graph)$label <- V(q2.graph)$name
V(q2.graph)$size = degree(q2.graph)
V(q2.graph)$label.cex<-  2.2 * V(q2.graph)$size / max(V(q2.graph)$size) + .2
plot(q2.graph, margin = -0.2)
```
![str result Image](https://github.com/matan-yes/ex3/blob/master/images/4-graph.JPG)  
We tried some types of graphs and decided that the circle is the most readable.

### 2.d) From Question 1
#### Calculate Concentration
**Betweenness**

```{r}
calc.betweenness = betweenness(q2.graph)
max.betweenness <- as.numeric(which(max(calc.betweenness) == calc.betweenness))
calc.betweenness[max.betweenness]
```
       tasty
    3742.635


**closeness**

```{r}
q2.closeness = closeness(q2.graph)
q2.max_closeness <- as.numeric(which(max(q2.closeness) == q2.closeness))
q2.closeness[q2.max_closeness]
```

           tasty
    0.0003752345


**Eigenvector**

```{r}
q2.eigenvector = eigen_centrality(q2.graph)
q2.max_eigenvector <- as.numeric(which(max(q2.eigenvector$vector) == q2.eigenvector$vector))
q2.eigenvector$vector[q2.max_eigenvector]
```

    now
      1
**conclusion**
The word "tasty"" is the word with the maximal Betweenness and closeness.  
The word "now"" is the word with the maximal Eigenvector.

### community detection:  
With the shape of circle it will be difficult to see the diffrent communities.  
We will change the shape to fruchterman.reingold

```{r}
q2.graph$layout <-layout.fruchterman.reingold(q2.graph)
```

### First algorithm - Girvan-Newman:

```{r}
q2.gn <-  edge.betweenness.community(q2.graph)
plot(q2.graph, vertex.size=10, vertex.color=membership(q2.gn), asp=FALSE)
```
![str result Image](https://github.com/matan-yes/ex3/blob/master/images/5-graph.JPG)

Check the size of the community and number of members:
first row is the number of the community, second row is the number of members in the community  
```{r}
sizes(q2.gn)
```
    Community sizes
     1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19  
     2 26 35 16 19 24  5  4 20  1  1  1  8  4  4  2  1  6  4
     
According to Girvan-Newman algorithm, there are 19 communities, the largest has 35 vertexes.

modularity value (returns max value):

```{r}
modularity(q2.gn)
```

    [1] 0.576375
    
### Second algorithm - Walktrap:

```{r}
q2.walktrap <- walktrap.community(q2.graph)
plot(q2.graph, vertex.size=10, vertex.color=membership(q2.walktrap), asp=FALSE)
```
![str result Image](https://github.com/matan-yes/ex3/blob/master/images/6-graph.JPG)

Check the size of the community and number of members:
```{r}
sizes(q2.walktrap)
```

    Community sizes
     1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
     18 41  8 14 16 33 12  6  4 18  4  2  2  4  1
According to Walktrap algorithm, there are 15 communities, the largest has 41 vertexes.

modularity value (returns max value):
```{r}
modularity(q2.walktrap)
```
    [1] 0.6615078
