# lexRankr: Extractive Text Summariztion in R

[![Build Status](https://travis-ci.org/AdamSpannbauer/lexRankr.svg?branch=master)](https://travis-ci.org/AdamSpannbauer/lexRankr) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/AdamSpannbauer/lexRankr?branch=master&svg=true)](https://ci.appveyor.com/project/AdamSpannbauer/lexRankr)  [![Coverage Status](https://img.shields.io/codecov/c/github/AdamSpannbauer/lexRankr/master.svg)](https://codecov.io/github/AdamSpannbauer/lexRankr?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/lexRankr)](https://CRAN.R-project.org/package=lexRankr) ![](http://cranlogs.r-pkg.org/badges/grand-total/lexRankr) [![Last Commit](https://img.shields.io/github/last-commit/AdamSpannbauer/lexRankr.svg)](https://github.com/AdamSpannbauer/lexRankr/commits/master)

## Installation 

```r
##install from CRAN
install.packages("lexRankr")

#install from this github repo
devtools::install_github("AdamSpannbauer/lexRankr")
```

## Overview
lexRankr is an R implementation of the LexRank algorithm discussed by Güneş Erkan & Dragomir R. Radev in [LexRank: Graph-based Lexical Centrality as Salience in Text Summarization](http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume22/erkan04a-html/erkan04a.html).  LexRank is designed to summarize a cluster of documents by proposing which sentences subsume the most information in that particular set of documents.  The algorithm may not perform well on a set of unclustered/unrelated set of documents.  As the white paper's title suggests, the sentences are ranked based on their centrality in a graph.  The graph is built upon the pairwise similarities of the sentences (where similarity is measured with a modified idf cosine similarity function).  The paper describes multiple ways to calculate centrality and these options are available in the R package.  The sentences can be ranked according to their degree of centrality or by using the Page Rank algorithm (both of these methods require setting a minimum similarity threshold for a sentence pair to be included in the graph).  A third variation is Continuous LexRank which does not require a minimum similarity threshold, but rather uses a weighted graph of sentences as the input to Page Rank.

*note: the lexrank algorithm is designed to work on a cluster of documents. LexRank is built on the idea that a cluster of docs will focus on similar topics*

*note: pairwise sentence similarity is calculated for the entire set of documents passed to the function.  This can be a computationally instensive process (esp with a large set of documents)*

## Basic Usage
```r
library(lexRankr)
library(dplyr)

df <- tibble(doc_id = 1:3, 
             text = c("Testing the system. Second sentence for you.", 
                      "System testing the tidy documents df.", 
                      "Documents will be parsed and lexranked."))
                      
df %>% 
    unnest_sentences(sents, text) %>% 
    bind_lexrank(sents, doc_id, level = 'sentences') %>% 
    arrange(desc(lexrank))
```

## More Examples

* [Vignette](https://CRAN.R-project.org/package=lexRankr/vignettes/Analyzing_Twitter_with_LexRankr.html)
* [Summarizing Web Articles with R using lexRankr](https://adamspannbauer.github.io/2017/12/17/summarizing-web-articles-with-r/)
* [lexRankr & Twitter: find a user's most representative tweets](https://adamspannbauer.github.io/2017/03/09/lexrankr--twitter-find-a-users-most-representative-tweets/)

