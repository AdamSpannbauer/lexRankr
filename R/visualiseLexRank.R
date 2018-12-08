## Objective is to visualise LexRank plot
## I find it helpful to inspect the plot
## And colour the nodes that represent the sentences that were selected


## inputs
### similDf <- the output df of the sentenceSimil function
### threshold <- user input on the threshold. edges are formed when weight is above the threshold


## output 
### plot of the sentences as nodes and edges when similarity is above the set threshold

library(tidyr)
library(dplyr)
library(igraph)
library(lexRankr)


text = c("Testing the system. Second sentence for you.",
         "System testing the tidy documents df.",
         "Documents will be parsed and lexranked.",
         "Plot the lexrank graph. See the lexrank graph connections.",
         "Complete test of lexrank graph.")
sentTokList <- sentenceTokenParse(text)
sentDf <- sentTokList$sentences
tokenDf <- sentTokList$tokens
similDf <- sentenceSimil(sentenceId = tokenDf$sentenceId, 
                         token = tokenDf$token, 
                         docId = tokenDf$docId)
topNSents <- lexRankFromSimil(s1 = similDf$sent1, 
                              s2 = similDf$sent2, 
                              simil = similDf$similVal,
                              n=1)


## if given the selected sentences in the format as per the output of lexRankFromSimil
## function to visualise the nodes of the sentences
visualiseLexRank <- function(similDf, 
                             threshold = 0.2,
                             selectedSents = NULL){
    edges <- data.frame(s1 = similDf$sent1, 
                        s2 = similDf$sent2, 
                        weight = similDf$similVal, 
                        stringsAsFactors = FALSE)
    edges <- edges[edges$weight > threshold, c("s1", "s2")]
    sentGraph <- igraph::graph_from_data_frame(edges, directed = FALSE)

    ## if selectedSents is provided
    ## the selected sentences will be coded blue
    ## and the other nodes yellow
    if(!is.null(selectedSents)){
        V(sentGraph)$select <-  as.character(selectedSents$value[match(V(sentGraph)$name,selectedSents$sentenceId)]) 
        V(sentGraph)$color <- "yellow" # default node color is yellow
        V(sentGraph)$color[!is.na(V(sentGraph)$select)] <- "blue" # selected sentences in blue
    }
    plot.igraph(sentGraph)
}


## test the functions
visualiseLexRank(similDf)
visualiseLexRank(similDf, selectedSents = topNSents)