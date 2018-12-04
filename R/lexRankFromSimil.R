#' Compute LexRanks from pairwise sentence similarities

#' @description Compute LexRanks from sentence pair similarities using the page rank algorithm or degree centrality the methods used to compute lexRank are discussed in "LexRank: Graph-based Lexical Centrality as Salience in Text Summarization."
#' @param s1 A character vector of sentence IDs corresponding to the \code{s2} and \code{simil} arguments
#' @param s2 A character vector of sentence IDs corresponding to the \code{s1} and \code{simil} arguments
#' @param simil A numeric vector of similarity values that represents the similarity between the sentences represented by the IDs in \code{s1} and \code{s2}.
#' @param threshold The minimum simil value a sentence pair must have to be represented in the graph where lexRank is calculated.
#' @param n The number of sentences to return as the extractive summary.  The function will return the top \code{n} lexRanked sentences.  See \code{returnTies} for handling ties in lexRank.
#' @param returnTies \code{TRUE} or \code{FALSE} indicating whether or not to return greater than \code{n} sentence IDs if there is a tie in lexRank.  If \code{TRUE}, the returned number of sentences will not be limited to \code{n}, but rather will return every sentence with a top 3 score.  If \code{FALSE}, the returned number of sentences will be \code{<=n}. Defaults to \code{TRUE}.
#' @param usePageRank \code{TRUE} or \code{FALSE} indicating whether or not to use the page rank algorithm for ranking sentences.  If \code{FALSE}, a sentences unweighted centrality will be used as the rank.  Defaults to \code{TRUE}.
#' @param damping The damping factor to be passed to page rank algorithm.  Ignored if \code{usePageRank} is \code{FALSE}.
#' @param continuous \code{TRUE} or \code{FALSE} indicating whether or not to use continuous LexRank.  Only applies if \code{usePageRank==TRUE}.  If \code{TRUE}, \code{threshold} will be ignored and lexRank will be computed using a weighted graph representation of the sentences. Defaults to \code{FALSE}.
#' @return A 2 column dataframe with columns \code{sentenceId} and \code{value}. \code{sentenceId} contains the ids of the top \code{n} sentences in descending order by \code{value}. \code{value} contains page rank score (if \code{usePageRank==TRUE}) or degree centrality (if \code{usePageRank==FALSE}).
#' @references \url{http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume22/erkan04a-html/erkan04a.html}
#' @examples
#' lexRankFromSimil(s1=c("d1_1","d1_1","d1_2"), s2=c("d1_2","d2_1","d2_1"), simil=c(.01,.03,.5))

#' @export

lexRankFromSimil <- function(s1, s2, simil, threshold=.2, n=3, returnTies=TRUE, usePageRank=TRUE, damping=0.85, continuous=FALSE) {
  if(!is.logical(returnTies)) stop("returnTies must be logical")
  if(length(returnTies) != 1) stop("returnTies must be length 1")
  if(!is.logical(usePageRank)) stop("usePageRank must be logical")
  if(length(usePageRank) != 1) stop("usePageRank must be length 1")
  if(!is.logical(continuous)) stop("continuous must be logical")
  if(length(continuous) != 1) stop("continuous must be length 1")
  if(!is.numeric(simil)) stop("simil must be numeric")
  if(!is.numeric(n)) stop("n must be numeric")
  if(length(n) != 1) stop("n must be length 1")
  
  if (length(s1) != length(s2) | length(s1) != length(simil)) stop("s1, s2, & simil must all be the same length")
  if (sum(simil) == 0) stop("all simil values are zero")
  if (sum(simil > threshold) == 0 & !continuous) stop("all simil values are below threshold; try lowering threshold or setting continuous to TRUE if you want to retry lexRanking this input data")
  
  s1 <- as.character(s1)
  s2 <- as.character(s2)
  
  if(returnTies) tieMethod <- "min" else if(!returnTies) tieMethod <- "first"
  
  edges <- data.frame(s1=s1, s2=s2, weight=simil, stringsAsFactors = FALSE)
  
  if(!continuous | !usePageRank) {
    if(!is.numeric(threshold)) stop("threshold must be numeric")
    if(length(threshold) != 1) stop("threshold must be length 1")
    
    edges <- edges[edges$weight > threshold, c("s1","s2")]
  }

  if (usePageRank) {
    if(!is.numeric(damping)) stop("damping must be numeric")
    if(length(damping) != 1) stop("damping must be length 1")

    sentGraph <- igraph::graph_from_data_frame(edges, directed = FALSE)
    sentRank <- igraph::page_rank(sentGraph, directed=FALSE, damping=damping)$vector
    sentRanksRanked <- rank(1/sentRank, ties.method = tieMethod)
    topCentral <- sentRank[which(sentRanksRanked <= n)]
    centralDf <- data.frame(sentenceId=names(topCentral), value=topCentral,stringsAsFactors = FALSE)
    rownames(centralDf) <- NULL
  } else if(!usePageRank){
    centralDf = data.frame(sentenceId = c(edges$s1, edges$s2), stringsAsFactors = FALSE)
    centralDfList = split(centralDf, centralDf$sentenceId)
    centralDfList = lapply(centralDfList, function(dfi) {
      dfi[['degree']] = nrow(dfi)
      unique(dfi)
    })
    centralDf = do.call('rbind', centralDfList)
    centralDf = centralDf[order(-centralDf$degree),]
    centralDf[['degRank']] = rank(1/centralDf$degree, ties.method = tieMethod)
    centralDf = centralDf[centralDf$degRank <= n, c("sentenceId", "degree")]
    names(centralDf) = c("sentenceId", "value")
    
    class(centralDf) <- "data.frame"
    rownames(centralDf) <- NULL
  }
  return(centralDf)
}
