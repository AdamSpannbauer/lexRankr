#' @useDynLib lexRankr
#' @importFrom Rcpp sourceCpp
NULL

#' Compute distance between sentences

#' @description Compute distance between sentences using modified idf cosine distance from "LexRank: Graph-based Lexical Centrality as Salience in Text Summarization".  Output can be used as input to \code{\link{lexRankFromSimil}}.
#' @param sentenceId A character vector of sentence IDs corresponding to the \code{docId} and \code{token} arguments
#' @param token A character vector of tokens corresponding to the \code{docId} and \code{sentenceId} arguments
#' @param docId A character vector of document IDs corresponding to the \code{sentenceId} and \code{token} arguments.  Can be \code{NULL} if \code{sentencesAsDocs} is \code{TRUE}.
#' @param sentencesAsDocs \code{TRUE} or \code{FALSE}, indicating whether or not to treat sentences as documents when calculating tfidf scores. If \code{TRUE}, inverse document frequency will be calculated as inverse sentence frequency (useful for single document extractive summarization)
#' @return A 3 column dataframe of pairwise distances between sentences. Columns: \code{sent1} (sentence id), \code{sent2} (sentence id), & \code{dist} (distance between \code{sent1} and \code{sent2}).
#' @references \url{http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume22/erkan04a-html/erkan04a.html}
#' @examples
#' sentenceSimil(docId=c("d1","d1","d2","d2"),
#'                sentenceId=c("d1_1","d1_1","d2_1","d2_1"),
#'                token=c("i", "ran", "jane", "ran"))
#' @importFrom utils combn
#' @importFrom stats xtabs

#' @export
sentenceSimil <- function(sentenceId, token, docId=NULL, sentencesAsDocs=FALSE){
  if(!is.logical(sentencesAsDocs)) stop("sentencesAsDocs must be logical")
  if(length(sentencesAsDocs) != 1) stop("sentencesAsDocs must be length 1")

  if(!sentencesAsDocs & is.null(docId)) stop("docIds must be provided if sentencesAsDocs is FALSE")
  sentenceId <- as.character(sentenceId)
  if(!is.character(token)) stop("token must be character")
  if(length(token) < 1) stop("token must be at least length 1")

  if(sentencesAsDocs) {
    docId <- sentenceId
    if(length(docId) != length(sentenceId) | length(docId) != length(token)) stop("docId, sentenceId, & token must all be the same length")
  } else if (!sentencesAsDocs) {
    docId <- as.character(docId)
    if(length(sentenceId) != length(token)) stop("sentenceId & token must be the same length")
  }

  ndoc <- length(unique(docId))
  if(ndoc > length(unique(sentenceId))) warning("There are more unique docIds than sentenceIds.  Verify you have passed the correct parameters to the function.")
  
  tokenDf <- data.frame(docId=docId, sentenceId=sentenceId, token=token, stringsAsFactors = FALSE)
  
  stmList = split(tokenDf, paste0(tokenDf$docId,tokenDf$token))
  stmList = lapply(stmList, function(dfi) {
    dfi[['tf']] = nrow(dfi)
    unique(dfi)
  })
  stm = do.call('rbind', stmList)
  stmList = split(stm, stm$token)
  stmList = lapply(stmList, function(dfi) {
    dfi[['idf']] = 1+log(ndoc/length(unique(dfi$docId)))
    dfi[['tfidf']] = dfi$tf*dfi$idf
    unique(dfi)
  })
  stm = do.call('rbind', stmList)
  rownames(stm) = NULL
  stm = stm[order(stm$docId, stm$token), c("docId", "token", "tf", "idf", "tfidf")]
  
  if(!sentencesAsDocs) {
    stm = merge(tokenDf, stm, by=c("docId","token"), all.x=FALSE, all.y=TRUE)
    stm = unique(stm[stm$tfidf > 0, c("sentenceId", "token", "tfidf")])
  } else if (sentencesAsDocs) {
    stm = unique(stm[stm$tfidf > 0, c("docId", "token", "tfidf")])
    names(stm) = c("sentenceId", "token", "tfidf")
  }
  
  stm = stm[order(stm$sentenceId, stm$token),]
  
  if(nrow(stm)==0) stop("All values in sentence term tfidf matrix are 0.  Similarities would return as NaN")
  if(length(unique((stm$sentenceId))) == 1) stop("Only one sentence had nonzero tfidf scores.  Similarities would return as NaN")

  stm = xtabs(tfidf ~ sentenceId + token, stm)
  
  sentencePairsDf = as.data.frame(t(combn(sort(rownames(stm)), 2)), stringsAsFactors=FALSE)
  sentencePairsDf[['similVal']] = idfCosineSimil(stm)
  names(sentencePairsDf) = c("sent1", "sent2", "similVal")

  return(sentencePairsDf)
}
