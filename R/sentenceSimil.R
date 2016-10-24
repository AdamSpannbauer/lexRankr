#' @useDynLib lexRankr
#' @importFrom Rcpp sourceCpp
NULL

utils::globalVariables(c("n","tf","idf","tfidf","V1","V2","similVal"))
#' Compute distance between sentences

#' @description Compute distance between sentences using modified idf cosine distance from "LexRank: Graph-based Lexical Centrality as Salience in Text Summarization".  Output can be used as input to \code{\link{lexRankFromSimil}}.
#' @param sentenceId A character vector of sentence IDs corresponding to the \code{docId} and \code{token} arguemants.
#' @param token A character vector of tokens corresponding to the \code{docId} and \code{sentenceId} arguemants.
#' @param docId A character vector of document IDs corresponding to the \code{sentenceId} and \code{token} arguemants.  Can be \code{NULL} if \code{sentencesAsDocs} is \code{TRUE}.
#' @param sentencesAsDocs \code{TRUE} or \code{FALSE}, indicating whether or not to treat sentences as documents when calculating tfidf scores. If \code{TRUE}, inverse document frequency will be calculated as inverse sentence frequency (useful for single document extractive summarization)
#' @return A 3 column dataframe of pairwise distances between sentences. Columns: \code{sent1} (sentence id), \code{sent2} (sentence id), & \code{dist} (distance between \code{sent1} and \code{sent2}).
#' @references \url{http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume22/erkan04a-html/erkan04a.html}
#' @examples
#' sentenceSimil(docId=c("d1","d1","d2","d2"),
#'                sentenceId=c("d1_1","d1_1","d2_1","d2_1"),
#'                token=c("i", "ran", "jane", "ran"))
#' @importFrom utils combn
#' @importFrom magrittr "%>%"

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

  tokenDf <- dplyr::data_frame(docId=docId, sentenceId=sentenceId, token=token)
  stm <- tokenDf %>%
    dplyr::group_by(docId,token) %>%
    dplyr::summarise(tf = n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(token) %>%
    dplyr::mutate(idf = log(ndoc/n())) %>%
    dplyr::mutate(tfidf = tf*idf) %>%
    dplyr::ungroup()

  if(!sentencesAsDocs) {
    stm <- stm %>%
      dplyr::right_join(tokenDf, by=c("docId"="docId", "token"="token")) %>%
      dplyr::select(sentenceId, token, tfidf) %>%
      dplyr::filter(tfidf > 0) %>%
      unique()
  } else if (sentencesAsDocs) {
    stm <- stm %>%
      dplyr::select(sentenceId=docId, token, tfidf) %>%
      dplyr::filter(tfidf > 0) %>%
      unique()
  }

  if(nrow(stm)==0) stop("All values in sentence term tfidf matrix are 0.  Similarities would return as NaN")
  if(nrow(stm) == 1) stop("Only one sentence had nonzero tfidf scores.  Similarities would return as NaN")

  stm <- tidyr::spread(stm, key=token, value=tfidf, fill=0, drop=FALSE)

  matRowNames <- stm$sentenceId

  stm <- stm %>%
    dplyr::select(-sentenceId) %>%
    as.matrix()
  rownames(stm) <- matRowNames

  #old non C slow version for idfcosine similarity
  # idfCosine <- function(x,y) {
  #   sum(x*y)/(sqrt(sum(x^2))*sqrt(sum(y^2)))
  # }
  # prDBname <- "idfCosine"
  # while (proxy::pr_DB$entry_exists(prDBname)) {
  #   prDBname <- paste0(prDBname, sample(100:999,1))
  #   cat(prDBname,"\n")
  # }
  # proxy::pr_DB$set_entry(FUN=idfCosine, names=prDBname)
  # similMat <- proxy::dist(stm, method=prDBname)
  # proxy::pr_DB$delete_entry(prDBname)

  sentencePairsDf <- sort(rownames(stm)) %>%
    combn(2) %>%
    t() %>%
    as.data.frame(stringsAsFactors=FALSE) %>%
    dplyr::mutate(similVal = idfCosineSimil(stm)) %>%
    # dplyr::mutate(similVal = as.numeric(similMat)) %>%
    dplyr::select(sent1=V1, sent2=V2, similVal)
  class(sentencePairsDf) <- "data.frame"

  return(sentencePairsDf)
}
