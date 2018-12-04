#' Extractive text summarization with LexRank

#' @description Compute LexRanks from a vector of documents using the page rank algorithm or degree centrality the methods used to compute lexRank are discussed in "LexRank: Graph-based Lexical Centrality as Salience in Text Summarization."
#' @param text A character vector of documents to be cleaned and processed by the LexRank algorithm
#' @param docId A vector of document IDs with length equal to the length of \code{text}.  If \code{docId == "create"} then doc IDs will be created as an index from 1 to \code{n}, where \code{n} is the length of \code{text}.
#' @param threshold The minimum simil value a sentence pair must have to be represented in the graph where lexRank is calculated.
#' @param n The number of sentences to return as the extractive summary.  The function will return the top \code{n} lexRanked sentences.  See \code{returnTies} for handling ties in lexRank.
#' @param returnTies \code{TRUE} or \code{FALSE} indicating whether or not to return greater than \code{n} sentence IDs if there is a tie in lexRank.  If \code{TRUE}, the returned number of sentences will not be limited to \code{n}, but rather will return every sentence with a top 3 score.  If \code{FALSE}, the returned number of sentences will be \code{<=n}. Defaults to \code{TRUE}.
#' @param usePageRank \code{TRUE} or \code{FALSE} indicating whether or not to use the page rank algorithm for ranking sentences.  If \code{FALSE}, a sentences unweighted centrality will be used as the rank.  Defaults to \code{TRUE}.
#' @param damping The damping factor to be passed to page rank algorithm.  Ignored if \code{usePageRank} is \code{FALSE}.
#' @param continuous \code{TRUE} or \code{FALSE} indicating whether or not to use continuous LexRank.  Only applies if \code{usePageRank==TRUE}.  If \code{TRUE}, \code{threshold} will be ignored and lexRank will be computed using a weighted graph representation of the sentences. Defaults to \code{FALSE}.
#' @param sentencesAsDocs \code{TRUE} or \code{FALSE}, indicating whether or not to treat sentences as documents when calculating tfidf scores for similarity. If \code{TRUE}, inverse document frequency will be calculated as inverse sentence frequency (useful for single document extractive summarization).
#' @param removePunc \code{TRUE} or \code{FALSE} indicating whether or not to remove punctuation from text while tokenizing.  If \code{TRUE}, punctuation will be removed.  Defaults to \code{TRUE}.
#' @param removeNum \code{TRUE} or \code{FALSE} indicating whether or not to remove numbers from text while tokenizing.  If \code{TRUE}, numbers will be removed.  Defaults to \code{TRUE}.
#' @param toLower \code{TRUE} or \code{FALSE} indicating whether or not to coerce all of text to lowercase while tokenizing.  If \code{TRUE}, \code{text} will be coerced to lowercase.  Defaults to \code{TRUE}.
#' @param stemWords \code{TRUE} or \code{FALSE} indicating whether or not to stem resulting tokens.  If \code{TRUE}, the outputted tokens will be tokenized using \code{SnowballC::wordStem()}.  Defaults to \code{TRUE}.
#' @param rmStopWords \code{TRUE}, \code{FALSE}, or character vector of stopwords to remove from tokens. If \code{TRUE}, words in \code{lexRankr::smart_stopwords} will be removed prior to stemming. If \code{FALSE}, no stopword removal will occur. If a character vector is passed, this vector will be used as the list of stopwords to be removed.  Defaults to \code{TRUE}.
#' @param Verbose \code{TRUE} or \code{FALSE} indicating whether or not to \code{cat} progress messages to the console while running.  Defaults to \code{TRUE}.

#' @return A 2 column dataframe with columns \code{sentenceId} and \code{value}. \code{sentence} contains the ids of the top \code{n} sentences in descending order by \code{value}. \code{value} contains page rank score (if \code{usePageRank==TRUE}) or degree centrality (if \code{usePageRank==FALSE}).
#' @references \url{http://www.cs.cmu.edu/afs/cs/project/jair/pub/volume22/erkan04a-html/erkan04a.html}
#' @examples
#' lexRank(c("This is a test.","Tests are fun.",
#' "Do you think the exam will be hard?","Is an exam the same as a test?",
#' "How many questions are going to be on the exam?"))

#' @export
lexRank <- function(text, docId = "create", threshold=.2, n=3, returnTies=TRUE, usePageRank=TRUE, damping=0.85, continuous=FALSE, sentencesAsDocs=FALSE, removePunc=TRUE, removeNum=TRUE, toLower=TRUE, stemWords=TRUE, rmStopWords=TRUE, Verbose=TRUE){

  if(!is.logical(Verbose)) stop("Verbose must be logical")
  if(length(Verbose) != 1) stop("Verbose must be length 1")

  if(Verbose) cat("Parsing text into sentences and tokens...")
  sentTokList <- sentenceTokenParse(text=text, docId = docId, removePunc=removePunc, removeNum=removeNum, toLower=toLower, stemWords=stemWords, rmStopWords=rmStopWords)
  if(Verbose) cat("DONE\n")
  sentDf <- sentTokList$sentences
  tokenDf <- sentTokList$tokens

  if(Verbose) cat("Calculating pairwise sentence similarities...")
  similDf <- sentenceSimil(sentenceId=tokenDf$sentenceId, token=tokenDf$token, docId=tokenDf$docId, sentencesAsDocs=sentencesAsDocs)
  if(Verbose) cat("DONE\n")

  if(Verbose) cat("Applying LexRank...")
  topNSents <- lexRankFromSimil(s1=similDf$sent1, s2=similDf$sent2, simil=similDf$similVal, threshold=threshold, n=n, returnTies=returnTies, usePageRank=usePageRank, damping=damping, continuous=continuous)
  if(Verbose) cat("DONE\nFormatting Output...")
  returnDf <- merge(sentDf, topNSents, by="sentenceId")
  returnDf <- returnDf[order(-returnDf$value), c("docId", "sentenceId", "sentence", "value")]
  rownames(returnDf) = NULL
  if(Verbose) cat("DONE\n")

  return(returnDf)
}
