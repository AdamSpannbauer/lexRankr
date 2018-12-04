#' Parse text into sentences and tokens

#' @description Parse a character vector of documents into into both sentences and a clean vector of tokens.  The resulting output includes IDs for document and sentence for use in other \code{lexRank} functions.
#' @param text A character vector of documents to be parsed into sentences and tokenized.
#' @param docId A character vector of document Ids the same length as \code{text}.  If \code{docId=="create"} document Ids will be created.
#' @param removePunc \code{TRUE} or \code{FALSE} indicating whether or not to remove punctuation from \code{text} while tokenizing.  If \code{TRUE}, punctuation will be removed.  Defaults to \code{TRUE}.
#' @param removeNum \code{TRUE} or \code{FALSE} indicating whether or not to remove numbers from \code{text} while tokenizing.  If \code{TRUE}, numbers will be removed.  Defaults to \code{TRUE}.
#' @param toLower \code{TRUE} or \code{FALSE} indicating whether or not to coerce all of \code{text} to lowercase while tokenizing.  If \code{TRUE}, \code{text} will be coerced to lowercase.  Defaults to \code{TRUE}.
#' @param stemWords \code{TRUE} or \code{FALSE} indicating whether or not to stem resulting tokens.  If \code{TRUE}, the outputted tokens will be tokenized using \code{SnowballC::wordStem()}.  Defaults to \code{TRUE}.
#' @param rmStopWords \code{TRUE}, \code{FALSE}, or character vector of stopwords to remove from tokens. If \code{TRUE}, words in \code{lexRankr::smart_stopwords} will be removed prior to stemming. If \code{FALSE}, no stopword removal will occur. If a character vector is passed, this vector will be used as the list of stopwords to be removed.  Defaults to \code{TRUE}.
#' @return A list of dataframes.  The first element of the list returned is the \code{sentences} dataframe; this dataframe has columns \code{docId}, \code{sentenceId}, & \code{sentence} (the actual text of the sentence).  The second element of the list returned is the \code{tokens} dataframe; this dataframe has columns \code{docId}, \code{sentenceId}, & \code{token} (the actual text of the token).
#' @examples
#' sentenceTokenParse(c("Bill is trying to earn a Ph.D.", "You have to have a 5.0 GPA."),
#'                    docId=c("d1","d2"))

#' @export
sentenceTokenParse <- function(text, docId = "create", removePunc=TRUE, removeNum=TRUE, toLower=TRUE, stemWords=TRUE, rmStopWords=TRUE){
  sentenceDf <- sentenceParse(text, docId=docId)

  tokenDfList <- lapply(seq_along(sentenceDf$sentence), function(i) {
    sentVec <- sentenceDf$sentence[i]
    tokenList <- tokenize(text = sentVec, removePunc = removePunc, removeNum = removeNum, toLower = toLower, stemWords = stemWords, rmStopWords=rmStopWords)
    subTokenDfList <- lapply(seq_along(tokenList), function(j) {
      data.frame(docId=sentenceDf$docId[i], sentenceId=sentenceDf$sentenceId[i], token=tokenList[[j]], stringsAsFactors = FALSE)
    })
    do.call('rbind', subTokenDfList)
  })
  tokenDf <- do.call('rbind', tokenDfList)
  tokenDf <- tokenDf[!is.na(tokenDf$token),]
  class(tokenDf) <- "data.frame"

  list(sentences=sentenceDf, tokens=tokenDf)
}
