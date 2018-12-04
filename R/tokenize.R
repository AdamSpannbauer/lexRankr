utils::globalVariables(c("smart_stopwords"))
#' Tokenize a character vector

#' Parse the elements of a character vector into a list of cleaned tokens.
#' @param text The character vector to be tokenized
#' @param removePunc \code{TRUE} or \code{FALSE} indicating whether or not to remove punctuation from \code{text}.  If \code{TRUE}, punctuation will be removed.  Defaults to \code{TRUE}.
#' @param removeNum \code{TRUE} or \code{FALSE} indicating whether or not to remove numbers from \code{text}.  If \code{TRUE}, numbers will be removed.  Defaults to \code{TRUE}.
#' @param toLower \code{TRUE} or \code{FALSE} indicating whether or not to coerce all of \code{text} to lowercase.  If \code{TRUE}, \code{text} will be coerced to lowercase.  Defaults to \code{TRUE}.
#' @param stemWords \code{TRUE} or \code{FALSE} indicating whether or not to stem resulting tokens.  If \code{TRUE}, the outputted tokens will be tokenized using \code{SnowballC::wordStem()}.  Defaults to \code{TRUE}.
#' @param rmStopWords \code{TRUE}, \code{FALSE}, or character vector of stopwords to remove. If \code{TRUE}, words in \code{lexRankr::smart_stopwords} will be removed prior to stemming. If \code{FALSE}, no stopword removal will occur. If a character vector is passed, this vector will be used as the list of stopwords to be removed.  Defaults to \code{TRUE}.
#' @examples
#' tokenize("Mr. Feeny said the test would be on Sat. At least I'm 99.9% sure that's what he said.")
#' tokenize("Bill is trying to earn a Ph.D. in his field.", rmStopWords=FALSE)

#' @export
tokenize <- function(text, removePunc=TRUE, removeNum=TRUE, toLower=TRUE, stemWords=TRUE, rmStopWords=TRUE){
  if(!is.character(text)) stop("text must be character")
  if(length(text) < 1) stop("text must be at least length 1")
  if(!is.logical(removePunc)) stop("removePunc must be logical")
  if(length(removePunc) != 1) stop("removePunc must be length 1")
  if(!is.logical(removeNum)) stop("removeNum must be logical")
  if(length(removeNum) != 1) stop("removeNum must be length 1")
  if(!is.logical(toLower)) stop("toLower must be logical")
  if(length(toLower) != 1) stop("toLower must be length 1")
  if(!is.logical(stemWords)) stop("stemWords must be logical")
  if(length(stemWords) != 1) stop("stemWords must be length 1")
  if(!is.logical(rmStopWords) & !is.character(rmStopWords)) stop("rmStopWords must be a logical or a character vector")
  if(is.character(rmStopWords)) {
    rmStopWordFlag <- TRUE
    stopwords <- rmStopWords
  } else if(is.logical(rmStopWords)) {
    if(length(rmStopWords) != 1) stop("rmStopWords must be length 1 if passed as a logical")
    if(rmStopWords) {
      rmStopWordFlag <- TRUE
      stopwords <- smart_stopwords
    } else {
      rmStopWordFlag <- FALSE
    }
  }

  if (removePunc) text <- gsub(x=text,pattern="[^[:alnum:] ]",replacement="")
  if (removeNum) text <- gsub(x=text,pattern="([[:digit:]])",replacement="")
  if (toLower) text <- tolower(text)

  text <- gsub(x=text, pattern="([^[:alnum:] ])",replacement=" \\1 ")
  text <- trimws(gsub(x=text, pattern="\\s+",replacement=" "))
  text <- strsplit(x=text, split=" ", fixed=TRUE)

  if(rmStopWordFlag) text <- lapply(text, function(tokens) {
    checkTokens <- tolower(tokens)
    if (!removePunc) {
      checkTokens <- gsub(x=checkTokens,pattern="[^[:alnum:] ]",replacement="")
    }
    
    nonStopTok <- tokens[which(!checkTokens %in% stopwords)]
    if(length(nonStopTok) == 0) NA_character_ else nonStopTok
  })
  if(stemWords) {
    text <- lapply(text, function(w) {
      w_na = which(is.na(w))
      out = SnowballC::wordStem(w)
      out[w_na] = NA
      out
    })
  }

  tokenList <- lapply(text, function(tokens) {
    goodTok <- tokens[which(trimws(tokens) != "")]
    if(length(goodTok) == 0) NA_character_ else goodTok
  })
  tokenList
}
