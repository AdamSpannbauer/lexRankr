#' Parse text into sentences

#' @description Parse the elements of a character vector into a dataframe of sentences with additional identifiers.
#' @param text Character vector to be parsed into sentences
#' @param docId A vector of document IDs with length equal to the length of \code{text}.  If \code{docId == "create"} then doc IDs will be created as an index from 1 to \code{n}, where \code{n} is the length of \code{text}.
#' @return A data frame with 3 columns and \code{n} rows, where \code{n} is the number of sentences found by the routine.  Column 1: \code{docId} document id for the sentence. Column 2: \code{sentenceId} sentence id for the sentence.  Column 3: \code{sentence} the sentences found in the routine.
#' @examples
#' sentenceParse("Bill is trying to earn a Ph.D.", "You have to have a 5.0 GPA.")
#' sentenceParse(c("Bill is trying to earn a Ph.D.", "You have to have a 5.0 GPA."),
#'                docId=c("d1","d2"))

#' @export
sentenceParse <- function(text, docId = "create") {
  if(!is.character(text)) stop("text must be character")
  if(length(text) < 1) stop("text must be at least length 1")
  docId <- as.character(docId)
  if(length(docId)==1 & docId[1]=="create") {
      createDocIds <- TRUE
    } else if(length(docId)==length(text)) {
      createDocIds <- FALSE
    } else if(length(docId)!=length(text)) stop("docId vector must be same length as text vector")
  
  sentences <- sentence_parser(text)
  sentenceDfList <- lapply(seq_along(sentences), function(i) {
    sentVec <- trimws(sentences[[i]])
    if (length(sentVec) == 0) sentVec = ""
    if(createDocIds) {
      out = data.frame(docId=i, sentenceId=paste0(i,"_",seq_along(sentVec)), sentence=sentVec, stringsAsFactors = FALSE)
    } else if(!createDocIds) {
      out = data.frame(docId=docId[i], sentence=sentVec, stringsAsFactors = FALSE)
    }
    
    out
  })
  sentenceDf <- do.call('rbind', sentenceDfList)
  sentenceDfList <- split(sentenceDf, sentenceDf$docId)
  sentenceDfList <- lapply(sentenceDfList, function(dfi) {
    dfi$sentenceId <- paste0(dfi$docId, "_", 1:nrow(dfi))
    dfi[,c("docId","sentenceId","sentence")]
  })
  sentenceDf <- do.call('rbind', sentenceDfList)
  class(sentenceDf) <- "data.frame"
  rownames(sentenceDf) <- NULL
  return(sentenceDf)
}
