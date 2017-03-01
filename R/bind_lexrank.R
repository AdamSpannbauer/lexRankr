#' Split a column of text into sentences

#' @description Split a column of text into sentences
#' @param tbl dataframe containing column of sentences to be lexranked
#' @param sentence name of column containing sentences to be lexranked
#' @param doc_id name of column containing document ids corresponding to the sentences column
#' @param sent_id name of column containing sentence ids corresponding to the sentences column
#' @param threshold The minimum simil value a sentence pair must have to be represented in the graph where lexRank is calculated.
#' @param usePageRank \code{TRUE} or \code{FALSE} indicating whether or not to use the page rank algorithm for ranking sentences.  If \code{FALSE}, a sentences unweighted centrality will be used as the rank.  Defaults to \code{TRUE}.
#' @param damping The damping factor to be passed to page rank algorithm.  Ignored if \code{usePageRank} is \code{FALSE}.
#' @param continuous \code{TRUE} or \code{FALSE} indicating whether or not to use continuous LexRank.  Only applies if \code{usePageRank==TRUE}.  If \code{TRUE}, \code{threshold} will be ignored and lexRank will be computed using a weighted graph representation of the sentences. Defaults to \code{FALSE}.
#' @param ... tokenizing options to be passed to lexRankr::tokenize 
#' @return A data.frame with an additional column of lexrank scores (column is given name lexrank)
#' @examples
#' library(dplyr)
#' 
#' df <- dplyr::tibble(doc_id = 1:3, 
#'                     text = c("Testing the system. Second sentence for you.", 
#'                              "System testing the tidy documents df.", 
#'                              "Documents will be parsed and tagged."))
#' 
#' df %>% 
#'   unnest_sentences(sents, text) %>% 
#'   bind_lexrank(sents, doc_id, sent_id)
#'   
#' df %>% 
#'   unnest_sentences(sents, text) %>% 
#'   bind_lexrank_("sents", "doc_id", "sent_id")
#' @export
bind_lexrank_ <- function(tbl, sentence, doc_id, sent_id, threshold=.2, usePageRank=TRUE, damping=0.85, continuous=FALSE, ...) {
  if(!is.data.frame(tbl)) stop("tbl must be a dataframe")
  if(!(sentence %in% names(tbl))) stop("sentence not found in tbl")
  if(!(doc_id %in% names(tbl))) stop("doc_id not found in tbl")
  if(!(sent_id %in% names(tbl))) stop("sent_id not found in tbl")
  
  tbl_class     <- class(tbl)
  doc_id_class  <- class(tbl[[doc_id]])
  sent_id_class <- class(tbl[[sent_id]])
  
  doc_sent_ids <- paste0(tbl[[doc_id]], "___lex___",tbl[[sent_id]])
  
  tokenDfList <- lapply(seq_along(tbl[[sentence]]), function(i) {
    sentVec   <- tbl[[sentence]][i]
    tokenList <- tokenize(text = sentVec, ...)
    subTokenDfList <- lapply(seq_along(tokenList), function(j) {
      data.frame(docId=tbl[[doc_id]][i], sentenceId=doc_sent_ids[i], token=tokenList[[j]], stringsAsFactors = FALSE)
    })
    dplyr::bind_rows(subTokenDfList)
  })
  
  tokenDf <- dplyr::bind_rows(tokenDfList) %>%
    dplyr::filter(!is.na(token))
  
  similDf <- sentenceSimil(tokenDf$sentenceId, tokenDf$token, tokenDf$docId)
  topSentIdsDf <- lexRankFromSimil(similDf$sent1, similDf$sent2, similDf$similVal, threshold=threshold, n=Inf, returnTies=TRUE, usePageRank=usePageRank, damping=damping, continuous=continuous)
  lex_lookup <- stringr::str_split_fixed(topSentIdsDf$sentenceId, "___lex___", n=2) %>% 
    dplyr::as_data_frame() %>% 
    setNames(c(doc_id, sent_id))
  class(lex_lookup[[doc_id]])  <- doc_id_class
  class(lex_lookup[[sent_id]]) <- sent_id_class
  lex_lookup$lexrank <- topSentIdsDf$value
  
  tbl_out <- dplyr::left_join(tbl, lex_lookup, by=c(doc_id, sent_id))
  class(tbl_out) <- tbl_class
  tbl_out
}

#' @rdname bind_lexrank_
#' @export
bind_lexrank <- function(tbl, sentence, doc_id, sent_id, threshold=.2, usePageRank=TRUE, damping=0.85, continuous=FALSE, ...) {
  sentence_str <- as.character(substitute(sentence))
  doc_id_str   <- as.character(substitute(doc_id))
  sent_id_str  <- as.character(substitute(sent_id))
  
  bind_lexrank_(tbl, sentence_str, doc_id_str, sent_id_str, threshold=threshold, usePageRank=usePageRank, damping=damping, continuous=continuous, ...)
}

df %>% 
  unnest_sentences(sents, text) %>% 
  bind_lexrank_("sents", "doc_id", "sent_id") %>% 
  as_data_frame()

df %>% 
  unnest_sentences(sents, text) %>% 
  bind_lexrank(sents, doc_id, sent_id) %>% 
  as_data_frame()
