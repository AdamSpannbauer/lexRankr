#' Bind lexrank scores to a dataframe of text

#' @description Bind lexrank scores to a dataframe of sentences or to a dataframe of tokens with sentence ids
#' @param tbl dataframe containing column of sentences to be lexranked
#' @param text name of column containing sentences or tokens to be lexranked
#' @param doc_id name of column containing document ids corresponding to \code{text}
#' @param sent_id Only needed if \code{level} is "tokens". name of column containing sentence ids corresponding to \code{text}
#' @param level the parsed level of the text column to be lexranked.  i.e. is \code{text} a column of "sentences" or "tokens"?  The "tokens" level is provided to allow users to implement custom tokenization.  Note: even if the input \code{level} is "tokens" lexrank scores are assigned at the sentence level. 
#' @param threshold The minimum simililarity value a sentence pair must have to be represented in the graph where lexRank is calculated.
#' @param usePageRank \code{TRUE} or \code{FALSE} indicating whether or not to use the page rank algorithm for ranking sentences.  If \code{FALSE}, a sentences unweighted centrality will be used as the rank.  Defaults to \code{TRUE}.
#' @param damping The damping factor to be passed to page rank algorithm.  Ignored if \code{usePageRank} is \code{FALSE}.
#' @param continuous \code{TRUE} or \code{FALSE} indicating whether or not to use continuous LexRank.  Only applies if \code{usePageRank==TRUE}.  If \code{TRUE}, \code{threshold} will be ignored and lexRank will be computed using a weighted graph representation of the sentences. Defaults to \code{FALSE}.
#' @param ... tokenizing options to be passed to lexRankr::tokenize.  Ignored if \code{level} is "sentences"
#' @return A dataframe with an additional column of lexrank scores (column is given name lexrank)
#' @examples
#' 
#' df <- data.frame(doc_id = 1:3, 
#'                  text = c("Testing the system. Second sentence for you.", 
#'                           "System testing the tidy documents df.", 
#'                           "Documents will be parsed and lexranked."),
#'                  stringsAsFactors = FALSE)
#' 
#' \dontrun{
#' library(magrittr)
#' 
#' df %>% 
#'   unnest_sentences(sents, text) %>% 
#'   bind_lexrank(sents, doc_id, level = "sentences")
#' 
#' df %>% 
#'   unnest_sentences(sents, text) %>% 
#'   bind_lexrank_("sents", "doc_id", level = "sentences")
#' 
#' df <- data.frame(doc_id  = c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2,
#'                              2, 2, 2, 3, 3, 3, 3, 3, 3), 
#'                  sent_id = c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 
#'                              1, 1, 1, 1, 1, 1, 1, 1, 1), 
#'                  tokens = c("testing", "the", "system", "second", 
#'                             "sentence", "for", "you", "system", 
#'                             "testing", "the", "tidy", "documents", 
#'                             "df", "documents", "will", "be", "parsed", 
#'                             "and", "lexranked"),
#'                  stringsAsFactors = FALSE)
#' 
#' df %>% 
#'   bind_lexrank(tokens, doc_id, sent_id, level = 'tokens')
#' }
#' @export
bind_lexrank_ <- function(tbl, text, doc_id, sent_id=NULL, level=c("sentences", "tokens"), threshold=.2, usePageRank=TRUE, damping=0.85, continuous=FALSE, ...) {
  if(!is.data.frame(tbl)) stop("tbl must be a dataframe")
  if(!(text %in% names(tbl))) stop("text column not found in tbl")
  if(!(doc_id %in% names(tbl))) stop("doc_id column not found in tbl")
  if(!is.character(level)) stop("level must be character")
  if(length(level) > 1) {
    level = level[1]
  }
  if(!(level %in% c("sentences", "tokens"))) stop("invalid value of level; accepted values for level are 'sentences' and 'tokens'")
  if(level == "tokens") {
    if(is.null(sent_id)) stop("sent_id must be provided when level is 'tokens'")
    if(!(sent_id %in% names(tbl))) stop("sent_id column not found in tbl")
    sent_ids <- tbl[[sent_id]]
  } else {
    sent_ids <- 1:nrow(tbl)
  }
  
  tbl_class     <- class(tbl)
  doc_id_class  <- class(tbl[[doc_id]])
  
  uuid_kinda <- paste0(c("a",sample(c(letters[1:6],0:9),30,replace=TRUE)), collapse = "")
  uuid_sep   <- paste0("__", uuid_kinda,"__")
  
  doc_sent_ids <- paste0(tbl[[doc_id]], uuid_sep, sent_ids)
  
  if(level=="sentences") {
    sent_id <- uuid_kinda
    tokenDfList <- lapply(seq_along(tbl[[text]]), function(i) {
      sentVec   <- tbl[[text]][i]
      tokenList <- tokenize(text = sentVec, ...)
      subTokenDfList <- lapply(seq_along(tokenList), function(j) {
        data.frame(docId=tbl[[doc_id]][i], sentenceId=doc_sent_ids[i], token=tokenList[[j]], stringsAsFactors = FALSE)
      })
      do.call('rbind', subTokenDfList)
    })
    
    tokenDf <- do.call('rbind', tokenDfList)
    tokenDf <- tokenDf[!is.na(tokenDf$token),]
  } else {
    tokenDf <- data.frame(docId=tbl[[doc_id]], sentenceId=doc_sent_ids, token=tbl[[text]], stringsAsFactors = FALSE)
  }
  
  similDf <- sentenceSimil(tokenDf$sentenceId, tokenDf$token, tokenDf$docId)
  topSentIdsDf <- lexRankFromSimil(similDf$sent1, similDf$sent2, similDf$similVal, threshold=threshold, n=Inf, returnTies=TRUE, usePageRank=usePageRank, damping=damping, continuous=continuous)
  lex_lookup <- do.call('rbind', strsplit(topSentIdsDf$sentenceId, uuid_sep, fixed=TRUE)) 
  lex_lookup <- as.data.frame(lex_lookup)
  names(lex_lookup) <- c(doc_id, sent_id)
  
  class(lex_lookup[[doc_id]])  <- doc_id_class
  
  lex_lookup$lexrank <- topSentIdsDf$value
  
  if(level=="tokens") {
    class(lex_lookup[[sent_id]]) <- class(tbl[[sent_id]])
    tbl_out <- merge(tbl, lex_lookup, all.x=TRUE, by=c(doc_id, sent_id))
  } else {
    tbl[[uuid_kinda]] <- as.character(sent_ids)
    tbl_out <- merge(tbl, lex_lookup, all.x=TRUE, by=c(doc_id, uuid_kinda))
    tbl_out <- tbl_out[order(as.numeric(tbl_out[[uuid_kinda]])),]
    tbl_out[[uuid_kinda]] <- NULL
  }
  rownames(tbl_out) <- NULL
  class(tbl_out) <- tbl_class
  tbl_out
}

#' @rdname bind_lexrank_
#' @export
bind_lexrank <- function(tbl, text, doc_id, sent_id=NULL, level=c("sentences", "tokens"), threshold=.2, usePageRank=TRUE, damping=0.85, continuous=FALSE, ...) {
  text_str    <- as.character(substitute(text))
  doc_id_str  <- as.character(substitute(doc_id))
  sent_id_str <- substitute(sent_id)
  if (!is.null(sent_id_str)) sent_id_str <- as.character(sent_id_str)
  
  bind_lexrank_(tbl, text_str, doc_id_str, sent_id=sent_id_str, level=level, threshold=threshold, usePageRank=usePageRank, damping=damping, continuous=continuous, ...)
}
