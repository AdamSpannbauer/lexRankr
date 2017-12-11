#' Split a column of text into sentences

#' @description Split a column of text into sentences
#' @param tbl dataframe containing column of text to be split into sentences
#' @param output name of column to be created to store parsed sentences
#' @param input name of input column of text to be parsed into sentences
#' @param doc_id column of document ids; if not provided it will be assumed that each row is a different document
#' @param output_id name of column to be created to store sentence ids
#' @param drop whether original input column should get dropped
#' @return A data.frame of parsed sentences and sentence ids
#' @examples
#' library(dplyr)
#' 
#' df <- dplyr::tibble(doc_id = 1:3, 
#'                     text = c("Testing the system. Second sentence for you.", 
#'                              "System testing the tidy documents df.", 
#'                              "Documents will be parsed and lexranked."))
#'
#' unnest_sentences(df, sents, text)
#' unnest_sentences_(df, "sents", "text")
#' 
#' df %>% 
#'   unnest_sentences(sents, text)

#' @export
unnest_sentences_ <- function(tbl, output, input, doc_id=NULL, output_id="sent_id", drop=TRUE) {
  if(!is.data.frame(tbl)) stop("tbl must be a dataframe")
  if(!(input %in% names(tbl))) stop("input column not found in tbl")
  if(!is.character(tbl[[input]])) stop("input column must be character")
  if(length(output_id) > 1) {
    warning("only first element of output_id will be used")
    output_id <- output_id[1]
  }
  if(!is.logical(drop)) stop("drop must be logical")
  if(!is.null(doc_id)) {
    if(!(doc_id %in% names(tbl))) stop("doc_id column not found in tbl")
  }
  
  text <- tbl[[input]]
  parsed_sents <- sentence_parser(text)
  sent_ids     <- lapply(parsed_sents, function(.x) 1:length(.x))
  
  if (drop) {
    tbl[[input]] <- NULL
  }
  
  tbl[[output_id]] <- sent_ids
  tbl[[output]]    <- parsed_sents
  
  out = tidyr::unnest(tbl)
  if(!is.null(doc_id)) {
    out_tbl_list = split(out, out[[doc_id]])
    out_tbl_list = lapply(out_tbl_list, function(dfi) {
      dfi[[output_id]] = seq_along(dfi[[output_id]])
      dfi
    })
    out = do.call('rbind', out_tbl_list)
  }
  rownames(out) = NULL
  return(out)
}

#' @rdname unnest_sentences_
#' @export
unnest_sentences <- function(tbl, output, input, doc_id=NULL, output_id='sent_id', drop=TRUE) {
  output_str <- as.character(substitute(output))
  input_str  <- as.character(substitute(input))
  out_id_str <- as.character(substitute(output_id))
  doc_id <- as.character(substitute(doc_id))
  if (length(doc_id) == 0) doc_id = NULL
  
  unnest_sentences_(tbl=tbl, output = output_str, 
                    input = input_str, doc_id = doc_id,
                    output_id = out_id_str, drop = drop)
}
