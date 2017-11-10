#' Split a column of text into sentences

#' @description Split a column of text into sentences
#' @param tbl dataframe containing column of text to be split into sentences
#' @param output name of column to be created to store parsed sentences
#' @param input name of input column of text to be parsed into sentences
#' @param output_id name of column to be created to store sentence ids
#' @param drop whether original input column should get dropped
#' @return A data.frame of parsed sentences and sentence ids
#' @examples
#' library(magrittr)
#' 
#' df <- data.frame(doc_id = 1:3, 
#'                  text = c("Testing the system. Second sentence for you.", 
#'                           "System testing the tidy documents df.", 
#'                           "Documents will be parsed and lexranked."),
#'                  stringsAsFactors=FALSE)
#'
#' unnest_sentences(df, sents, text)
#' unnest_sentences_(df, "sents", "text")
#' 
#' df %>% 
#'   unnest_sentences(sents, text)

#' @export
unnest_sentences_ <- function(tbl, output, input, output_id="sent_id", drop=TRUE) {
  if(!is.data.frame(tbl)) stop("tbl must be a dataframe")
  if(!(input %in% names(tbl))) stop("input column not found in tbl")
  if(!is.character(tbl[[input]])) stop("input column must be character")
  if(length(output_id) > 1) {
    warning("only first element of output_id will be used")
    output_id <- output_id[1]
  }
  if(!is.logical(drop)) stop("drop must be logical")
  
  text <- tbl[[input]]
  parsed_sents <- sentence_parser(text)
  
  if (drop) {
    tbl[[input]] <- NULL
  }
  
  tbl_out_list <- lapply(seq_along(parsed_sents), function(i) {
    row_i = tbl[i,,drop=FALSE]
    parsed_sent_rows_i = data.frame(sent_id = seq_along(parsed_sents[[i]]),
                                    sents = parsed_sents[[i]], 
                                    stringsAsFactors = FALSE)
    names(parsed_sent_rows_i) = c(output_id, output)
    out = suppressWarnings(cbind(row_i, parsed_sent_rows_i))
    names(out)[seq_along(row_i)] = names(row_i)
    out 
  })
  out_tbl = do.call('rbind', tbl_out_list)
  rownames(out_tbl) = 1:nrow(out_tbl)
  return(out_tbl)
}

#' @rdname unnest_sentences_
#' @export
unnest_sentences <- function(tbl, output, input, output_id='sent_id', drop=TRUE) {
  output_str <- as.character(substitute(output))
  input_str  <- as.character(substitute(input))
  out_id_str <- as.character(substitute(output_id))
  
  unnest_sentences_(tbl, output_str, input_str, out_id_str, drop)
}
