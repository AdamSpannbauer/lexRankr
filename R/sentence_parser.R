#' Utility to parse sentences from text

#' @description Utility to parse sentences from text; created to have a central shared sentence parsing function
#' @param text Character vector to be parsed into sentences
#' @return A list with length equal to `length(text)`; list elements are character vectors of text parsed with sentence regex

sentence_parser <- function(text) {
  stringr::str_split(string = text, 
                     pattern = stringr::regex("(?<!\\w\\.\\w.)(?<![A-Z][a-z]\\.)(?<=\\.|\\?|\\!)\\s"))
}
