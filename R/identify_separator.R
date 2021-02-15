#' Identifies most common character from dataset and tests for numbers or letters
#'
#' @param x dataset
#'
#' @return
#' @export
#'
#'
identify_separator <- function(x) {

  sep <- most_common_character(x)

  if (sep %in% LETTERS || sep %in% letters || sep %in% 0:9) return(NA)
  else return(sep)

}
