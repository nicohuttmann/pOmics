#' Locates last position of pattern in given string
#'
#' @param string vector of strings
#' @param pattern pattern to match
#'
#' @return
#' @export
#'
#'
str_locate_last <- function(string, pattern) {

  pattern <- str_rev(pattern)

  pattern <- gsub(pattern = "\\.", replacement = "\\\\.", x = pattern)


  pos <- regexpr(pattern = pattern, text = str_rev(string))

  for (i in seq_along(pos)) {

    if (pos[i] != -1) pos[i] <- nchar(string)[i] - pos[i] + 1

  }


  return(c(pos))

}
