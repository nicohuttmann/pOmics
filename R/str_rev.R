#' Returns given strings in reverse order
#'
#' @param strings vector of strings
#'
#' @return
#' @export
#'
#'
str_rev <- function(strings) {

  return(
    sapply(X = strings, FUN = function(x)
    {
    paste(rev(strsplit_(x, "")), collapse = "")
  }, USE.NAMES = F)
  )

}
