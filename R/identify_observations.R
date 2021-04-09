#' Guesses observations names from raw_dataset
#'
#' @param x data frame
#' @param pattern separation pattern of column names
#'
#' @return
#' @export
#'
#'
identify_observations <- function(x, pattern = ".") {

  suffix <- substring(text = colnames(x), str_locate_last(colnames(x), pattern = pattern) + 1)

  tab <- sort(table(suffix))

  tab <- tab[nchar(names(tab)) > 3]

  tab <- tab[!names(tab) %in% c("Count", "IDs", "acid", "window", "position", "names")]

  names(tab)[tab == max(tab)]

}
