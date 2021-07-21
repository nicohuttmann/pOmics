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

  #suffix <- substring(text = colnames(x), str_locate_last(colnames(x), pattern = pattern) + 1)

  prefix <- substring(text = colnames(x), 1, str_locate_last(colnames(x), pattern = pattern) - 1)

  tab <- sort(table(prefix), decreasing = TRUE)

  tab <- tab[nchar(names(tab)) >= 3]

  # Important: Exclusion list for potential sample names
  tab <- tab[!names(tab) %in% c("Count", "IDs", "acid", "window", "position", "names", "[%]")]

  return(colnames(x)[grepl(names(tab)[1], colnames(x))] %>%
           substring(first = nchar(names(tab)[1]) + 2))

}
