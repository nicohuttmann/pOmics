#' Prints options similar to menu() function
#'
#' @param options strings
#'
#' @return
#' @export
#'
#'
print_options <- function(options) {

  # Define maximum distance
  n <- nchar(length(options))

  # Prints all
  for(i in seq_along(options)) cat(paste0(i, ": "), options[i], "\n", sep = paste(rep(" ", n - nchar(i)), collapse = ""))

}
