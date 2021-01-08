#' Finds vector entry based on given patterns and lets User choose if not possible
#'
#' @param names vector of strings
#' @param patterns vector containing patterns
#' @param title Title of user interface
#'
#' @return
#' @export
#'
#'
findORchoose <- function(names, patterns, title = NULL) {
  
  # Collects entry indices
  y <- c()
  
  # Identifiy patterns/s
  for (pattern in patterns) {
    # Adds indices of matched entries
    y <- c(y, which(grepl(pattern, names, ignore.case = T)))
  }
  
  # Entries that match most patters
  y <- as.numeric(names(which(suppressWarnings(max(table(y)) == table(y)))))
  
  # Decides if entry was identified or must be chosen
  x <- ifelse(length(y) == 1,
              y,
              0)
  
  # Option NA if desired entry does not exist
  names <- c(names, NA)
  
  # Let's user choose the entry to use
  while (x == 0) {
    x <- menu(c(names), title = title)
  }
  
  # Returns chosen or identified entry
  return(names[x])
  
}
