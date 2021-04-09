#' Finds feasible identifier columns
#'
#' @param x data frame
#' @param sep separator
#' @param identifier vector of identifier columns
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
get_identifiers <- function(x, sep, identifier) {

  # Generate separators if not given
  if (!hasArg(sep)) {
    sep <- identify_separator(x)
  }


  # Fill NAs
  x <- x %>%
    dplyr::mutate(across(.fns = function(x) {
      for (i in seq_along(x)) {
        if (is.na(x[i])) {
          x[i] <- paste0("NA_", i)
        }
      }
      x
    }))


  # No identifier specified; only test first column
  if (!hasArg(identifier)) {
    x1 <- x[, 1] %>%
      as.character %>%
      keep_first(sep = sep)

    if (anyDuplicated(x1) == 0) {
      return(x1)
    }
    # Identifier column specified

  } else if (hasArg(identifier)) {

    # Are all identifiers in colnames?
    if (all(identifier %in% colnames(x))) {

      # Build identifiers vector; join columns if multiple specified
      x1 <- x %>%
        dplyr::pull(identifier[1]) %>%
        as.character %>%
        keep_first(sep = sep)
      if (length(identifier) > 1) {
        for (i in identifier[-1])
        x1 <- paste(x1,
                    x %>%
                    dplyr::pull(i) %>%
                    as.character %>%
                    keep_first(sep = sep),
                    sep = "_")

      }
      # test identifier feasibility
      if (anyDuplicated(x1) == 0) {
        return(x1)
      }
    # Given identifiers not found
    } else {
      message("Not all identifiers found in column names.")
    }

  }


  # Define identifiers manually
  try <- 0
  while (try <= 10) {

    identifier <- c()
    # Select columns for identifiers
    identifier <- select.list(choices = colnames(x), multiple = T, graphics = T, title = "Choose column/s as identifiers: ")

    if (length(identifier) > 0) {

      # Build identifiers vector; join columns if multiple specified
      x1 <- x[, identifier[1]] %>%
        as.character %>%
        keep_first(sep = sep)
      if (length(identifier > 1)) {
        for (i in identifier[-1])
          x1 <- paste(x1, x[, i] %>%
                      as.character() %>%
                      keep_first(sep = sep),
                      sep = "_")

      }
      # test identifier feasibility
      if (anyDuplicated(x1) == 0) {
        return(x1)
      }

    }

    try <- try + 1
  }

}
