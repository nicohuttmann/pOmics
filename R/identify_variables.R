#' Finds feasible identifier columns
#'
#' @param x data frame
#' @param identifier vector of identifier columns
#' @param modify.identifiers action to perform on identifier column ("not" for
#' nothing; "split" to separate strings by separator)
#' @param sep separator
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
identify_variables <- function(x,
                               identifier,
                               modify.identifiers = "split",
                               sep) {

  # Generate separators if not given
  if (!hasArg(sep)) {
    sep <- identify_separator(x)
  }


  # Fill NAs
  if (hasArg(identifier) && !is.null(identifier)) {

    for (i in identifier) {

      x <- x %>%
        dplyr::mutate(dplyr::across(-where(is.character), as.character))

      x[is.na(x[, i]), i] <- paste0("NA_", 1:nrow(x))[is.na(x[[i]])]

    }

  }



  # No identifier specified; only test first column
  if (!hasArg(identifier) || is.null(identifier)) {
    if (grepl(pattern = "split", x = modify.identifiers)) {
      x1 <- x[, 1] %>%
      #as.character %>%
      strsplit_keep_first(split = sep)
    } else {
      x1 <- x[, 1]
    }


    if (anyDuplicated(x1) == 0) {
      return(x1)
    }
    # Identifier column specified

  } else if (hasArg(identifier) && !is.null(identifier)) {

    # Are all identifiers in colnames?
    if (all(identifier %in% colnames(x))) {

      # Build identifiers vector; join columns if multiple specified
      x1 <- x %>%
        dplyr::pull(identifier[1]) %>%
        as.character()

      if (grepl(pattern = "split", x = modify.identifiers)) {
        x1 <- x1 %>%
          strsplit_keep_first(split = sep)
      }

      if (length(identifier) > 1) {
        for (i in identifier[-1]) {

          x2 <- x %>%
            dplyr::pull(i) %>%
            as.character()

          if (grepl(pattern = "split", x = modify.identifiers)) {
            x2 <- x2 %>%
              strsplit_keep_first(split = sep)
          }

          x1 <- paste(x1, x2, sep = "_")

        }

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
    identifier <- select.list(choices = colnames(x),
                              multiple = T,
                              graphics = T,
                              title = "Choose column/s as identifiers: ")

    if (length(identifier) > 0) {

      # Build identifiers vector; join columns if multiple specified
      x1 <- x[, identifier[1]] %>%
        as.character()

      if (grepl(pattern = "split", x = modify.identifiers)) {
        x1 <- x1 %>%
          strsplit_keep_first(split = sep)
      }

      if (length(identifier > 1)) {
        for (i in identifier[-1])

          x2 <- x[, i] %>%
                      as.character()

        if (grepl(pattern = "split", x = modify.identifiers)) {
          x2 <- x2 %>%
            strsplit_keep_first(split = sep)
        }

          x1 <- paste(x1, x2, sep = "_")

      }
      # test identifier feasibility
      if (anyDuplicated(x1) == 0) {
        return(x1)
      }

    }

    try <- try + 1
  }

}
