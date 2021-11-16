#' Same as strsplit but option to return as vector and to convert to numeric
#'
#' @param x string
#' @param split split character
#' @param output.type output type (default = "vector"; other option is "list")
#' @param as.numeric try to convert to numeric
#'
#' @return
#' @export
#'
#'
strsplit_ <- function(x, split, output.type = "vector", as_numeric = T) {

  # Check if input is given
  if (!hasArg(x)) return(NULL)

  # Identifies separator if not given
  if (!hasArg(split)) split <- identify_separator(x)

  # Check if input is list
  if (is.list(x) || is.atomic(x)) {

    x <- unlist(x)

    # Check if list contains only character
    if (all(unlist(lapply(x, is.character)))) {
      x.split <- strsplit(x = x, split = split)
    } else if (any(unlist(lapply(x, is.character)))) {
      x.split <- strsplit(x = lapply(x, as.character), split = split)
    } else {
      x.split <- as.list(x)
    }

  # No list or vector input
  } else {
    message("Unknown input type for this function.")
    return(invisible(x))
  }


  # Coerce to numeric if possible
  if (as_numeric &&
      !any(unlist(suppressWarnings(
        lapply(x, function(x) is.na(as.numeric(x))))))) {

    x.split <- lapply(x, as.numeric)

  }

  # Return list or vector
  if (output.type == "list") {
    return(x.split)
  } else if (output.type == "vector") {
    return(unlist(x.split))
  } else {
    message("Output type not known. Returning as list.")
    return(x.split)
  }

}
