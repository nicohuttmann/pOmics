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


#' Performs strsplit and keeps elements specified by vector
#'
#' @param x vector
#' @param keep vector of elements to keep
#' @param split separator
#' @param as_numeric try to convert to numeric
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
strsplit_keep <- function(x, keep = 1, split, as_numeric = T) {

  # Identifies separator if not given
  if (!hasArg(split)) split <- identify_separator(x)

  # Separates strings and keeps first element of each vector
  x <- x %>%
    unlist() %>%
    strsplit_(split = split, output.type = "list", as_numeric = as_numeric) %>%
    lapply(FUN = function(x) na.omit(x[keep])) %>%
    lapply(FUN = function(x) paste(x, collapse = split)) %>%
    unlist() %>%
    unname()

  return(x)

}


#' Performs strsplit and keeps first n elements of each string
#'
#' @param x vector
#' @param n number of elements to keep from beginning
#' @param split separator
#' @param as_numeric try to convert to numeric
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
strsplit_keep_firstn <- function(x, n = 1, split, as_numeric = T) {

  # Identifies separator if not given
  if (!hasArg(split)) split <- identify_separator(x)

  # Separates strings and keeps first element of each vector
  x <- x %>%
    unlist() %>%
    strsplit_(split = split, output.type = "list", as_numeric = as_numeric) %>%
    lapply(FUN = function(x) x[1:(min(c(length(x), n)))]) %>%
    lapply(FUN = function(x) paste(x, collapse = split)) %>%
    unlist() %>%
    unname()

  return(x)

}


#' Performs strsplit and keeps first element of each string
#'
#' @param x vector
#' @param split separator
#' @param as_numeric try to convert to numeric
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
strsplit_keep_first <- function(x, split, as_numeric = T) {

  strsplit_keep_firstn(x = x, n = 1, split = split, as_numeric = as_numeric)

}
