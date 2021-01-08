#' Transforms list entries to vector of one element each; specific for rawdata2list
#'
#' @param x data.list
#'
#' @return
#' @export
#'
#'
list_entries2vector <- function(x) {

  # Make copy of list x
  x.mod <- x

  # Loop through all entries of x
  for (i in seq_along(x)) {

    # Check if entry is list
    if(is.list(x.mod[[i]])) {
      # Replace vector by its first element
      x.mod[[i]] <- unlist(lapply(x.mod[[i]], FUN = function(x1) x1[1]))
      # Add complete vector to end of list
      x.mod[[paste0(names(x)[i], "_all")]] <- x[[i]]

    }

  }

  # Return modified list
  return(x.mod)

}
