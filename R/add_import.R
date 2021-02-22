#' Saves imported files to .imports list
#'
#' @param import list of imported files
#'
#' @return
#' @export
#'
#'
add_import <- function(import) {

  # Check for .imports list
  new_imports_list()

  # All imported files in list
  for (i in seq_along(import)) {

    .imports[[names(import)[i]]] <<- import[[i]]

  }

}
