#' Creates imports list
#'
#' @param replace Should imports list be replaced if already existing
#'
#' @return
#' @export
#'
#'
new_imports_list <- function(replace = F) {

  #
  if (!".imports" %in% objects(all.names = T, envir = .GlobalEnv) || replace) {
    .imports <<- tibble::lst()

    # Indicate if new info list was created
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }

}
