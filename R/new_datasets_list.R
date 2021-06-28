#' Creates datasets list
#'
#' @param replace Should datasets list be replaced if already existing
#'
#' @return
#' @export
#'
#'
new_datasets_list <- function(replace = F) {

  #
  if (!".datasets" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .datasets <<- tibble::lst()

    # Indicate if new datasets list was created
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
}
