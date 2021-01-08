#' Creates datasets list
#'
#' @return
#' @export
#'
#'
new_datasets_list <- function() {

  if (!".datasets" %in% objects(all.names = T, envir = .GlobalEnv)) {
    .datasets <<- list()
  }
}
