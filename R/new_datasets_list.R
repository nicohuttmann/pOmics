#' Creates datasets list
#'
#' @param replace Should datasets list be replaced if already existing
#' @param return Should indication whether new datasets info was created be returned
#'
#' @return
#' @export
#'
#'
new_datasets_list <- function(replace = F, return = T) {

  #
  if (!".datasets" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .datasets <<- tibble::lst()

    # Indicate if new datasets list was created
    if (return) return(TRUE)
  } else {
    if (return) return(FALSE)
  }
}
