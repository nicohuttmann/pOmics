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
<<<<<<< HEAD

  if (!".datasets" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .datasets <<- tibble::lst()

=======

  if (!".datasets" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .datasets <<- list()

>>>>>>> 7d97d86d9a6bddacd1932c3fceb78539f0e59175
    # Indicate if new datasets list was created
    if (return) return(TRUE)
  } else {
    if (return) return(FALSE)
  }
}
