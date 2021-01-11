#' Creates new info list
#'
#' @param replace Should info list be replaced if already existing
#' @param return Should indication whether new info list was created be returned
#'
#' @return whether a new info list was created
#' @export
#'
#'
new_info_list <- function(replace = F, return = T) {
<<<<<<< HEAD

  if (!".info" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .info <<- tibble::lst("datasets" = c(),
                   "default_dataset" = c())
    .info[["datasets"]] <<- c()
    .info[["default_dataset"]] <<- c()

    new_default_data()

=======

  if (!".info" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .info <<- list("datasets" = c(),
                   "default_dataset" = c())
    .info[["datasets"]] <<- c()
    .info[["default_dataset"]] <<- c()

>>>>>>> 7d97d86d9a6bddacd1932c3fceb78539f0e59175
    # Indicate if new info list was created
    if (return) return(TRUE)
  } else {
    if (return) return(FALSE)
  }

}
