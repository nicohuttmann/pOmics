#' Creates imports list
#'
#' @param replace Should imports list be replaced if already existing
#' @param return Should indication whether new imports list was created be returned
#'
#' @return
#' @export
#'
#'
new_imports_list <- function(replace = F, return = T) {

  if (!".imports" %in% objects(all.names = T, envir = .GlobalEnv) || replace) {
<<<<<<< HEAD
    .imports <<- tibble::lst()
=======
    .imports <<- list()
>>>>>>> 7d97d86d9a6bddacd1932c3fceb78539f0e59175

    # Indicate if new info list was created
    if (return) return(TRUE)
  } else {
    if (return) return(FALSE)
  }

}
