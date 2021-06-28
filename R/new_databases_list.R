#' Creates databases list
#'
#' @param replace Should databases list be replaced if already existing
#'
#' @return
#' @export
#'
#'
new_databases_list <- function(replace = F) {
  
  #
  if (!".databases" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .databases <<- tibble::lst()
    
    # Indicate if new databases list was created
    return(invisible(TRUE))
  } else {
    return(invisible(FALSE))
  }
}
