#' Creates new cache list
#'
#' @param replace Should cache list be replaced if already existing
#' @param return Should indication whether new cache list was created be returned
#'
#' @return whether a new cache list was created
#' @export
#'
#'
new_cache <- function(replace = T, return = F) {

  #
  if (!".cache" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .cache <<- tibble::lst()


    # Indicate if new cache list was created
    if (return) return(TRUE)
  } else {
    if (return) return(FALSE)
  }

}
