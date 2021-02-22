#' Creates new cache list
#'
#' @param replace Should cache list be replaced if already existing
#'
#' @return whether a new cache list was created
#' @export
#'
#'
new_cache <- function(replace = T) {

  #
  if (!".cache" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .cache <<- tibble::lst()


    # Indicate if new cache list was created
    invisible(TRUE)
  } else {
    iinvisible(FALSE)
  }

}
