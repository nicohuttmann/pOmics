#' Creates new info list
#'
#' @param replace Should info list be replaced if already existing
#'
#' @return whether a new info list was created
#' @export
#'
#'
new_info_list <- function(replace = F) {

  #
  if (!".info" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .info <<- tibble::lst("datasets" = c(),
                   "default_dataset" = c())
    .info[["datasets"]] <<- NA
    .info[["default_dataset"]] <<- NA

    # Defaults data
    new_default_data()


    # Indicate if new info list was created
    invisible(TRUE)
  } else {
    invisible(FALSE)
  }

}
