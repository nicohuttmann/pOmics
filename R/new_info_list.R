#' Creates new info list
#'
#' @param replace Should info list be replaced if already existing
#'
#' @return logical indication if new .info list was created
#' @export
#'
#'
new_info_list <- function(replace = F) {

  #
  if (!".info" %in% objects(all.names = TRUE, envir = .GlobalEnv) || replace) {
    .info <<- tibble::lst("default_dataset" = NA,
                          "raw_datasets" = tibble::lst())

    # Defaults data
    new_default_data()


    # Indicate if new info list was created
    return(invisible(TRUE))

  } else {
    return(invisible(FALSE))
  }

}
