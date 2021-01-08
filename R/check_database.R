#' Checks if database is available
#'
#' @param database annotation database
#' @param taxId taxonomy Id
#'
#' @return
#' @export
#'
#'
check_database <- function(database = "UniProt", taxId) {

  # .databases exists
  if (".databases" %in% objects(all.names = T, envir = .GlobalEnv) && hasArg(taxId)) {

    # database entry exists
    if (database %in% names(.databases)) {

      # taxId exists
      if (taxId %in% names(.databases[[database]])) {

        return(TRUE)
      }

    }

  } else {
    return(FALSE)
  }

}
