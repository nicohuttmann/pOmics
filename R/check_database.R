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
  if (hasArg(taxId) && ".databases" %in% objects(all.names = T, envir = .GlobalEnv)) {

    # database entry exists
    if (length(names(.databases)) > 0 && database %in% names(.databases)) {

      # taxId exists
      if (length(names(.databases[[database]])) > 0 && taxId %in% names(.databases[[database]])) {

        return(TRUE)

      }

    }

  } else {
    return(FALSE)
  }

}
