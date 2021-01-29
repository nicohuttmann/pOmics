#' Checks if database is available
#'
#' @param type type of database
#' @param id database name/identifier
#'
#' @return
#' @export
#'
#'
check_database <- function(id, type = "UniProt") {

  # .databases exists
  if (hasArg(id) && ".databases" %in% objects(all.names = T, envir = .GlobalEnv)) {

    # database entry exists
    if (length(names(.databases)) > 0 && type %in% names(.databases)) {

      # taxId exists
      if (length(names(.databases[[type]])) > 0 && id %in% names(.databases[[type]])) {

        return(TRUE)

      } else {
        return(FALSE)
      }

    } else {
      return(FALSE)
    }


  } else {
    return(FALSE)
  }

}
