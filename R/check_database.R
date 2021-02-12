#' Checks if database is available
#'
#' @param type type of database
#' @param id database name/identifier
#'
#' @return
#' @export
#'
#'
check_database <- function(id, type) {

  #
  if (!hasArg(id) && !hasArg(type)) stop("No names specified.")

  # .databases exists
  if (".databases" %in% objects(all.names = T, envir = .GlobalEnv) && length(names(.databases)) > 0) {

    # Check database by id
    if (hasArg(id)) {

      # database type given?
      if (!hasArg(type)) {
        type <- names(.databases)
      }

      for (type in type) {

        # database entry exists
        if (type %in% names(.databases)) {

          # make taxIds characters
          if (is.numeric(id)) id <- as.character(id)
          # id exists
          if (length(names(.databases[[type]])) > 0 && id %in% names(.databases[[type]])) {

            return(TRUE)

          }

        }

      }

    # Check database type
    } else {
      if (type %in% names(.databases)) return(TRUE)
    }



  }

  # Database not found
  return(FALSE)

}
