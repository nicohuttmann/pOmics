#' Returns  database
#'
#' @param id database name
#' @param type database type
#'
#' @return
#' @export
#'
#'
get_database <- function(id, type) {

  # Check input
  if (!hasArg(id) || !hasArg(type)) {

    message("Incomplete call.")
    return(invisible(NULL))

  }

  # Make id character
  if (is.numeric(id)) id <- as.character(id)

  # Check database
  if (!check_database(id = id, type = type)) {

    message("Database not found.")
    return(invisible(NULL))

  }

  return(.databases[[type]][[id]])

}
