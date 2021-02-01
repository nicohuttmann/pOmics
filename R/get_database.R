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
  if (!hasArg(id) || !hasArg(type)) stop("Incomplete call.")

  # Make id character
  if (is.numeric(id)) id <- as.character(id)

  # Check database
  if (!check_database(id = id, type = type)) stop("Database not found.")

  return(.databases[[type]][[id]])

}
