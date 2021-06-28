#' Adds new database to .databases
#'
#' @param database new database
#' @param id database id
#' @param type database type
#' @param replace Should existing database be replaced?
#'
#' @return
#' @export
#'
#'
add_database <- function(database, id, type, replace = F) {

  # Check databases list
  new_databases_list()

  # Check input
  if (!hasArg(database) || !hasArg(id) || !hasArg(type)) stop("No database given.")

  # Check if name already exists
  if (check_database(id = id, type = type)) {

    if (!replace) {
      id2 <- id
      x <- 1
      while(check_database(id = id, type = type)) {
        id <- paste0(id2, x)
        x <- x + 1
      }

    }

  }

  # Save in .databases list
  .databases[[type]][[as.character(id)]] <<- database

}
