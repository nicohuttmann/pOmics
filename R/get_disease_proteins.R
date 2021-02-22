#' Returns disease associated proteins
#'
#' @param terms disease terms
#'
#' @return
#' @export
#'
#'
get_disease_proteins <- function(terms) {

  # Check if database exists
  if (check_database(id = "DisGeNet", type = "Annotations")) {
    database <- get_database(id = "DisGeNet", type = "Annotations")
  # Not set up
  } else {
    database <- new_annotations_data_DisGeNet(nodeSize = 5,
                                              save = TRUE)
  }

  # No argument or terms do not match
  if (!hasArg(terms) || any(!terms %in% names(database))) {
    # #
    # View(database)
    #
    # while () {
    #
    # }

  }


  # Collect proteins
  proteins <- c()
  for (i in terms) {
    proteins <- unique(c(proteins, database[[i]]))
  }


  # Return
  return(proteins)

}
