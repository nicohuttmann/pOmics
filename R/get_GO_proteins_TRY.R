#' Returns GO terms from given terms
#'
#' @param terms GO ids
#'
#' @return
#' @export
#'
#'
get_GO_proteins_TRY <- function(terms) {


  # Check if database exists
  if (check_database(id = "GO-ID", type = "Annotations")) {
    database <- get_database(id = "GO-ID", type = "Annotations")
    # Not set up
  } else {
    database <- new_annotations_data_UniProt(annotation = "GO-ID",
                                             dataset = dataset,
                                             save = T)
  }


  # Inverse database list
  database <- topGO::inverseList(database)




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
