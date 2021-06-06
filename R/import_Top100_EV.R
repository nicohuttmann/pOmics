#' Downloads Top100 protein list and converts to protein accession ids
#'
#' @return
#' @export
#'
#'
import_Top100_EV <- function() {

  # Download list
  data <- read.delim("http://microvesicles.org/Archive/EV_TOP_100.txt")

  # Add new database entry
  add_database(database = data[[1]],
               id = "Top100",
               type = "Protein_lists",
               replace = T)

}
