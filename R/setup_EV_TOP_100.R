#' Downloads Top 100 EV protein markers from Vesiclepedia
#'
#' @return
#' @export
#'
#'
setup_EV_TOP_100 <- function() {
  
  
  # Download list
  data <- read.delim("http://microvesicles.org/Archive/EV_TOP_100.txt")
  
  
  # Translate Gene symbols to UniProt ids
  data <- select_org(keys = data[[1]],
                     columns = "UNIPROT",
                     output = "vector.rm",
                     keytype = "SYMBOL",
                     OrgDb = "org.Hs.eg.db")
  
  
  # Add new database entry
  add_database(database = data,
               id = "EV_TOP_100",
               type = "Protein_lists",
               replace = T)
  
}
