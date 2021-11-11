#' Downloads Top 100 EV protein markers from Vesiclepedia
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
setup_EV_TOP_100 <- function(id = "EV_TOP_100", type = "EV_database") {


  # Download list
  data <- read.delim(url("http://microvesicles.org/Archive/EV_TOP_100.txt"))


  # Translate Gene symbols to UniProt ids
  data <- data %>%
    as_tibble() %>%
    dplyr::rename(SYMBOL = GENE.SYMBOL) %>%
    dplyr::mutate(UNIPROT = select_org(keys = SYMBOL,
                                       columns = "UNIPROT",
                                       output = "vector.na",
                                       keytype = "SYMBOL",
                                       OrgDb = "org.Hs.eg.db"))


  # Add new database entry
  add_database(database = data,
               id = id,
               type = type,
               replace = T)

}
