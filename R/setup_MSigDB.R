#' Downloads and prepares MSigDB annotations
#'
#' @param dataset dataset
#' @param category categories to extract for enrichment
#'
#' @return
#' @export
#'
#'
setup_MSigDB <- function(dataset, category) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Get scientific_name to load database
  scientific_name <- get_dataset_attr(which = "scientific_name", dataset = dataset)

  # Get taxId
  taxId <- get_dataset_attr(which = "taxId", dataset = dataset)

  # scientific_name available
  if (is.na(scientific_name)) {

    message("No scientific name availale.")
    invisible(FALSE)

  }

  # Test species
  species <- msigdbr::msigdbr_species()

  if (!scientific_name %in% species$species_name) {

    message("Species not supported.")
    invisible(FALSE)

  }

  add_database(database = msigdbr::msigdbr(species = scientific_name),
               id = taxId,
               type = "MSigDB",
               replace = T)



  if (hasArg(categories)) {



  }




}
