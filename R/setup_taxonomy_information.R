#' Sets up annotaion database for biological Id translation and functional enrichment
#'
#' @param dataset dataset
#' @param taxId taxonomy Id
#' @param OrgDb name of annotation package from Bioconductor
#'
#' @return
#' @export
#'
#'
setup_taxonomy_information <- function(dataset, taxId, OrgDb) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Taxonomy Id
  if (!hasArg(taxId)) {
    taxId <- identify_taxId(get_variables(dataset = dataset), silent = T)
  }

  # Choose if not found or given
  if (taxId == 0) {

    taxId <- choose_taxId()

  }

  # Species name
  scientific_name <- suppressMessages(UniProt.ws::lookupUniprotSpeciesFromTaxId(taxId))

  # Kegg code
  kegg_code <- clusterProfiler::search_kegg_organism(str = scientific_name,
                                                     by = "scientific_name")[1, 1]

  # Common name (not important)
  common_name <- clusterProfiler::search_kegg_organism(str = scientific_name,
                                                       by = "scientific_name")[1, 3]

  # Remove
  rm(kegg_species, pos = globalenv())

  # Set attributes
  set_dataset_attr(x = taxId, which = "taxId", dataset = dataset)

  set_dataset_attr(x = scientific_name, which = "scientific_name", dataset = dataset)

  set_dataset_attr(x = kegg_code, which = "kegg_code", dataset = dataset)

  set_dataset_attr(x = common_name, which = "common_name", dataset = dataset)


  cat(paste0("TaxID: ", taxId, "\n"))
  cat(paste0("Scientific name: ", scientific_name, "\n"))
  cat(paste0("KEGG code: ", kegg_code, "\n"))
  cat(paste0("Common name: ", common_name, "\n"))


}
