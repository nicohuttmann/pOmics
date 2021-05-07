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
setup_annotations <- function(dataset, taxId, OrgDb) {

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

  OrgDb_list <- list("Homo sapiens" = "org.Hs.eg.db",
                     "Mus musculus" = "org.Mm.eg.db")

  # Annotation database
  if (scientific_name %in% names(OrgDb_list)) {

    if (!hasArg(OrgDb)) {

      OrgDb <- OrgDb_list[[scientific_name]]

    }

    # Check if available to load
    if (!requireNamespace(package = OrgDb, quietly = TRUE)) {

      # Install
      # Check BiocManager
      if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

      # Install annotation database
      BiocManager::install(OrgDb)

    }


  } else {

    OrgDb <- ask_name(OrgDb, message = "Please provide the name of a organism annotation database. You can find the names here https://www.bioconductor.org/packages/release/data/annotation/")

    # Check if available to load
    if (!requireNamespace(package = OrgDb, quietly = TRUE)) {

      # Install
      # Check BiocManager
      if (!requireNamespace("BiocManager", quietly = TRUE))
        install.packages("BiocManager")

      # Install annotation database
      BiocManager::install(OrgDb)

    }

  }





  # Check if all worked
  if (requireNamespace(package = OrgDb, quietly = TRUE)) {

    # Attach package for clusterProfiler
    suppressPackageStartupMessages(library(OrgDb, quietly = TRUE, character.only = TRUE))

    set_dataset_attr(x = taxId, which = "taxId", dataset = dataset)

    set_dataset_attr(x = scientific_name, which = "scientific_name", dataset = dataset)

    set_dataset_attr(x = kegg_code, which = "kegg_code", dataset = dataset)

    set_dataset_attr(x = common_name, which = "common_name", dataset = dataset)

    set_dataset_attr(x = OrgDb, which = "OrgDb", dataset = dataset)

    invisible(TRUE)

  } else {

    cat("Annotation database not setup properly. Make sure the Annotation package can be installed or install it manuall from Bioconductor.")

    invisible(FALSE)

  }

}
