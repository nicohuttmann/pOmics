#' Downloads and prepares MSigDB annotations
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
setup_msigdb <- function(dataset,
                         keep.loaded = F,
                         save = T,
                         dir = "Data/Databases/") {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Get scientific_name to load database
  scientific_name <- get_dataset_attr(which = "scientific_name",
                                      dataset = dataset)

  # Get taxId
  taxId <- get_dataset_attr(which = "taxId", dataset = dataset)

  # scientific_name available
  if (is.na(scientific_name)) {

    message("No scientific name availale.")
    return(invisible(FALSE))

  }

  # Test species
  species <- msigdbr::msigdbr_species()

  if (!scientific_name %in% species$species_name) {

    message("Species not supported.")
    return(invisible(FALSE))

  }

  # Retrieve database
  msigdb <- msigdbr::msigdbr(species = scientific_name)


  # Keep database loaded in R
  if(keep.loaded) {

    for (i in sort(unique(dplyr::pull(msigdb, gs_cat)))) {

      add_database(database = dplyr::filter(msigdb, gs_cat == !!i),
                   id = i,
                   type = paste0("MSigDB_", taxId),
                   replace = T)

    }

    message("MSigDB loaded.")

  }

  #
  if (save) {

    # Check folder
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = T)
    }

    # Write
    for (i in sort(unique(dplyr::pull(msigdb, gs_cat)))) {

      vroom::vroom_write(x = dplyr::filter(msigdb, gs_cat == !!i),
                         file = paste0("Data/Databases/MSigDB_",
                                       taxId,
                                       "_",
                                       i,
                                       ".tsv.gz"),
                         progress = F)
    }

    message("MSigDB saved in ", dir, ".")

  }

  # Return
  return(invisible(TRUE))

}
