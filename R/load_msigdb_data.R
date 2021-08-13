#' Extracts category specific TERM2GENE data frames from local MSigDB data
#'
#' @param category category (see MSig_categories)
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
load_msigdb_data <- function(category, subcategory, dataset) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Get taxId
  taxId <- get_dataset_attr(which = "taxId", dataset = dataset)


  # Search
  files <- list.files(recursive = T)[grepl(paste0("Data/Databases/MSigDB_",
                                                  taxId,
                                                  "_",
                                                  category,
                                                  ".tsv.gz"),
                                           list.files(recursive = T))]

  if (length(files) != 1) {



  } else {

    msigdb.data <- vroom::vroom(file = files,
                                show_col_types = F,
                                progress = F)

  }


  if (hasArg(subcategory) & subcategory %in% msigdb.data[["gs_subcat"]]) {

    msigdb.data <- msigdb.data %>% dplyr::filter(gs_subcat == !!subcategory)

  }




  TERM2GENE <- dplyr::select(msigdb.data, c(gs_name, gene_symbol))

  # id for databases list
  id <- paste0(taxId, "_", category)

  if (hasArg(subcategory)) id <- paste0(id, "_", subcategory)


  # Save in databases
  add_database(database = TERM2GENE,
               id = id,
               type = "MSigDB")


}
