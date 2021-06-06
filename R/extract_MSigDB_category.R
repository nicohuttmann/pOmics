#' Extracts category specific TERM2GENE data frames from local MSigDB data
#'
#' @param category category (see MSig_categories)
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
extract_MSigDB_category <- function(category, dataset) {

  # Get dataset
  dataset <- get_dataset()

  # Get taxId
  taxId <- get_dataset_attr(which = "taxId", dataset = dataset)

  # Check if database exists
  if (!check_database(10090, "MSigDB")) {

    if (!setup_MSigDB(dataset = dataset)) invisible(NULL)

  }


  collections <- msigdbr_collections()


  if (!hasArg(category)) {


    View(collections)

  } else if (category == "all") {

    category <- collections %>%
    dplyr::select(c(1, 2)) %>%
    #mutate(gs_subcat = ifelse(gs_subcat == "", NA, gs_subcat)) %>%
    mutate(name = paste(gs_cat, gs_subcat, sep = "_")) %>%
    pull(name)

  }


  for (cat in category) {


    add_database(database =
                   get_database(id = taxId, type = "MSigDB") %>%
                   dplyr::filter(gs_cat == strsplit_(cat, split = "_")[1]) %>%
                   dplyr::filter(gs_subcat == strsplit_(cat, split = "_")[2]) %>%
                   do_anova(),
                 id = cat,
                 type = "TERM2GENE")


  }




}
