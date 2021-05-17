#' Sets up the STRING database
#'
#' @param version STRING database version
#' @param score_threshold minimum score for interaction
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
setup_STRINGdb <- function(version = "11", score_threshold = 400, dataset, input_directory = getwd()) {


  # Get dataset
  dataset <- get_dataset(dataset)

  # Get taxonomy Id
  taxId <- get_dataset_attr(which = "taxId", dataset = dataset)



  # Initiate database object
  STRINGdb::STRINGdb$new(version = version,
                             species = taxId,
                             score_threshold = score_threshold,
                             input_directory = input_directory) %>%
  add_database(id = as.character(taxId), type = "STRING", replace = TRUE)

  # Map all proteins to STRING Ids
  STRING_id <- get_database(id = taxId, type = "STRING")$map(my_data_frame = data.frame(variables = get_all_variables()),
                                    my_data_frame_id_col_names = "variables",
                                    removeUnmappedRows = T)

  # Remove duplicated entries
  STRING_id <- STRING_id[!duplicated(STRING_id[[1]]), ]
  rownames(STRING_id) <- c()

  # Dataframe to vector
  STRING_id_v <- pOmics::data2vector(STRING_id)


  # Add Ids to all variables_data frames
  for (dataset in get_datasets()) {

    ids <- STRING_id_v[intersect(get_variables(variables = All, dataset = dataset),
                                               names(STRING_id_v))]

    add_variables_data(data = ids,
                       name = "STRING_id",
                       dataset = dataset,
                       replace = T)

  }





}
