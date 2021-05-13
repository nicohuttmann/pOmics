#' Sets up the STRING database
#'
#' @param version STRING database version
#' @param score_threshold
#' @param dataset
#'
#' @return
#' @export
#'
#'
setup_STRINGdb <- function(version = "11", score_threshold = 200, dataset) {


  # Get dataset
  dataset <- get_dataset(dataset)

  # Get taxonomy Id
  taxId <- get_dataset_attr(which = "taxId", dataset = dataset)



  # Initiate database object
  string_db <- STRINGdb::STRINGdb$new(version = version,
                             species = taxId,
                             score_threshold = score_threshold,
                             input_directory = "C:/Users/Nico Huettmann/OneDrive - University of Ottawa/Documents/R/projects/hjv-proteomics")



  example1_mapped <- string_db$map(my_data_frame = data.frame(variables = get_variables()[1:100]),
                                    my_data_frame_id_col_names = "variables",
                                    removeUnmappedRows = TRUE)


}
