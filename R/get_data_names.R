#' Prints or returns data names
#'
#' @param type data type
#' @param dataset dataset
#' @param print print
#' @param return return
#'
#' @return
#' @export
#'
#'
get_data_names <- function(type, dataset, print = T, return = F) {

  # No dataset given
  if (!hasArg(dataset)) {
    dataset <- get_datasets(print = FALSE, return = T)
  } else {
    dataset <- get_dataset(dataset)
  }


  #
  for (i in dataset) {

    # No data type given
    if (!hasArg(type)) {
      types <- get_data_types(dataset = i, print = FALSE, return = T)
    } else {
      types <- get_data_type(type)
    }

    # Print
    if(print) {

      message(paste0("Dataset: ", i))

      # Print all combinations
      for (j in types) {

        message(paste0("  Data type: ", j))
        print(attr(.datasets[[i]], j))
        print(attr(.datasets[[i]], paste0("default_", j)))

      }

    }

  }


  #
  if (return && length(dataset) == 1 && length(type) == 1) return(attr(.datasets[[dataset]], type))

}
