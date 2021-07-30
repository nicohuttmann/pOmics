#' Shows available raw variables data and returns them or prints a transfer call
#'
#' @param dataset dataset
#' @param return return variables data names
#' @param print.call print call for transferring variables data
#'
#' @return
#' @export
#'
#'
available_variables_data <- function(dataset, return = F, print.call = F) {

  # Dataset
  dataset <- get_dataset(dataset)

  # All available variables data names
  variables.data <-
    names(.info[["raw_datasets"]][[dataset]][["variables.data"]])

  # View
  View(data.frame(variables.data))

  # Select data.columns
  if (return | print.call) {

    variables.data <- select.list(variables.data, multiple = T, graphics = F)

  }

  # Print call
  if (print.call) {

    cat(paste0('transfer_variables_data(dataset = "',
               dataset,
               '",\n\t',
               'data.columns = c(\n\t"',
               paste(variables.data, collapse = '",\n\t\t"'),
               '"))'))


  # Return data frames names
  } else if (return) {

    return(variables.data)

  }

}
