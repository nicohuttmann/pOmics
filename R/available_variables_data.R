#' Shows available raw variables data and returns them or prints a transfer call
#'
#' @param dataset dataset
#' @param view View?
#' @param return return all variables data names
#' @param print.call print call for transferring variables data
#'
#' @return
#' @export
#'
#'
available_variables_data <- function(dataset, view = T, return = F,
                                     print.call = F) {

  # Dataset
  dataset <- get_dataset(dataset)

  # All available variables data names
  variables.data <-
    names(.info[["raw_datasets"]][[dataset]][["variables.data"]])

  # View
  if (view) View(data.frame(variables.data))



  # Print call
  if (print.call) {

    variables.data.sel <- select.list(variables.data,
                                      multiple = T,
                                      graphics = F)

    call_str <- paste0('transfer_variables_data(dataset = "',
               dataset,
               '",\n\t',
               'data.columns = c("',
               paste(variables.data.sel, collapse = '",\n\t\t"'),
               '"))')

    writeClipboard(call_str)

    cat(call_str)


  # Return data frames names
  } else if (return) {

    return(variables.data)

  }

}
