#' Shows available raw data frames and returns them or prints a transfer call
#'
#' @param dataset dataset
#' @param return return data frame names
#' @param print.call print call for transferring data frames
#'
#' @return
#' @export
#'
#'
available_data_frames <- function(dataset, return = F, print.call = F) {

  # Dataset
  dataset <- get_dataset(dataset)

  # All available data frame names
  data.frames <- names(.info[["raw_datasets"]][[dataset]][["data.frames"]])

  # View
  View(data.frame(data.frames))

  # Select data.columns
  if (return | print.call) {

    data.frames <- select.list(data.frames, multiple = T, graphics = F)

  }

  # Print call
  if (print.call) {

    cat(paste0('transfer_data_frames(dataset = "',
               dataset,
               '",\n\t',
               'data.columns = c(\n\t"',
               paste(data.frames, collapse = '",\n\t\t"'),
               '"))'))

  #  Return data frames names
  } else if (return) {

    return(data.frames)

  }

}
