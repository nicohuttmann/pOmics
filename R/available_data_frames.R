#' Shows available raw data frames and returns them or prints a transfer call
#'
#' @param dataset dataset
#' @param view View?
#' @param return return data frame names
#' @param print.call print call for transferring data frames
#'
#' @return
#' @export
#'
#'
available_data_frames <- function(dataset, view = T, return = F,
                                  print.call = F) {

  # Dataset
  dataset <- get_dataset(dataset)

  # All available data frame names
  data.frames <- names(.info[["raw_datasets"]][[dataset]][["data.frames"]])

  # View
  if (view) View(data.frame(data.frames))


  # Print call
  if (print.call) {

    data.frames.sel <- select.list(data.frames,
                                   multiple = T,
                                   graphics = F)

    call_str <- paste0('transfer_data_frames(dataset = "',
               dataset,
               '",\n\t',
               'data.columns = c("',
               paste(data.frames.sel, collapse = '",\n\t\t"'),
               '"))')
    writeClipboard(call_str)

    cat(call_str)

  #  Return data frames names
  } else if (return) {

    return(data.frames)

  }

}
