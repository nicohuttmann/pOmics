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

    data.frames.sel <- select.list(data.frames,
                                   multiple = T,
                                   graphics = F)

    return(data.frames.sel)

  }

}


#' Transfers observations from raw_dataset to .datasets
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
transfer_observations <- function(dataset) {

  # Check data
  if (!is_dataset(dataset) | !is_raw_dataset(dataset))
    stop("Dataset or raw dataset missing.")

  # Add observations data frame
  .datasets[[dataset]][["observations"]] <<- tibble::lst()
  .datasets[[dataset]][["observations"]][["raw"]] <<-
    tibble::tibble(observations =
                     colnames(get_raw_dataset(dataset)
                              [["data.frames"]][[1]])) %>%
    dplyr::mutate(All = TRUE)

}


#' Transfers data from raw_dataset to .datasets
#'
#' @param dataset dataset
#' @param data.columns data frames to transfer
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
transfer_data_frames <- function(dataset, data.columns) {

  # Dataset
  dataset <- get_dataset(dataset)

  # Check data
  if (!is_dataset(dataset) || !is_raw_dataset(dataset))
    stop("Dataset or raw dataset missing.")

  # Add new variables data frame
  if (!"variables" %in% names(.datasets[[dataset]])) {

    .datasets[[dataset]][["variables"]] <<-
      .info[["raw_datasets"]][[dataset]][["variables.data"]] %>%
      dplyr::select(variables)

  }

  # No data columns given
  if (!hasArg(data.columns)) {

    data.columns <- available_data_frames(dataset = dataset,
                                          return = T,
                                          print.call = F)

  }

  # Check which columns are available
  data.columns <-
    data.columns[data.columns %in%
                   names(.info[["raw_datasets"]][[dataset]][["data.frames"]])]

  if (length(data.columns) == 0) {

    print("No data types found.")
    return(invisible(FALSE))

  }

  for(type in data.columns) {

    data <- .info[["raw_datasets"]][[dataset]][["data.frames"]][[type]] %>%
      as.matrix() %>%
      t() %>%
      data2tibble(to.row.names = "observations")

    names(data)[-1] <-
      dplyr::pull(.info[["raw_datasets"]][[dataset]][["variables.data"]],
                  var = "variables")

    # Add variables and observations attributes (make dataframe sticky)


    .datasets[[dataset]][[type]] <<- data

  }

  # Set default data type
  if (is.na(get_default_data_name(dataset))) {
    set_default_data_name(name = data.columns[1], dataset = dataset)
  }

  # Return
  return(invisible(TRUE))

}



#' Transfers variables data from raw_dataset to .datasets
#'
#' @param dataset dataset
#' @param data.columns columns to transfer
#'
#' @return
#' @export
#'
#'
transfer_variables_data <- function(dataset, data.columns) {

  # Dataset
  dataset <- get_dataset(dataset)

  # Check data
  if (!is_dataset(dataset) | !is_raw_dataset(dataset)) stop("Dataset or raw dataset missing.")

  # Add new variables data frame
  if (!"variables" %in% names(.datasets[[dataset]])) {

    .datasets[[dataset]][["variables"]] <<- .info[["raw_datasets"]][[dataset]][["variables.data"]] %>%
      dplyr::select(variables) %>%
      dplyr::mutate(All = TRUE)

  }

  # No data columns given
  if (!hasArg(data.columns)) {

    data.columns <- available_variables_data(dataset = dataset,
                                             return = T,
                                             print.call = F)

  }

  # Check which columns are available
  data.columns <-
    data.columns[data.columns %in%
                   colnames(.info[["raw_datasets"]][[dataset]][["variables.data"]])]

  for(column in data.columns) {

    .datasets[[dataset]][["variables"]] <<- .datasets[[dataset]][["variables"]] %>%
      dplyr::mutate(
        !!column := dplyr::pull(.info[["raw_datasets"]][[dataset]][["variables.data"]], column))

  }

}
