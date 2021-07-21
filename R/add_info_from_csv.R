#' Adds all info from imported csv file to observations data
#'
#' @param x imported info file
#' @param observations.set observations.set
#' @param dataset dataset
#' @param id.column column containing id of observations
#' @param data.columns (optional) specific data columns to add
#' @param order.by column to order observations based on
#' @param character2factor transform character data to factors
#'
#' @return
#' @export
#'
#'
add_info_from_csv <- function(x, observations.set, dataset, id.column = "id",
                              data.columns, order.by = "name",
                              character2factor = T) {

  # Check input
  if (!hasArg(x)) {
    print("No info file given.")
    return(invisible(FALSE))
  } else if (!is.data.frame(x)) {
    print("Given info file is no data frame.")
    return(invisible(FALSE))
  }

  # Check column names
  if (!id.column %in% colnames(x)) {
    print("Id column can not be found in data frame.")
    return(invisible(FALSE))
  }

  # Dataset
  dataset <- get_dataset(dataset)

  # Observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)

  # Define data columns if not given
  if (!hasArg(data.columns)) data.columns <- setdiff(colnames(x), id.column)

  # Check data columns
  else data.columns <- data.columns[data.columns %in% colnames(x)]



  # Add data columns
  for (i in data.columns) {

    dummy <- dplyr::pull(x,
                         var = i,
                         name = id.column)

    if (character2factor & is.character(dummy)) {
      dummy <- factor(x = dummy, levels = unique(dummy))
    }

    add_observations_data(data = dummy,
                          name = i,
                          observations.set = observations.set,
                          dataset = dataset,
                          order.by = order.by == i)
  }

}
