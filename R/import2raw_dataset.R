#' Extracts variables and grouped data from imported data frame
#'
#' @param import imported data frame
#' @param identifier columns to use as identifiers
#' @param add add raw_dataset to .info list
#'
#' @return
#' @export
#'
#'
import2raw_dataset <- function(import, identifier, add = T) {

  #
  sep <- identify_separator(x = import)

  # Define identifiers
  import <- import %>%
    dplyr::mutate(variables = get_identifiers(x = import,
                                              sep = sep,
                                              identifier = identifier),
                  .before = 1)

  # Build raw dataset
  raw_dataset <- import2grouped_data(import = import)

  attr(x = raw_dataset, which = "name") <- attr(import, "name")

  # Set separator
  attr(x = raw_dataset, which = "separator") <- sep

  if (add) add_raw_dataset(raw_dataset = raw_dataset, name = attr(import, "name"))

  return(raw_dataset)

}
