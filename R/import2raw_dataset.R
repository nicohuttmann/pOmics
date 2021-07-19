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

  identifiers <- identify_variables(x = import,
                                    identifier = identifier,
                                    sep = sep)

  # Define identifiers
  import <- import %>%
    dplyr::mutate(variables = identifiers, .before = 1)

  # Build raw dataset
  raw_dataset <- import2grouped_data(import = import)

  attr(x = raw_dataset, which = "name") <- attr(import, "name")
  attr(x = raw_dataset, which = "project") <- attr(import, "project")
  attr(x = raw_dataset, which = "datatype") <- attr(import, "datatype")

  # Set separator
  attr(x = raw_dataset, which = "separator") <- sep

  if (add)
    add_raw_dataset(raw_dataset = raw_dataset, name = attr(import, "name"))

  return(raw_dataset)

}
