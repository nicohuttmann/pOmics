#' Add variables data and data in matrix form to a dataset
#'
#' @param x raw data
#' @param dataset dataset list
#' @param data.types types of data to be imported
#' @param data.origin origin of data
#' @param min.similarity minimum similarity oi column names
#' @param min.groupsize minimum number of samples
#'
#' @return
#' @export
#'
#'
add_data2dataset <- function(x, dataset, data.types, data.origin, min.similarity = 8, min.groupsize = 6) {


  # Aggregate data frame
  raw.dataset <- x %>%
    df2groupedList(min.similarity = min.similarity, min.groupsize = min.groupsize) %>%
    sep_vector2list(sep = attr(dataset, "separator")) %>%
    list_entries2vector()





  # Change data.types if data.origin is specified
  if (hasArg(data.origin) && get_defaults(data.origin = data.origin)) {

    # Get default data types
    if (!hasArg(data.types)) data.types <- get_defaults(data.origin = data.origin,
                                                        type =  "data.types",
                                                        silent = TRUE)

    names(data.types) <- data.types
    # Check each given argument
    for (i in seq_along(data.types)) {
      repl <- get_defaults(data.origin = data.origin, type = data.types[i], silent = TRUE)
      if (is.list(repl)) data.types[i] <- repl$column
    }
  # Data types given
  } else if (hasArg(data.types)) {
    names(data.types) <- data.types
  # Filler
  } else {
    data.types <- c()
  }




  # Check if datatypes are present in imported data
  if (!all(data.types %in% names(raw.dataset))) {

    message("Not all data types were found.")

    data.types <- select.list(choices = names(raw.dataset),
                              preselect = data.types,
                              multiple = TRUE,
                              title = "Select all data types to be extracted from raw data.",
                              graphics = TRUE)
    names(data.types) <- data.types

  }



  # Add data
  matrix.types <- unlist(lapply(raw.dataset[data.types], is.matrix))


  #
  if (length(dataset) == 0 || !"variables" %in% names(dataset)) dataset[["variables"]] <- NA

  if (!"observations" %in% names(dataset)) dataset[["observations"]] <- NA



  # Add matrix data
  for (i in which(matrix.types)) {

    dataset[[names(data.types)[i]]] <- tibble::lst()
    dataset[[names(data.types)[i]]][["raw"]] <- raw.dataset[[data.types[i]]]

    # Set default LFQ dataset
    attr(dataset, paste0("default_", names(data.types)[i])) <- "raw"

    if (is.null(attr(dataset, "default_data_type"))) attr(dataset, "default_data_type") <- names(data.types)[i]

  }



  # Build and add variables dataframe
  variables <- tibble::tibble(variables = rownames(x))

  for (i in which(!matrix.types)) {

    varname <- names(data.types)[i]
    variables <- variables %>%
      mutate(!!varname := raw.dataset[[data.types[i]]])

  }

  # Add Names data frame
  dataset[["variables"]] <- variables %>% dplyr::mutate(All = TRUE)

  # Return
  return(dataset)

}
