#' Combines variables in one dataset
#'
#' @param datasets names of datasets
#' @param name name of combined dataset
#'
#' @return
#' @export
#'
#'
add_combined_dataset <- function(datasets = "all", name) {


  if (datasets == "all") {

    if (!hasArg(name)) {

      name <- "all"

    }

    datasets <- get_datasets()

  } else if (!hasArg(name)) {

    name <- paste(datasets, collapse = "_")

  }


  # Check datasets
  if (!all(datasets %in% get_datasets())) stop("Not all datasets found.")

  #if (length(datasets) == 1) stop("More than one dataset must be selected.")



  add_dataset(name = name)


  if (length(datasets) == 1) {

    dataset.comb <- .datasets[[datasets[1]]][["variables"]] %>%
      dplyr::select(variables) %>%
      dplyr::mutate(All = TRUE)

  } else {

    dataset.comb <- dplyr::full_join(x = .datasets[[datasets[1]]][["variables"]],
                                     y = .datasets[[datasets[2]]][["variables"]],
                                     by = "variables",
                                     suffix = c(paste0("_", c(datasets[1:2])))) %>%
      dplyr::select(variables) %>%
      dplyr::mutate(All = TRUE)

  }



  if (length(datasets) > 2) {

    for (dataset in datasets[-c(1:2)]) {

      dataset.comb <- dplyr::full_join(x = dataset.comb,
                                       y = .datasets[[dataset]][["variables"]],
                                       by = "variables",
                                       suffix = c("", paste0("_", dataset))) %>%
        dplyr::select(variables)

    }

  }


  .datasets[[name]][["variables"]] <<- dataset.comb


}
