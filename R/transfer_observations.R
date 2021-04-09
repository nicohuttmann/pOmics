#' Transfers observations from raw_dataset to .datasets
#'
#' @param name dataset name
#'
#' @return
#' @export
#'
#'
transfer_observations <- function(name) {

  # Check data
  if (!is_dataset(name) | !is_raw_dataset(name)) stop("Dataset or raw dataset missing.")

  # Add observations data frame
  .datasets[[name]][["observations"]] <<- tibble::lst()
  .datasets[[name]][["observations"]][["raw"]] <<- tibble::tibble(observations = colnames(get_raw_dataset(name)[["data.frames"]][[1]])) %>%
    dplyr::mutate(All = TRUE)

}
