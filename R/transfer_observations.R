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
                     coldatasets(get_raw_dataset(dataset)
                                 [["data.frames"]][[1]])) %>%
    dplyr::mutate(All = TRUE)

}
