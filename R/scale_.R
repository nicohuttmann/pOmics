#' Scales data (Z-scores) from tibbles or analysis_list containing a tibble
#'
#' @param analysis_list data list
#' @param target.data data to be modified
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
scale_ <- function(analysis_list, target.data = "raw_data") {

  # Data frame input
  if (tibble::is_tibble(analysis_list)) return(analysis_list %>%
                                                 dplyr::mutate(dplyr::across(.cols = where(is.numeric), .fns = scale)))

  else {

    # check data
    if (!hasArg(analysis_list) | !is.list(analysis_list) | !target.data %in% names(analysis_list))
      stop("Please provide a list with the specified data entry.")

    # Scale data
    analysis_list[[target.data]] <- analysis_list[[target.data]] %>%
      dplyr::mutate(dplyr::across(.cols = where(is.numeric), .fns = scale))

    # Return
    return(analysis_list)

  }

}
