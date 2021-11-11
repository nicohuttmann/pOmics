#' Combines information of identified and quantified proteins
#'
#' @param data_ data_
#' @param input.id data frame of identified proteins
#' @param input.quant data frame of quantified proteins
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_count_id_quant <- function(data_,
                              input.id = "Unique.peptides",
                              input.quant = "LFQ.intensity",
                              output = "data_count_id_quant") {

  # Check data


  # Count
  data_[[output]] <- data_[[input.id]] %>%
    dplyr::rowwise() %>%
    dplyr::mutate(count.id = sum(dplyr::c_across(where(is.logical))),
                  .after = where(is.character)) %>%
    dplyr::ungroup() %>%
    dplyr::select(c(observations, count.id)) %>%
    dplyr::inner_join(
      data_[[input.quant]] %>%
        dplyr::rowwise() %>%
        dplyr::mutate(count.quant = sum(dplyr::c_across(where(is.logical))),
                      .after = where(is.character)) %>%
        dplyr::ungroup() %>%
        dplyr::select(c(observations, count.quant)),
      by = "observations")


  # Return
  return(invisible(data_))

}
