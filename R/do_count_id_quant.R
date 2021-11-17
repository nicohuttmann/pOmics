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


#' Wrapper around do_count_id_quant
#'
#' @param variables variables
#' @param observations observations
#' @param observations.set observations set
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
quick_protein_count <- function(variables, observations, observations.set,
                                dataset) {

  dataset <- get_dataset(dataset)

  variables <- get_variables({{variables}}, dataset)

  observations.set <- get_observations_set(observations.set,
                                           dataset = dataset)

  observations <- get_observations({{observations}}, observations.set, dataset)

  data_ <- get_data(which = "Unique.peptides",
                    variables = variables,
                    observations = observations,
                    observations.set = observations.set,
                    dataset = dataset) %>%
    do_expr(expr = x > 0, input = "Unique.peptides") %>%
    put_data(which = "LFQ.intensity",
             variables = variables,
             observations = observations,
             observations.set = observations.set,
             dataset = dataset) %>%
    do_expr(expr = x > 0, input = "LFQ.intensity") %>%
    do_count_id_quant()

  return(data_)

}
