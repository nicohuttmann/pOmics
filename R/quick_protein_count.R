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
quick_protein_count <- function(variables, observations, observations.set, dataset) {

  dataset <- get_dataset(dataset)

  variables <- get_variables({{variables}}, dataset)

  observations.set <- get_observations_set(observations.set, dataset)

  observations <- get_observations({{observations}}, observations.set, dataset)

  return(get_data(variables = variables,
                  observations = observations,which = "Peptides",

                  observations.set = observations.set,
                  dataset = dataset) %>%
    eval_data(expr = x > 0, input = "Peptides") %>%
    put_data(variables = variables,
             observations = observations,which = "LFQ.intensity",

             observations.set = observations.set,
             dataset = dataset) %>%
    eval_data(expr = x > 0, input = "LFQ.intensity") %>%
    do_count_id_quant())



}
