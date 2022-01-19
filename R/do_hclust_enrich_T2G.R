#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param dend which dendogram to work with
#' @param ... specific arguments
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_hclust_enrich_T2G <- function(data_,
                             dend = "dend_y",
                             max.k = 8,
                             ...,
                             dataset,
                             input,
                             output) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }



  # Do literally nothing
  dend <- data[[dend]]


  database = .databases[["UniProt"]][["SUBCELLULAR-LOCATIONS"]]


  cluster_list <- list()

  for (k in 2:max.k) {

    cluster_list[[as.character(k)]] <- dend %>%
      cutree_(k = k) %>%
      fun_enrich(database = database, view = F) #%>%
      #merge_enrichment_results_recursive()

  }







  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- .set_data_attributes(data, data_attributes)
    attr(data_, "data") <- output
  }

  else data_ <- .set_data_attributes(data, data_attributes)

  # Return
  return(data_)

}
