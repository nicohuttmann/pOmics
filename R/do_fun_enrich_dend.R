#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param ... specific arguments
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_fun_enrich_dend <- function(data_,
                               dend = "dend_y",
                               database,
                               max.k = 10,
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


  # Check given TERM2GENE
  if (!hasArg(TERM2GENE)) {

    message("Please provide a database for enrichment as name or data frame.")

    return(FALSE)

  }


  # Get correct dendrogram to analyze
  data_dend <- data[[dend]]


  # Enrich clusters 2 - max.k
  cluster_list <- list()

  #
  if (is.data.frame(database) || is.character(database)) {

    for (k in 2:max.k) {

    cluster_list[[as.character(k)]] <- data_dend %>%
      cutree_(k = k) %>%
      fun_enrich(database = database, view = F)

    }

  cluster_df <- cluster_list %>%
    enrichResult2data.frame() %>%
    merge_enrichResults()

  # Quantitative value for enrichment
  } else {

    cluster_list <- enrich_dend_ks(dend.table = dend2data.frame(data_dend),
                                   protein.scores = -database, n = max.k) %>%
      eval_dend_enrich()

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
