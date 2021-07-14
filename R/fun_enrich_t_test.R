#' Functional enrichment of t.test results
#'
#' @param data_ list or tibble
#' @param pValueThreshold threshold for t.test p-value
#' @param log2fcThreshold abs. threshold for log2 fold-change
#' @param view view results
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
fun_enrich_t.test <- function(data_, pValueThreshold = 0.05, log2fcThreshold = 0, ..., view = F, dataset, input = "data_t.test", output = "fun_enrich_t.test") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }


  # Upregulated proteins

  data_fun_enrich <- list()

  data_fun_enrich[["up"]] <- fun_enrich(
    proteins = data %>%
      dplyr::filter(p.adjust < pValueThreshold) %>%
      dplyr::filter(log2.fc > log2fcThreshold) %>%
      pull("variables"),
    background = data %>%
      pull("variables"),
    view = view,
    ...
  )

  data_fun_enrich[["down"]] <- fun_enrich(
    proteins = data %>%
      dplyr::filter(p.adjust < pValueThreshold) %>%
      dplyr::filter(log2.fc < log2fcThreshold) %>%
      pull("variables"),
    background = data %>%
      pull("variables"),
    view = view,
    ...
  )

  #barplot(data_fun_enrich[["down"]][["KEGG"]])




  # Output name
  #if (!hasArg(output)) output <- input

  # Prepare return
  if (list.input) data_[[output]] <- data_fun_enrich

  else data_ <- data

  # Return
  return(data_)

}
