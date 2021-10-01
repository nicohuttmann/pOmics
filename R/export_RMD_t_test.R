#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param order.by order rows by given column
#' @param descending arrange in descending order
#' @param view view table
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
export_RMD_t.test <- function(data_, order.by = "p.adjust",
                              descending = F,
                              add.annotations = F,
                              dataset,
                              view = F,
                              input = "data_t.test",
                              output = "data_DT_t.test") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }


  if (add.annotations && "fun_enrich_t.test" %in% names(data_)) {

    data <- data %>%
      dplyr::mutate(Annotations ="")

    for (i in 1:2) {

      if (length(data_[["fun_enrich_t.test"]][[i]]) > 0) {

        for (j in 1:length(data_[["fun_enrich_t.test"]][[i]])) {

          if (!is.null(data_[["fun_enrich_t.test"]][[i]][[j]])) {

            dummy <- data_[["fun_enrich_t.test"]][[i]][[j]] %>%
              data.frame() %>%
              dplyr::pull(geneID, Description) %>%
              sapply(function(x) strsplit_(x, split = "/")) %>%
              {if (is.matrix(.)) as.list(as.data.frame(.))
                else topGO::inverseList(.)}

            data <- data %>%
              dplyr::rowwise() %>%
              dplyr::mutate(Annotations = paste(strsplit_(Annotations, "/"),
                                                dummy[[variables]],
                                                collapse = "/"))

          }

        }

      }

    }

  }

  #
  data <- export_DT_t.test(data = data, order.by = order.by,
                           descending = descending)

  if (view) print(data)

  # Output name
  #if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}
