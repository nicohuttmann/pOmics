#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param scale scale data
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_pca <- function(data_, scale = T, dataset, input, output = "data_pca") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }


  # Scale data
  if (scale) {
    data <- do_scale(data)
    if (input_list[["list.input"]]) data_[[paste0(input, "_scaled")]] <- data
  }


  # Compute PCA
  data.prcomp <- data %>%
    dplyr::select(where(is.numeric)) %>%
    prcomp()


  data <- data %>%
    dplyr::select(-where(is.numeric)) %>%
    cbind(data.prcomp[["x"]])

  # # PCA summary
  # data_[["pca_summary"]] <- summary(data_[["pca_object"]])
  #
  # # Print summary
  # if (print.summary) print(data_[["pca_summary"]])
  #
  # # PCA summary
  # data_[["pca_data"]] <- as_tibble(data_[["pca_object"]][["x"]]) %>%
  #   dplyr::mutate(dplyr::select(data, -where(is.numeric)),
  # .before = where(is.numeric))
  #




  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    data_[["data_prcomp"]] <- data.prcomp
  }



  else data_ <- data

  # Return
  return(data_)

}
