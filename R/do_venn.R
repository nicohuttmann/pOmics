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
do_venn <- function(data_, ..., dataset, input, output) {

  # Check for ggvenn package
  if (!requireNamespace("ggvenn", quietly = TRUE)) {
    stop(
      "Package \"ggvenn\" must be installed to use this function.",
      call. = FALSE
    )
  }


  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    data_attributes <- input_list[["data_attributes"]]
  }


  # Check for list input
  if (is.list(data)) {
    data <- list2tibble(data, identifier = "groups")
  }


  # 2 intersections
  if (sum(unlist(lapply(data, typeof)) == "logical") == 2) {

    p <- ggplot2::ggplot(data) +
      ggvenn::geom_venn(mapping = aes(A = !!rlang::sym(colnames(data)[2]), B = !!rlang::sym(colnames(data)[3])),
                        digits = 1,
                        stroke_size = 1 / .pt * 4 / 3,
                        text_size = 8 / .pt,
                        set_name_size = 8 / .pt,
                        fill_color = "white") +
      coord_fixed() +
      theme_void()

    # 3 intersections
  } else if (sum(unlist(lapply(data, typeof)) == "logical") == 3) {

    p <- ggplot2::ggplot(data) +
      ggvenn::geom_venn(mapping = ggplot2::aes(A = !!rlang::sym(colnames(data)[2]),
                                               B = !!rlang::sym(colnames(data)[3]), C = !!rlang::sym(colnames(data)[4])),
                        digits = 1,
                        stroke_size = 1 / .pt * 4 / 3,
                        text_size = 8 / .pt,
                        set_name_size = 8 / .pt,
                        fill_color = "white") +
      coord_fixed() +
      theme_void()

    # 3 intersections
  } else if (sum(unlist(lapply(data, typeof)) == "logical") == 4) {

    p <- ggplot2::ggplot(data) +
      ggvenn::geom_venn(mapping = ggplot2::aes(A = !!rlang::sym(colnames(data)[2]),
                                               B = !!rlang::sym(colnames(data)[3]),
                                               C = !!rlang::sym(colnames(data)[4]),
                                               D = !!rlang::sym(colnames(data)[5])),
                        digits = 1,
                        stroke_size = 1 / .pt * 4 / 3,
                        text_size = 8 / .pt,
                        set_name_size = 8 / .pt,
                        fill_color = "white") +
      coord_fixed() +
      theme_void()

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
