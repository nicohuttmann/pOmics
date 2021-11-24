#' Pull names vector from
#'
#' @param data_ list or tibble
#' @param values column for vector values; by default second column
#' @param names column for vector names; by default first column
#' @param dataset dataset
#' @param input name of input data
#' @param output name of output data
#' @param output.type (optional) type of output ("vector" to force vector output)
#'
#' @return
#' @export
#'
#'
pull_data <- function(data_,
                      values,
                      names,
                      dataset,
                      input,
                      output,
                      output.type) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else data <- input_list[["data"]]



  # Check values and names column
  if (!hasArg(values))
    values <- colnames(data)[2]

  if (!hasArg(names))
    names <- colnames(data)[1]

  # Transform DF to tibble
  data <- data %>%
    data2tibble(to.row.names = "rownames") %>%
    dplyr::pull(var = !!values, name = !!names)

  if (all(data == names(data))) data <- unname(data)


  # Output name
  if (!hasArg(output)) output <- input_list[["input"]]

  # Prepare return
  if (input_list[["list.input"]] &&
      (!hasArg(output.type) || output.type != "vector")) {
    data_[[output]] <- data
  }

  else data_ <- data

  # Return
  return(data_)

}
