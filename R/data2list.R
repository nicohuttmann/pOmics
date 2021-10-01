#' Pull names vector from
#'
#' @param data_ list or tibble
#' @param names column for vector names; by default first column
#' @param input name of input data
#' @param output name of output data
#' @param output.type (optional) type of output ("list" to force list output)
#'
#' @return
#' @export
#'
#'
data2list <- function(data_, names, input, output, output.type) {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used
    
  }


  # Check names input
  if (!hasArg(names))
    names <- colnames(data)[1]

  # Check data
  if ((!is.character(data[[names]]) && !is.factor(data[[names]])) ||
      !any(unlist(lapply(data[-1], typeof)) == "logical")) {
    stop("Dataframe must consist of one character and n logical columns.
         Please specify character column with <names>.")
  }


  # Prepare list to add colnames from data
  data.list <- tibble::lst()

  # Iterate over logical columns
  for (i in colnames(data)[unlist(lapply(data, typeof)) == "logical"]) {
    data.list[[i]] <- data %>%
      dplyr::filter(rlang::eval_tidy(rlang::parse_expr(paste0("`", i, "`")))) %>%
      dplyr::pull(1)
  }



  # Output name
  if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]] && (!hasArg(output.type) || output.type != "list")) {
    data_[[output]] <- data.list
  }

  else data_ <- data.list

  # Return
  return(data_)

}
