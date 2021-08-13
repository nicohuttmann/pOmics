#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param view view table
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
export_RMD_fun_enrich <- function(data_,
                                  view = F,
                                  input = "fun_enrich",
                                  output = "fun_enrich_DT") {
  
  # Handle input
  input_list <- data_input(data_ = data_, input = input)
  
  if (input_list[["error"]]) return(invisible(input_list[["data"]]))
  
  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }
  
  
  # Prepare all sub data frames
  for (i in seq_along(data)) {
    
    for (j in seq_along(data[[i]])) {
      
      data[[i]][[j]] <- export_DT_fun_enrich(data[[i]][[j]])
      
    }
    
  }
  
  
  if (view) print(data)
  
  # Output name
  #if (!hasArg(output)) output <- input
  
  # Prepare return
  if (list.input) data_[[output]] <- data
  
  else data_ <- data
  
  # Return
  return(data_)
  
}
