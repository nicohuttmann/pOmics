#' INCOMOPLETE FUNCTION: Combines data frames with variables info 
#'
#' @param input list of protein data frames
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>% 
#' 
total_proteins_summary <- function(input, dataset) {
  
  dataset <- get_dataset(dataset)
  
  
  output <- .datasets[[dataset]][["variables"]]
  
  
  for (i in seq_along(input)) {
    
    dummy <- input[[i]] %>% 
      include_observations_data("labels") %>% 
      transpose_tibble(from.row.names = "labels")
    
    names(dummy)[-1] <- paste0(names(input)[i], "_", names(dummy)[-1])
    
    output <- dplyr::left_join(output, dummy, by = "variables")
    
  }
  
  output <- DT::datatable(output,
                          escape = FALSE,
                          options = list(
                            columnDefs = list(list(className = 'dt-left',
                                                   targets = "_all"))),
                          rownames = FALSE)
  
  return(output)
  
}
