#' Views a list of proteins
#'
#' @param proteins vector of proteins
#'
#' @return
#' @export
#'
#' 
view_proteins <- function(proteins) {
  
  table <- tibble::tibble(proteins) %>% 
    dplyr::mutate(Gene = get_variables_data("GENES", proteins)) %>% 
    dplyr::mutate(Name = get_variables_data("PROTEIN-NAMES", proteins)) %>% 
    arrange(Name) %>%
    View()
  
}
