#' Formats table for nice output (R Markdown)
#'
#' @param data enrichResult
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
export_DT_fun_enrich <- function(data, dataset) {
  
  if (class(data) != "enrichResult") {
    
    if (!silent) message("No enrichResult object given.")
    
    return(invisible(NULL))
    
  }
  
  # Dataset
  dataset <- get_dataset(dataset)
  
  OrgDb <- get_OrgDb(dataset = dataset)
  
  # Extract data
  data_df <- data %>% 
    as.data.frame() %>% 
    as_tibble()
  
  data_table <- data %>% 
    clusterProfiler::setReadable(OrgDb = OrgDb, keyType = "UNIPROT") %>% 
    as.data.frame() %>% 
    as_tibble %>% 
    dplyr::mutate(UniProt = data_df$geneID, .after = "geneID") %>% 
    dplyr::select(-Count) %>% 
    dplyr::mutate(pvalue = nice_number(pvalue, format = "e", round = F, sci.digits = 2, output = "character")) %>%
    dplyr::mutate(p.adjust = nice_number(p.adjust, format = "e", round = F, sci.digits = 2, output = "character")) %>%
    dplyr::mutate(qvalue = nice_number(qvalue, format = "e", round = F, sci.digits = 2, output = "character"))
  
  
  
  
  if (all(data_table$pvalue == data_table$p.adjust)) data_table <- dplyr::select(data_table, -p.adjust)
  
  

  # Transform to DT datatable
  table <- DT::datatable(data_table,
                         escape = FALSE,
                         options = list(
                           columnDefs = list(list(className = 'dt-left', targets = "_all"))),
                         rownames = FALSE)
  
  return(table)
  
}
