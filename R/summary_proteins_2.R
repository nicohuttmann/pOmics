#' Views a list of proteins with links to UniProt
#'
#' @param proteins vector of proteins
#' @param caption table caption
#' @param sort.names order table by protein names
#'
#' @return
#' @export
#'
#'
summary_proteins_2 <- function(proteins, caption = NULL, sort.names = F) {

  table <- summary_proteins(proteins = proteins,
                            sort.names = sort.names,
                            view = FALSE)

  # Insert links
  table$Protein <- paste0('<a  target=_blank href=',
                          "https://www.uniprot.org/uniprot/",
                          table$Protein, '>', table$Protein,'</a>' )




  # Transform to DT datatable
  table <- DT::datatable(table,
                         escape = FALSE,
                         options = list(
                           columnDefs = list(list(className = 'dt-left', targets = "_all"))
                         ),
                         caption = htmltools::tags$caption(caption, style="color:black; text-align:center"),
                         rownames = FALSE)


  # Return
  return(table)

}
