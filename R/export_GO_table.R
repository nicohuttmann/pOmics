#' Prepares GO table for output in HTML file
#'
#' @param table GO results table
#' @param caption table caption
#'
#' @return
#' @export
#'
#'
export_GO_table <- function(table, caption = NULL) {

  # Check input
  if (is.list(table) && !is.data.frame(table) && length(table) == 1) table <- table[[1]]

  else if (is.list(table)&& !is.data.frame(table) && length(table) > 1) {
    message("Only first table in list used.")
    table <- table[[1]]
  } else if (!is.data.frame(table)) {
    stop("Either provide a list or a data frame.")
  }

  # Insert links
  table$GO.ID <- paste0('<a  target=_blank href=',
                          "https://www.ebi.ac.uk/QuickGO/term/",
                          table$GO.ID, '>', table$GO.ID,'</a>' )


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
