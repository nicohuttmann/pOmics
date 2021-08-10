#' Prints latex table code to console (made for abbreviations table of geegeedown)
#'
#' @param table position of table file
#' @param line.start character to start lines
#' @param sort.abc sort table alphabetically
#'
#' @return
#' @export
#'
#'
latex_abbr_table <- function(table, line.start = "\t\t", sort.abc = T) {

  if (!hasArg(table)) table <- choose.files()

  table <- openxlsx::read.xlsx(xlsxFile = table, colNames = F)

  if (sort.abc) {

    table <- table[order(table[, 1]), ]

  }

  text <- paste0(table[, 1], " & &  ", table[, 2])

  collapse <- paste0("\\\\ \n", line.start)

  fulltext <- paste(text, collapse = collapse)

  cat(fulltext)

}
