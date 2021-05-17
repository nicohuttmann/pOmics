#' Exports plot to pdf
#'
#' @param p plot
#' @param file filename
#' @param width width
#' @param height height
#'
#' @return
#' @export
#'
#'
export_pdf <- function(p, file = "test.pdf", width = 4, height = 4) {

  file <- gsub(pattern = ":", replacement = "", x = file)

  pdf(file = file, width = width, height = height)

  print(p)

  dev.off()

}
