#' Help make nice number in R Markdown
#'
#' @param x number
#' @param round should number be rounded
#' @param digits round to number of digits after comma
#' @param format number format (see formatC())
#' @param big.mark separotor between 3 digits; remove with ""
#'
#' @return
#' @export
#'
#'
nice_number <- function(x, round = T, digits = 0, format = "d", big.mark = ",") {
  
  if (round) x <- round(x, digits = digits)
  
  if (big.mark != "") x <- formatC(x, format = format, big.mark = big.mark)
  
  # Return
  return(x)
  
}
