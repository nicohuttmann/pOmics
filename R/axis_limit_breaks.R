#' Helps make nice axis limit breaks
#'
#' @param values data values
#' @param break.space space between breaks
#'
#' @return
#' @export
#'
#'
axis_limit_breaks <- function(values, break.space = 2) {


  output <- list()


  output[["limits"]] <- c(floor(min(values) / break.space) * break.space,
                          ceiling(max(values) / break.space) * break.space)

  output[["breaks"]] <- seq(output[["limits"]][1],
                            output[["limits"]][2],
                            break.space)

  # Return
  return(output)

}
