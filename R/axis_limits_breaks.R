#' Title
#'
#' @param values
#' @param break.space
#'
#' @return
#' @export
#'
#'
axis_limits_breaks <- function(values, break.space = 2) {


  output <- list()


  output[["limits"]] <- c(floor(min(values) / break.space) * break.space,
                          ceiling(max(values) / break.space) * break.space)

  output[["breaks"]] <- seq(output[["limits"]][1],
                            output[["limits"]][2],
                            break.space)

  # Return
  return(output)

}
