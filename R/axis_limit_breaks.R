#' Helps make nice axis limit breaks
#'
#' @param plot_limits vector containing plot limits
#' @param break.space space between breaks
#'
#' @return
#' @export
#'
#'
axis_limit_breaks <- function(plot_limits, break.space = 1) {


  output <- list()


  output[["limits"]] <- c(floor(plot_limits[1] / break.space) * break.space,
                          ceiling(plot_limits[2] / break.space) * break.space)

  output[["breaks"]] <- seq(output[["limits"]][1],
                            output[["limits"]][2],
                            break.space)

  # Return
  return(output)

}
