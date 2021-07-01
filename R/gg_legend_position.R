#' Removes or repositions legend of ggplot
#'
#' @param p ggplot object
#' @param remove remove legend
#' @param legend.position positoin of legend if remove = FALSE
#'
#' @return
#' @export
#'
#'
gg_legend.position <- function(p, remove = T, legend.position = "right") {

  if (remove) {

    return(p + theme(legend.position = "none"))

  } else {

    return(p + theme(legend.position = legend.position))

  }

}
