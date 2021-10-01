#' Removes all objects except specified analysis objects
#'
#' @return
#' @export
#'
#'
cleanup <- function() {

  #
  rm(list = setdiff(ls(pos = .GlobalEnv, all.names = FALSE),
                    c(".cache",
                      ".datasets",
                      ".databases",
                      ".imports",
                      ".info",
                      "Analysis",
                      "data_")),
     pos = .GlobalEnv)

}
