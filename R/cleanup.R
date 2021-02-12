#' Removes all objects except specified analysis objects
#'
#' @return
#' @export
#'
#'
cleanup <- function() {

  #
  rm(list = setdiff(ls(pos = .GlobalEnv, all.names = TRUE),
                    c(".cache",
                      ".datasets",
                      ".databases",
                      ".imports",
                      ".info")),
     pos = .GlobalEnv)

}
