#' Shows all data lists
#'
#' @return
#' @export
#'
#'
view_data <- function(which = c(".imports",
                                ".info",
                                ".databases",
                                ".datasets")) {

  if (".imports" %in% intersect(ls(all.names = TRUE, envir = .GlobalEnv),
                                which))
    View(.imports)

  if (".databases" %in% intersect(ls(all.names = TRUE, envir = .GlobalEnv),
                                  which))
    View(.databases)

  if (".info" %in% intersect(ls(all.names = TRUE, envir = .GlobalEnv),
                             which))
    View(.info)

  if (".datasets" %in% intersect(ls(all.names = TRUE, envir = .GlobalEnv),
                                 which))
    View(.datasets)

}

#' Shows datasets list
#'
#' @return
#' @export
#'
v_ds <- function() {
  view_data(which = ".datasets")
}

#' Shows databases list
#'
#' @return
#' @export
#'
v_db <- function() {
  view_data(which = ".databases")
}

#' Shows info list
#'
#' @return
#' @export
#'
v_info <- function() {
  view_data(which = ".info")
}

#' Shows imports list
#'
#' @return
#' @export
#'
v_imp <- function() {
  view_data(which = ".imports")
}
