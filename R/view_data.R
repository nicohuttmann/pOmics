#' Shows all data lists
#'
#' @return
#' @export
#'
#'
view_data <- function() {

  if (".imports" %in% ls(all.names = TRUE, envir = .GlobalEnv)) View(.imports)

  if (".databases" %in% ls(all.names = TRUE, envir = .GlobalEnv)) View(.databases)

  if (".info" %in% ls(all.names = TRUE, envir = .GlobalEnv)) View(.info)

  if (".datasets" %in% ls(all.names = TRUE, envir = .GlobalEnv)) View(.datasets)

}
