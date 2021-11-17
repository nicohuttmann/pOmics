#' Shows all data lists
#'
#' @param which data ftrames to show
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


#' Prints variables data frame
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
view_variables <- function(dataset) {

  # get_dataset
  dataset <- get_dataset(dataset)

  # print
  print(.datasets[[dataset]][["variables"]])
  View(.datasets[[dataset]][["variables"]])


}


#' Prints observations data frame
#'
#' @param dataset dataset
#' @param observations.set observations set
#'
#' @return
#' @export
#'
#'
view_observations <- function(dataset, observations.set) {

  # Get dataset
  dataset <- get_dataset(dataset)

  # Get observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)

  # print
  print(.datasets[[dataset]][["observations"]][[observations.set]])
  View(.datasets[[dataset]][["observations"]][[observations.set]])

}


#' Prints default dataset attributes
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_all_attr <- function(dataset) {

  # Dataset
  dataset <- get_dataset(dataset)

  # Collectr all attributes
  dataset.attr <- attributes(.datasets[[dataset]])

  # Print
  cat(paste0("Default settings for dataset ", dataset, ":\n\n"))

  print(dataset.attr)

}


#' Prints default dataset attributes
#'
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
view_default_attr <- function(dataset) {

  # Dataset
  dataset <- get_dataset(dataset)

  # Collectr all attributes
  dataset.attr <- attributes(.datasets[[dataset]])

  # Remove non-default attributes
  dataset.attr <- dataset.attr[grepl(pattern = "default_", x = names(dataset.attr))]

  # Print
  cat(paste0("Default settings for dataset ", dataset, ":\n\n"))

  print(dataset.attr)

}
