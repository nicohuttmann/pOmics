#' Creates new info list
#'
#' @return
#' @export
#'
#'
new_info_list <- function() {

  if (!".info" %in% objects(all.names = T, envir = .GlobalEnv)) {
    .info <<- list("datasets" = c(),
                   "dataset_default" = c())
    .info[["datasets"]] <- c()
    .info[["dataset_default"]] <<- c()
  }

}
