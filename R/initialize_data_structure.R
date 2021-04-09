#' Creates basic data structure
#'
#' @param replace replace existing lists
#'
#' @return
#' @export
#'
#'
initialize_data_structure <- function(replace = F) {

  new_info_list(replace = replace)

  new_datasets_list(replace = replace)

  new_imports_list(replace = replace)

  new_cache(replace = replace)

}
