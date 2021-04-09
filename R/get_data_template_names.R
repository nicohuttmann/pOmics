#' Returns existing templates for raw data
#'
#' @return
#' @export
#'
#'
get_data_template_names <- function() {

  # .info list
  new_info_list()

  # Return names
  names(.info[["defaults"]])[-1]

}
