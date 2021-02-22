#' Saves given or current working directory
#'
#' @param wd working directory
#' @param silent Should a message be
#'
#' @return
#' @export
#'
#'
save_wd <- function(wd, silent = F) {

  # Check info file
  new_info_list(replace = FALSE)

  # No working directory given
  if(!hasArg(wd)) {
    wd <- getwd()
  }

  # Save in .info
  if (dir.exists(wd)) {
    .info[["working_directory"]] <<- wd
    if (!silent) message("Working directory saved.")
  } else {
    if (!silent) message("Working directory is not valid.")
  }

}
