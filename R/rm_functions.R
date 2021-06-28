#' Removes functions from global environment
#'
#' @return
#' @export
#'
#'
rm_functions <- function() {

  objects <- ls(pos = .GlobalEnv)

  to.rm <- c()
  for (i in objects) {
    to.rm <- c(to.rm, is.function(eval(sym(i))))
  }

  rm(list = objects[to.rm], pos = .GlobalEnv)

}
