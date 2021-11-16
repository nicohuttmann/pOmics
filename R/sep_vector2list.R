#' # Generates a list from separated vector
#'
#' @param x vector
#' @param sep separator
#'
#' @return
#' @export
#'
#'
sep_vector2list <- function(x, sep) {

  x <- lapply(x,
              FUN = function(x1) {
                if (is.atomic(x1) && is.character(x1) && any(grepl(sep, x1))) {
                  l <- strsplit(x1, split = sep)

                  tryCatch(
                    lapply(l, as.numeric),
                    warning = function(cond) return(l)
                  )

                  } else {
                    x1
                  }
                }
              )

  # Return modified list
  return(x)

}
