#' Prints vectors to console in code-form
#'
#' @param ... vector/s
#'
#' @return
#' @export
#'
#'
cat_vector_string <- function(...) {

    cat(paste0('c("',
           paste(..., collapse = '",\n\t"'),
           '")'))

}
