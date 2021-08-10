#' Prints vectors to console in code-form
#'
#' @param ... vector/s
#'
#' @return
#' @export
#'
#'
print_code_vector_string <- function(...) {

    cat(paste0('c("',
           paste(..., collapse = '",\n"'),
           '")'))

}
