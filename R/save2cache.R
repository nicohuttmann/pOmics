#' Saves data in cache
#'
#' @param data date
#' @param view Should data be viewed after saving
#' @param new Should cache list be renewed before saving
#'
#' @return
#' @export
#'
#'
save2cache <- function(data, view = F, new = F) {

  # Add cache list or replace
  new_cache(replace = new)

  # Add new data
  .cache[[length(.cache) + 1]] <<- data

  #View
  View(.cache[[length(.cache)]], title = paste0(".cache[[", length(.cache), "]]"))
  if (length(data) == 1) View(.cache[[length(.cache)]][[1]], title = paste0(".cache[[", length(.cache), "]][[1]]"))

}
