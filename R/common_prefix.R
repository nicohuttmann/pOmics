#' Finds common substring; returns "" if no characters are in comomn
#'
#' @param names vector of strings
#'
#' @return
#' @export
#'
#'
common_prefix <- function(names) {

  # end of common part
  end <- 0

  while(length(unique(substr(names, start = 1, stop = end + 1))) == 1) {
    end <- end + 1
  }

  # Returns common part of the name
  substr(names[1], 1, end)
}
