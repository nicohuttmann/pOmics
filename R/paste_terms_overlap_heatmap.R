#' Combines names and count of annotation terms
#'
#' @param terms named term vector
#' @param max maximum number of terms
#' @param include.count incude count in brackets
#'
#' @return
#' @export
#'
#'
paste_terms_overlap_heatmap <- function(terms, max = 5, include.count = F) {

  if (length(terms) == 0) return(NULL)

  if (length(terms) > max) terms <- terms[1:max]

  return <- ""

  if (include.count) {

    for (i in seq_along(terms)) {

      return <- paste0(return, terms[i], " (", names(terms)[i], ")\n")

    }

  } else {

    for (i in seq_along(terms)) {

      return <- paste0(return, terms[i], "\n")

    }

  }



  return(return)

}
