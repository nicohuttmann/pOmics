#' Performs Benjamini-Hochberg-Correction of set of p-values
#'
#' @param p.values named vector of pvalues
#' @param FDR false discovery rate
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
bhc <- function(p.values, FDR = 0.05) {

  if (length(names(p.values)) == 0) stop("P-value vector has no names.")

  order <- names(p.values)

  #
  data <- tibble::tibble(variables = names(sort(x = p.values, na.last = T)), p.value = sort(x = p.values, na.last = T))

  data <- data %>%
    dplyr::mutate(`(i/m)Q` = c(FDR * (1:sum(!is.na(data[, "p.value"]))) / sum(!is.na(data[, "p.value"])),
                               rep(NA, sum(is.na(data[, "p.value"]))))) %>%
    dplyr::mutate(significant = FALSE) %>%
    dplyr::mutate(p.adj = c(p.value * sum(!is.na(data[, "p.value"])) / (1:nrow(data))))

  for(i in nrow(data):1) {

    if(!is.na(data[i, "(i/m)Q"]) && data[i, "(i/m)Q"] > data[i, "p.value"]) {
      data[1:i, "significant"] <- TRUE
      break()
    }

  }



  return(data[match(order, data$variables), ])
}
