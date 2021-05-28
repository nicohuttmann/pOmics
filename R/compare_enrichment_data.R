#' Compares enrichment data and returns list of common and unique terms
#'
#' @param enrichment.data.x enrichment data frame 1
#' @param enrichment.data.y enrichment data frame 2
#' @param ... arguments for simplify_enrichment_results
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
compare_enrichment_data <- function(enrichment.data.x, enrichment.data.y, ...) {


  go.x <- enrichment.data.x[["ID"]]
  go.y <- enrichment.data.y[["ID"]]


  enrichment.common <-  merge_enrichment_results(list(x = dplyr::filter(enrichment.data.x, ID %in% intersect(go.x, go.y)),
                                                      y = dplyr::filter(enrichment.data.y, ID %in% intersect(go.x, go.y)))) %>%
    simplify_enrichment_results(dataset = dataset, ...) %>%
    dplyr::select(-from)



  enrichment.unique.x <- enrichment.data.x %>%
    dplyr::filter(ID %in% setdiff(go.x, go.y)) %>%
    simplify_enrichment_results(dataset = dataset, by.semantic = T, minimum.count = 5) %>%
    dplyr::select(-from)



  enrichment.unique.y <- enrichment.data.y %>%
    dplyr::filter(ID %in% setdiff(go.y, go.x)) %>%
    simplify_enrichment_results(dataset = dataset, ...) %>%
    dplyr::select(-from)



  return <- list(x = dplyr::pull(enrichment.unique.x, var = Description, name = Count),
                 y = dplyr::pull(enrichment.unique.y, var = Description, name = Count),
                 xy = dplyr::pull(enrichment.common, var = Description, name = Count))


  # Return
  return(return)

}
