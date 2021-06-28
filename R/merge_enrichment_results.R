#' Combines enrichment results data frames
#'
#' @param enrichment_list list where each entry is a enrichment results data frame
#' @param order.by column name by which to order
#' @param descending order in descending order
#' @param append.name
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
merge_enrichment_results <- function(enrichment_list, order.by = "p.adjust", descending = F, append.name = T) {


  for (i in names(enrichment_list)) {

    if ("from" %in% names(enrichment_list[[i]])) {

      if (append.name)
        enrichment_list[[i]] <- dplyr::mutate(enrichment_list[[i]], from = paste(from, i, sep = "_"), .before = 1)

      else
        enrichment_list[[i]] <- dplyr::mutate(enrichment_list[[i]], from = paste(i, from, sep = "_"), .before = 1)

    } else {

      enrichment_list[[i]] <- dplyr::mutate(enrichment_list[[i]], from = i, .before = 1)

    }

  }

  # Merge
  enrichment_df <- purrr::map_df(enrichment_list, ~.x)

  #
  if (!is.null(order.by)) {

    if (descending) {

      enrichment_df <- enrichment_df %>%
        dplyr::arrange(dplyr::desc(.data[[order.by]]))

    } else {

      enrichment_df <- enrichment_df %>%
        dplyr::arrange(.data[[order.by]])

    }


  }

  # Return
  return(enrichment_df)

}
