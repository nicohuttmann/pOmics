#' Extracts result data frame Cluster enrichResult object or list of objects
#'
#' @param x enrichResult or list of objects
#'
#' @return
#' @export
#'
#'
enrichResult2data.frame  <- function(x) {

  # Input is one enrichResult object
  if (class(x) == "enrichResult") {

    # Return transformed object
    return(dplyr::as_tibble(x@result))

    # Recursively analyse list
  } else if (class(x) == "list") {

    for (i in seq_along(x)) {

      x[[i]] <- enrichResult2data.frame(x = x[[i]])

    }

  }

  # Return
  return(x)

  }


#' Merges a (nested) list of enrichResult objects
#'
#' @param enrichResult_list nested list of enrichment data frames
#'
#' @return
#' @export
#'
#'
merge_enrichResults <- function(enrichResult_list,
                               order.by = "p.adjust",
                               descending = F,
                               append.name = T) {


  # Keep iterating if not fully merged
  while (!is.data.frame(enrichResult_list)) {


    # Merge if
    if (is.data.frame(enrichResult_list[[1]])) {

      # Iterate over full list
        for (i in names(enrichResult_list)) {

          if ("Cluster" %in% names(enrichResult_list[[i]])) {

            if (append.name)
              enrichResult_list[[i]] <- dplyr::mutate(enrichResult_list[[i]],
                                                      Cluster = paste(Cluster, i, sep = "_"),
                                                      .before = 1)

            else
              enrichResult_list[[i]] <- dplyr::mutate(enrichResult_list[[i]],
                                                      Cluster = paste(i, Cluster, sep = "_"),
                                                      .before = 1)

          } else {

            enrichResult_list[[i]] <- dplyr::mutate(enrichResult_list[[i]], Cluster = i,
                                                    .before = 1)

          }

        }

      # Merge
      enrichResult_list <- purrr::map_df(enrichResult_list, ~.x)

      #
      if (!is.null(order.by)) {

        if (descending) {

          enrichResult_list <- enrichResult_list %>%
            dplyr::arrange(dplyr::desc(.data[[order.by]]))

        } else {

          enrichResult_list <- enrichResult_list %>%
            dplyr::arrange(.data[[order.by]])

        }

      }


      # Recursively merge lists
    } else {


      for (i in seq_along(enrichResult_list)) {

        enrichResult_list[[i]] <-
          merge_enrichResult(enrichResult_list[[i]],
                             order.by = order.by,
                             descending = descending,
                             append.name = append.name)

      }

    }

  }

  # Return
  return(enrichResult_list)

}

