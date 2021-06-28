#' Calculates composition of given annotations
#'
#' @param list_ list containing protein sets
#' @param TERM2GENE TERM2GENE data frame
#' @param correct.fractions.by devide fractions by total annotated proteins
#'
#' @return
#' @export
#'
#'
do_annotation_composition <- function(list_, TERM2GENE, correct.fractions.by, output = "data_annotation_composition") {



  samples <- list_

  data_ <- list()



  colnames(TERM2GENE) <- c("TERM", "GENE")

  data <- dplyr::tibble(TERM = unique(TERM2GENE$TERM))

  # Add sample data
  for (i in seq_along(samples)) {

    TERMS <- TERM2GENE %>%
      dplyr::filter(GENE %in% samples[[i]]) %>%
      dplyr::pull(TERM)


    count <- paste0("count.", names(samples)[i])
    percent <- paste0("percent.", names(samples)[i])

    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(!!count := sum(TERM == TERMS)) %>%
      dplyr::mutate(!!percent := .data[[count]] / length(samples[[i]]))


  }


  # Correct fractions
  if (hasArg(correct.fractions.by)) {

    for (i in which(grepl("percent", colnames(data)))) {

      data[[i]] <- data[[i]] / data[[i]][data$TERM == correct.fractions.by]

    }

  }


  data_[[output]] <- data %>%
    dplyr::arrange(desc(.data[[colnames(data)[2]]]))

  # Return
  return(invisible(data_))

}
