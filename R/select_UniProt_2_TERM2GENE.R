#' Exports TERM2GENE dataframe from select_org
#'
#' @param mapping output from select_UniProt
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
select_UniProt_2_TERM2GENE <- function(mapping) {

  if ("SUBCELLULAR-LOCATIONS" %in% colnames(mapping) ||
      "KEYWORDS" %in% colnames(mapping)) {

    mapping <- pull_data(mapping) %>%
      strsplit(";")

    terms <- unlist(mapping)

    mapping.length <- lapply(mapping, length)

    genes <- c()

    for (i in seq_along(mapping.length)) {

      genes <- c(genes, rep(names(mapping.length)[i],
                            times = mapping.length[[i]]))

    }

    TERM2GENE <- dplyr::tibble(TERM = terms,
                               GENE = genes)

  } else if ("GO" %in% colnames(mapping)) {

    mapping <- pull_data(mapping) %>%
      strsplit("; ")

    terms <- unlist(mapping)

    mapping.length <- lapply(mapping, length)

    genes <- c()

    for (i in seq_along(mapping.length)) {

      genes <- c(genes, rep(names(mapping.length)[i],
                            times = mapping.length[[i]]))

    }

    TERM2GENE <- dplyr::tibble(TERM = terms,
                               GENE = genes)

  } else if ("GO-ID" %in% colnames(mapping)) {

    mapping <- pull_data(mapping) %>%
      strsplit("; ")

    terms <- unlist(mapping)

    mapping.length <- lapply(mapping, length)

    genes <- c()

    for (i in seq_along(mapping.length)) {

      genes <- c(genes, rep(names(mapping.length)[i],
                            times = mapping.length[[i]]))

    }

    TERM2GENE <- dplyr::tibble(TERM = terms,
                               GENE = genes)

  } else {

    TERM2GENE <- mapping

  }


  #Remove redundant entries
  TERM2GENE <- TERM2GENE[!duplicated(paste0(TERM2GENE[[1]], TERM2GENE[[2]])), ]


  # Return
  return(TERM2GENE)

}
