#' Retrieves proteins from annotation terms
#'
#' @param proteins subset of proteins to choose from
#' @param terms annotation terms
#' @param TERM2GENE TERM2GENE dataframe
#' @param graphics choose terms from graphical window or console
#'
#' @return
#' @export
#'
#'
get_proteins_TERM2GENE <- function(proteins, terms, TERM2GENE, graphics) {

  if (!hasArg(TERM2GENE)) {

    message("Please provide a TERM2GENE dataframe.")

    return(NULL)

  }

  if (!hasArg(terms)) terms <- choose_terms(proteins = proteins,
                                            TERM2GENE = TERM2GENE,
                                            graphics = graphics)

  if (hasArg(proteins)) {

    TERM2GENE <- TERM2GENE %>%
      dplyr::filter(GENE %in% proteins)

  }

  TERM2GENE.list <- TERM2GENE_2_list(TERM2GENE)


  if (all(terms %in% names(TERM2GENE.list))) {

    return(unique(unlist(TERM2GENE.list[terms])))

  } else if (all(!terms %in% names(TERM2GENE.list))) {

    message("Terms not found in TERM2GENE dataframe. \nMaybe try a different database or select from list by leaving the <terms> argument empty.")
    return(NULL)

  } else {

    terms <- terms[terms %in% names(TERM2GENE.list)]

    message(paste0("Only returning proteins from following terms: ", paste(terms, collapse = ", "), "."))

    return(unique(unlist(TERM2GENE.list[terms])))

  }



}
