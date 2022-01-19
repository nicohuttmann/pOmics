#' Subset term-to-gene data frame by given terms
#'
#' @param T2G term-to-gene data frame
#' @param terms character vector containing terms to keep
#'
#' @return
#' @export
#'
#'
t2g_subset_terms <- function(T2G, terms) {

  # Check T2G input
  if (!hasArg(T2G) || !is.data.frame(T2G)) {
    message("Please provide a term-to-gene data frame.")
    return(invisible(FALSE))
  }

  # Check terms input
  if (!hasArg(terms) || is.null(terms) || length(terms) == 0) {
    return(T2G)
  }

  # Subset T2G data
  T2G.subset <- dplyr::filter(T2G, TERM %in% terms)

  # Return
  return(T2G.subset)

}


#' Subset term-to-gene data frame by given genes
#'
#' @param T2G term-to-gene data frame
#' @param genes character vector contaiing genes to keep
#'
#' @return
#' @export
#'
#'
t2g_subset_genes <- function(T2G, genes) {

  # Check T2G input
  if (!hasArg(T2G) || !is.data.frame(T2G)) {
    message("Please provide a term-to-gene data frame.")
    return(invisible(FALSE))
  }

  # Check genes input
  if (!hasArg(genes) || is.null(genes) || length(genes) == 0) {
    return(T2G)
  }

  # Subset T2G data
  T2G.subset <- dplyr::filter(T2G, GENE %in% genes)

  # Return
  return(T2G.subset)

}


#' Get vector or list of genes mapped to given term/s
#'
#' @param T2G term-to-gene data frame
#' @param terms character vector of terms
#' @param output.type "vector" (default for one term) or "list" (default for
#' multiple terms)
#'
#' @return
#' @export
#'
#'
t2g_get_genes <- function(T2G, terms, output.type) {

  # Check arguments
  if (!hasArg(T2G)) {
    stop("term-to-gene data and terms must be given.")
  }

  # No terms given
  if (!hasArg(terms)) terms <- unique(T2G$TERM)

  # Filter terms
  T2G.subset <- t2g_subset_terms(T2G = T2G, terms = terms)

  # Determine output type if not given
  if (!hasArg(output.type) || !output.type %in% c("vector", "list")) {

    # Unknown output type given
    if (hasArg(output.type) && !output.type %in% c("vector", "list")) {
      message("Output type unknown. Automatically set.")
    }

    if (length(terms) == 1) output.type <- "vector"

    else output.type <- output.type <- "list"

    # Check output type regarding number of given terms
  } else if (output.type == "vector" && length(terms) > 1) {

    message("Use output.type = \"list\" for more than 1 term.")

  }

  # Output type vector
  if (output.type == "vector") {

    genes <- dplyr::pull(T2G.subset, GENE, TERM)

    # Output type list
  } else {

    genes <- split(T2G.subset$GENE, T2G.subset$TERM)

  }

  # Return
  return(genes)

}


#' Get vector or list of terms mapped to given gene/s
#'
#' @param T2G term-to-gene data frame
#' @param genes character vector of genes
#' @param output.type "vector" (default for one gene) or "list" (default for
#' multiple genes)
#'
#' @return
#' @export
#'
#'
t2g_get_terms <- function(T2G, genes, output.type) {

  # Check arguments
  if (!hasArg(T2G)) {
    stop("TERM2GENE data and genes must be given.")
  }

  # No genes given
  if (!hasArg(genes)) genes <- unique(T2G$GENE)

  # Filter genes
  T2G.subset <- t2g_subset_genes(T2G = T2G, genes = genes)

  # Determine output type if not given
  if (!hasArg(output.type) || !output.type %in% c("vector", "list")) {

    # Unknown output type given
    if (hasArg(output.type) && !output.type %in% c("vector", "list")) {
      message("Output type unknown. Automatically set.")
    }

    if (length(genes) == 1) output.type <- "vector"

    else output.type <- output.type <- "list"

    # Check output type regarding number of given genes
  } else if (output.type == "vector" && length(genes) > 1) {

    message("Use output.type = \"list\" for more than 1 gene.")

  }

  # Output type vector
  if (output.type == "vector") {

    terms <- dplyr::pull(T2G.subset, TERM, GENE)

    # Output type list
  } else {

    terms <- split(T2G.subset$TERM, T2G.subset$GENE)

  }

  # Return
  return(terms)

}


#' Get tibble of terms mapped to genes
#'
#' @param T2G term-to-gene data frame
#' @param terms character vector of terms
#' @param genes character vector of genes
#' @param output.type "tibble" (default) or "matrix"
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
t2g_get_matrix <- function(T2G,
                           terms = NULL,
                           genes = NULL,
                           output.type = "tibble") {

  # Check T2G input
  if (!hasArg(T2G)) {

    # No genes or terms given
    if (!hasArg(terms) || !hasArg(genes)) {

      stop("Terms and genes must be given if no term-to-gene data is given.")

    }

    # Make template data frame from genes and terms
    t2g.mat <- dplyr::tibble(variables = genes)

    for (i in terms) {
      t2g.mat <- dplyr::mutate(t2g.mat, !!i := FALSE)
    }




    # If term-to-gene data frame given
  } else {

    T2G.subset <- T2G %>%
      t2g_subset_genes(genes = genes)

    T2G.subset[is.na(T2G.subset$TERM), "TERM"] <- "NA"



    # genes as rows
    t2g.mat <- split(T2G.subset$GENE, T2G.subset$TERM) %>%
      list2tibble(identifier = "variables") %>%
      dplyr::select(c("variables", dplyr::any_of(terms))) %>%
      dplyr::arrange(match(variables, genes))

  }

  # Output type
  if (output.type == "matrix") {
    t2g.mat <- tibble2matrix(t2g.mat)
  }

  # Return
  return(t2g.mat)

}
