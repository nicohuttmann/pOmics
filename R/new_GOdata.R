#' Builds GO object from all variables in the study
#'
#' @param proteins (optional) prein vector
#' @param ontology ontology (CC, BP, MF)
#' @param statistic statistic
#' @param nodeSize minimum genes per annotation
#' @param dataset dataset
#' @param save Save?
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
new_GOdata <- function(proteins, ontology = "CC", statistic = "fisher", nodeSize = 10, dataset, save = T) {

  require(topGO)

  # Checks dataset
  dataset <- get_dataset(dataset)

  # Get all possible variables if no proteins supplied
  if (!hasArg(proteins)) {
    variables <- get_all_variables()
  } else {
    variables <- proteins
  }

  # For Fisher's exact test
  if (statistic == "fisher") {

    #
    if (!is.null(names(variables))) {
      genes <- variables
    } else {
      genes <- rep(0, length(variables))
      names(genes) <- variables
      genes[1:ceiling(length(genes) / 2)] <- 1
    }


    # Build annotations if not existing
    if (!check_database(id = "GO-ID", type = "Annotations")) new_annotations_data_UniProt(annotation = "GO-ID", dataset = dataset)

    # Get annonations
    annotations <- get_database(id = "GO-ID", type = "Annotations")



    #
    GOdata <- new("topGOdata",
                  ontology = ontology,
                  allGenes = as.factor(genes),
                  nodeSize = nodeSize,
                  annot = topGO::annFUN.gene2GO,
                  gene2GO = annotations)

    # For KS test
  } else if (statistic == "ks") {

    if (!is.null(names(variables))) {
      genes <- variables
    } else {
      genes <- rnorm(n = length(variables),
                     mean = 10,
                     sd = 1)
      names(genes) <- variables
    }



    # Build annotations if not existing
    if (!check_database(id = "GO-ID", type = "Annotations")) new_annotations_data_UniProt(annotation = "GO-ID", dataset = dataset)

    # Get annonations
    annotations <- get_database(id = "GO-ID", type = "Annotations")



    #
    GOdata <- new("topGOdata",
                  ontology = ontology,
                  allGenes = genes,
                  geneSel = function(x) {x > 10},
                  nodeSize = nodeSize,
                  annot = topGO::annFUN.gene2GO,
                  gene2GO = annotations)

    # Statistic not given
  } else stop("Statistic must be 'fisher' or 'ks'.")


  # save GOdata object
  if (save) {
    add_database(database = GOdata, id = paste(ontology, statistic, sep = "_"), type = "GOdata", replace = F)
    message("")
  }

  # Return
  invisible(GOdata)

}
