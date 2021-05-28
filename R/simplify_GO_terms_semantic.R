#' Removes similar GO terms and keeps terms in order (assumes terms are ordered by p-value)
#'
#' @param terms vector of GO terms
#' @param similarity.measure method to measure pairwise GO terms similarity (see ?GOSemSim::mgoSim)
#' @param similarity.threshold threshold to remove terms (0-1, default = 0.4)
#' @param dataset dataset
#'
#' @return Named logical vector
#' @export
#'
#'
simplify_GO_terms_semantic <- function(terms, similarity.measure = "Wang", similarity.threshold = 0.4, dataset) {

  # Get dataset
  dataset <- get_dataset()

  # Get Organism database
  OrgDb <- get_OrgDb(dataset = dataset)

  # taxonomy Id
  taxId <- get_dataset_attr("taxId", dataset)

  # Get ontologies
  ontology <- sapply(terms, function(x) ifelse(length(clusterProfiler::go2ont(x)[, 2]) == 1,
                                               clusterProfiler::go2ont(x)[, 2],
                                               "not"))


  # vector to collect terms to keep
  terms.keep <- c()


  # Check and remove terms for ontologies separately
  for (ont in intersect(unique(ontology), c("CC", "BP", "MF"))) {

    # Check database
    if (!check_database(id = taxId, type = paste("GO", ont, "SemSim", sep = "_"))) {

      add_database(database = GOSemSim::godata(OrgDb = OrgDb, ont = ont, computeIC = T),
                   id = taxId, type = paste("GO", ont, "SemSim", sep = "_"))
    }

    GO <- get_database(id = taxId, type = paste("GO", ont, "SemSim", sep = "_"))

    terms.test <- terms[ont == ontology]


    #
    while (length(terms.test) > 0) {

      # First term of list
      term <- terms.test[1]

      # Keep first term of current list
      terms.keep <- c(terms.keep, term)

      # Remove term from testing list
      terms.test <- setdiff(terms.test, term)

      # Check if any terms left to test
      if (length(terms.test) > 0) {

        # Remove similar terms
        terms.test <- terms.test[c(GOSemSim::mgoSim(GO1 = term,
                                                    GO2 = terms.test,
                                                    semData = GO,
                                                    measure = similarity.measure,
                                                    combine=NULL)) < similarity.threshold]

      }

    }


  }


  return <- rep(FALSE, length(terms))

  names(return) <- terms

  return[terms.keep] <- TRUE

  return[ontology == "not"] <- TRUE

  # Return
  return(return)


}
