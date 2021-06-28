#' Performs appropriate enrichment on given sets of proteins or values
#'
#' @param proteins list of proteins/scores/protein groups
#' @param background (optional) background genes
#' @param database database(s) to use
#' @param pvalueCutoff p-value cutoff for annotations
#' @param pAdjustMethod one of "none", "BH" (Benjamini-Hochberg correction), "hochberg", "bonferroni", "holm", "hommel", "BY", "fdr"#' @param qvalueCutoff q-value cutoff for annotations
#' @param minGSSize minimum number of proteins for annotation to be used for enrichment
#' @param maxGSSize maximum number of proteins for annotation to be used for enrichment
#' @param inverse scores: enriches for higher scores if TRUE
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param threshold p-value/confidence threshold to exclude terms
#' @param dataset dataset
#' @param view View results?
#'
#' @return
#' @export
#'
#'
fun_enrich <- function(proteins, background = NULL, database = "GO", pvalueCutoff = 0.05, pAdjustMethod = "none", minGSSize = 10,
                       maxGSSize = 500, inverse = F, algorithm = "classic", dataset, view = T) {



  # Get dataset
  dataset <- get_dataset(dataset)


  # Modify input
  if (length(database) == 1 && database == "All") database <- c("CC", "BP", "MF", "Kegg", "Reactome", "CORUM")
  database <- unlist(sapply(database, function(x) if(x == "GO") return(c("CC", "BP", "MF")) else return(x), USE.NAMES = FALSE))


  #


  # Check databases for functional enrichment
  #if (!"Functional enrichment databases" %in% names(.info)&& any(!databases %in% .info[["Functional enrichment databases"]]))
  #  stop("No database setup for functional enrichment. Use setup_fun_enrich() to prepare databases.")



  # Check input proteins
  if (is.null(background) && is.null(names(proteins))) stop("Background or interesting proteins must be defined.")





  # Case 1: No background given
  if (is.null(background) && any(names(proteins) %in% get_all_variables())) {

    allProteins <- proteins[!is.na(proteins)]

    # Case 2: Proteins w/out names and background given
  } else if (!is.null(background) && is.null(names(proteins))) {

    allProteins <- rep(FALSE, length(unique(c(proteins, background))))
    names(allProteins) <- unique(c(proteins, background))
    allProteins[proteins] <- TRUE

    # Case 3.1: Proteins w/ names and background given; ignore names(proteins)
  } else if (!is.null(background) && any(proteins %in% get_all_variables())) {

    allProteins <- rep(FALSE, na.omit(length(unique(c(proteins, background)))))
    names(allProteins) <- na.omit(unique(c(proteins, background)))
    allProteins[proteins] <- TRUE

    # Case 3.2: Proteins w/ names and background given; use names(proteins)
  } else if (!is.null(background) && any(names(proteins) %in% get_all_variables())) {

    allProteins <- rep(FALSE, length(unique(c(names(proteins), background))))
    names(allProteins) <- na.omit(unique(c(names(proteins), background)))
    allProteins[names(proteins)[!is.na(proteins)]] <- TRUE
    #
  } else {

    stop("Something went wrong.")

  }


  # Check databases
  #databases <- databases[databases %in% .info[["Functional enrichment databases"]]]

  # Prepare list
  list.enrichment <- tibble::lst()

  for (db in database) {
    # Single Fisher's exact test
    if (is.logical(allProteins)) list.enrichment[[db]] <- do_ORA(proteins = ifelse(allProteins, 1, 0),
                                                                 database = db,
                                                                 pvalueCutoff = pvalueCutoff,
                                                                 pAdjustMethod = pAdjustMethod,
                                                                 minGSSize = minGSSize,
                                                                 maxGSSize = maxGSSize,
                                                                 algorithm = algorithm,
                                                                 dataset = dataset)
    # Multiple fisher's exact test
    else if (is.character(allProteins)) list.enrichment[[db]] <- do_ORA_groups(proteins = allProteins,
                                                                               database = db,
                                                                               pvalueCutoff = pvalueCutoff,
                                                                               pAdjustMethod = pAdjustMethod,
                                                                               minGSSize = minGSSize,
                                                                               maxGSSize = maxGSSize,
                                                                               algorithm = algorithm,
                                                                               dataset = dataset)
    # Kolmogorov-Smirnov test
    else if (is.numeric(allProteins)) list.enrichment[[db]] <- do_GSEA(proteins = allProteins,
                                                                       database = db,
                                                                       inverse = inverse,
                                                                       pvalueCutoff = pvalueCutoff,
                                                                       pAdjustMethod = pAdjustMethod,
                                                                       minGSSize = minGSSize,
                                                                       maxGSSize = maxGSSize,
                                                                       algorithm = algorithm,
                                                                       dataset = dataset)

  }


  # View
  if (view) save2cache(data = list.enrichment, view = TRUE, new = FALSE)

  # Return
  invisible(list.enrichment)

}

