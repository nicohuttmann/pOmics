#' Performs appropriate enrichment on given sets of proteins or values
#'
#' @param proteins list of proteins/scores/protein groups
#' @param background (optional) background genes
#' @param inverse scores: enriches for higher scores if TRUE
#' @param databases databases to use
#' @param algorithm algorithm to use ("classic", "elim", "weight", "weight01")
#' @param threshold p-value/confidence threshold to exclude terms
#' @param add.info Add additional information (takes longer)
#' @param view View results?
#'
#' @return
#' @export
#'
#'
fun_enrich <- function(proteins, background = NULL, inverse = F, databases = "GO", algorithm = "weight01", threshold = 0.05, add.info = F, view = T) {

  # Modify input
  if (databases == "GO") databases <- c("CC", "BP", "MF")


  #
  suppressMessages(require(topGO))

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

  #
  list.enrichment <- tibble::lst()

  for (database in databases) {
    # Single Fisher's exact test
    if (is.logical(allProteins)) list.enrichment[[database]] <- do_ORA(proteins = ifelse(allProteins, 1, 0),
                                                                       database = database,
                                                                       algorithm = algorithm,
                                                                       threshold = threshold,
                                                                       add.info = add.info)
    # Multiple fisher's exact test
    if (is.character(allProteins)) list.enrichment[[database]] <- do_ORA_groups(proteins = allProteins,
                                                                                database = database,
                                                                                algorithm = algorithm,
                                                                                threshold = threshold,
                                                                                add.info = add.info)
    # Kolmogorov-Smirnov test
    if (is.numeric(allProteins)) list.enrichment[[database]] <- do_GSEA(proteins = allProteins,
                                                                        inverse = inverse,
                                                                        database = database,
                                                                        algorithm = algorithm,
                                                                        threshold = threshold,
                                                                        add.info = add.info)

  }


  # View
  if (view) save2cache(data = list.enrichment, view = TRUE, new = FALSE)

  # Return
  invisible(list.enrichment)

}
