#' Performs appropriate enrichment on given sets of proteins or values
#'
#' @param proteins list of proteins/scores/protein groups
#' @param background (optional) background genes
#' @param databases databases to use
#' @param view View results?
#' @param return Return results?
#' @param save Save results?
#'
#' @return
#' @export
#'
#'
fun_enrich <- function(proteins, background = NULL, databases = "CC", view = T, return = F, save = F) {

  #
  require(topGO)

  # # Check databases for functional enrichment
  # if (any(!databases %in% .info[["Functional enrichment databases"]]))
  #   stop("No database setup for functional enrichment. Use setup_fun_enrich() to prepare databases.")



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
  databases <- databases[databases %in% .info[["Functional enrichment databases"]]]

  #
  list.enrichment <- tibble::lst()

  for (database in databases) {
    # Single Fisher's exact test
    if (is.logical(allProteins)) list.enrichment[[database]] <- do_enrichment_fisher(proteins = ifelse(allProteins, 1, 0),
                                                                                     database = database)
    # Multiple fisher's exact test
    if (is.character(allProteins)) list.enrichment[[database]] <- do_enrichment_mfisher(proteins = allProteins,
                                                                                        database = database)
    # Kolmogorov-Smirnov test
    if (is.numeric(allProteins)) list.enrichment[[database]] <- do_enrichment_ks(proteins = allProteins,
                                                                                 database = database)

  }


  # View
  if (view) save2cache(data = list.enrichment, view = TRUE, new = FALSE)

  # Return
  if (return) return(list.enrichment)

}
