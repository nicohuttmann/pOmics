#' Checks organism database name from Bioconductor and if package can be loaded
#'
#' @param OrgDb name of organism database
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_OrgDb <- function(OrgDb, dataset) {

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  #
  if (!hasArg(OrgDb)) {

    OrgDb <- get_dataset_attr(which = "OrgDb",
                              dataset = dataset)

  }


  # If no OrgDb setup
  if (is.null(OrgDb)) {

    stop("No organism database set up. Please use setup_annotations() to enable ID translation and functional enrichment.")

  # Not installed
  } else if (requireNamespace(package = OrgDb, quietly = TRUE)) {

    # Attach package for clusterProfiler
    suppressPackageStartupMessages(library("org.Mm.eg.db", quietly = TRUE, character.only = TRUE))

    return(OrgDb)

  } else {

    stop(paste0("Annotations database ", OrgDb, " could not be loaded. make sure you have an internet connection or use setup_annotations."))

  }

}
