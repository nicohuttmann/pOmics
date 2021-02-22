#' Initiates databases for functional enrichment
#'
#' @param databases databases to setup
#'
#' @return
#' @export
#'
#'
setup_fun_enrich <- function(databases = "all") {

  require(topGO)

  #
  if (databases == "all") {
    databases <- c("CC", "BP", "MF")
  }

  # Message


  # GO CC
  if ("CC" %in% databases) {

    for (statistic in c("fisher", "ks")) {

      new_GOdata(ontology = "CC",
                 nodeSize = 10,
                 statistic = statistic,
                 save = TRUE)
    }
  }

  # GO BP
  if ("BP" %in% databases) {

    for (statistic in c("fisher", "ks")) {

      new_GOdata(ontology = "BP",
                 nodeSize = 10,
                 statistic = statistic,
                 save = TRUE)
    }
  }

  # GO MF
  if ("MF" %in% databases) {

    for (statistic in c("fisher", "ks")) {

      new_GOdata(ontology = "MF",
                 nodeSize = 10,
                 statistic = statistic,
                 save = TRUE)
    }
  }




  .info[["Functional enrichment databases"]] <<- databases


}
