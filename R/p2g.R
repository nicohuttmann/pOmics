#' Returns gene symbol from known proteins in dataset
#'
#' @param proteins UniProt protein Ids
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
p2g <- function(proteins, dataset) {



  if (!"SYMBOL" %in% get_variables_data_names(dataset = dataset)) {

    return(get_variables_data(name = "SYMBOL", variables = proteins, dataset = dataset))

  } else {



  }




  return(genes <- get_variables_data(name = "GENES", variables = proteins, dataset = dataset))





  genes <- clusterProfiler::bitr(geneID = proteins,
                                 fromType = "UNIPROT",
                                 toType = "GENENAME",
                                 OrgDb = "org.Mm.eg.db",
                                 drop = FALSE)

  genes <- genes[!duplicated(genes[[1]]), ]

  genes[is.na(genes[, 2]), 2] <- genes[is.na(genes[, 2]), 1]


}
