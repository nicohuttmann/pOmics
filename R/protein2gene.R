#' Translates protein accession Ids to gene names
#'
#' @param proteins proteins
#' @param dataset which dataset to use
#'
#' @return
#' @export
#'
#'
protein2gene <- function(proteins, dataset = 1) {




  annotations <- select_UniProt(x = .databases[["UniProt"]][[.info[[dataset]][["taxId"]]]],
                                columns = "GENES",
                                keys = proteins,
                                keytype = "UNIPROTKB")

  return(annotations)
}
