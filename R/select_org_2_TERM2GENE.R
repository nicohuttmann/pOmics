#' Exports TERM2GENE dataframe from select_org
#'
#' @param mapping output from select_org
#' @param subtype subtype of mapping; e.g. Ontology
#' @param dataset dataset
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
select_org_2_TERM2GENE <- function(mapping, subtype, dataset) {

  if ("GO" %in% colnames(mapping)) {

    # Filter specific ontology
    if (hasArg(subtype)) {

      mapping <- mapping %>%
        dplyr::filter(ONTOLOGY == subtype)

    }

    # Remove columns
    TERM2GENE <- mapping %>%
      dplyr::select(c(GO, UNIPROT)) %>%
      dplyr::mutate(GO = select_org(keys = GO,
                                    columns = "TERM",
                                    output = "vector.na",
                                    keytype = "GOID",
                                    OrgDb = "GO.db",
                                    dataset = dataset))


  } else if ("GOALL" %in% colnames(mapping)) {

    # Filter specific ontology
    if (hasArg(subtype)) {

      mapping <- mapping %>%
        dplyr::filter(ONTOLOGYALL == subtype)

    }

    # Remove columns
    TERM2GENE <- mapping %>%
      dplyr::select(c(GOALL, UNIPROT)) %>%
      dplyr::mutate(GOALL = select_org(keys = GOALL,
                                       columns = "TERM",
                                       output = "vector.na",
                                       keytype = "GOID",
                                       OrgDb = "GO.db",
                                       dataset = dataset))


  } else if ("PATHNAME" %in% colnames(mapping)) {


    # Remove columns
    TERM2GENE <- mapping %>%
      dplyr::mutate(PATHNAME = substring(text = PATHNAME,
                                         first = regexpr(pattern = ": ", text = PATHNAME) + 2)) %>%
      dplyr::mutate(UNIPROT = select_org(keys = ENTREZID,
                                         columns = "UNIPROT",
                                         output = "vector.na",
                                         keytype = "ENTREZID")) %>%
      dplyr::select(c(PATHNAME, UNIPROT))

  }


  colnames(TERM2GENE) <- c("TERM", "GENE")


  #Remove redundant entries
  TERM2GENE <- TERM2GENE[!duplicated(paste0(TERM2GENE$TERM, TERM2GENE$GENE)), ]


  # Return
  return(TERM2GENE)

}
