#' Returns default list for identifier and data types to import
#'
#' @param data.type datatype
#'
#' @return
#' @export
#'
#'
get_MaxQuant_defaults <- function(data.type) {

  defaults <- list(
    # proteinGroups
    proteinGroups = list(identifier = c("Protein.IDs"),
                         data.columns = c("Gene.names",
                                          "Protein.names",
                                          "Peptides",
                                          "LFQ.intensity",
                                          "id",
                                          "Only.identified.by.site",
                                          "Potential.contaminant",
                                          "Reverse")),
    # peptides
    peptides = list(identifier = c("Leading.razor.protein",
                                   "Start.position",
                                   "End.position"),
                    data.columns = c("LFQ.intensity",
                                     "Experiment",
                                     "Protein.group.IDs",
                                     "id",
                                     "Potential.contaminant",
                                     "Reverse")),
    # modificationSpecificPeptides
    modificationSpecificPeptides = list(identifier = c("Proteins",
                                                       "id"),
                                        data.columns = c("Proteins",
                                                         "Protein.group.IDs",
                                                         "Peptide.ID",
                                                         "Experiment",
                                                         "id",
                                                         "Potential.contaminant",
                                                         "Reverse")),
    # Sites
    Sites = list(identifier = c(c("Proteins",
                                  "Amino.acid",
                                  "Positions.within.proteins")),
                 data.columns = c("Proteins",
                                  "Localization.prob",
                                  "Amino.acid",
                                  "Positions.within.proteins",
                                  "id",
                                  "Potential.contaminant",
                                  "Reverse"))
    # proteinGroups = list(identifier = c(),
    #                      data.columns = c()),
    # proteinGroups = list(identifier = c(),
    #                      data.columns = c()),
  )


  # Match name
  if (data.type %in% names(defaults)) {

    return(defaults[[data.type]])

  } else {

    for (i in names(defaults)) {

      if (regexpr(pattern = i, text = data.type) != -1) {

        return(defaults[[i]])

      }

    }

  }

  return(list(identifier = NULL, data.columns = NULL))

}
