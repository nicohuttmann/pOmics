#' Translates biological IDs using annotations packages
#'
#' @param Ids ids
#' @param fromType type of given Ids
#' @param toType type Ids should be translated to
#' @param OrgDb name of organism database to use; defined by setup_annotations()
#' @param dataset dataset
#' @param drop drop missing translations
#' @param fill_missing How to handle missing Ids. By default, given Ids are filled in, other characters or NA can be used
#' @param save translated Ids in variables data frame for next time (all Ids in dataset will be   )
#'
#' @return
#' @export
#'
#'
translate_Ids <- function(Ids, fromType = "UNIPROT", toType, OrgDb, dataset, drop = F, fill_missing = "initial", save = T) {

  if (!hasArg(Ids)) {

    cat("No Ids given.")

    invisible(NULL)

  }

  # Checks correct name of dataset
  dataset <- get_dataset(dataset)

  # Given toType and saved variables data column
  if (hasarg(toType) && toType %in% get_variables_data_names(dataset)) {

    return(get_variables_data(name = toType,
                              variables = Ids,
                              dataset = dataset))

  }

  # Annotation database for organism
  OrgDb <- get_OrgDb(OrgDb, dataset)

  # If no organism database can be found
  if (is.null(OrgDb)) {

    invisible(NULL)

  }

  if (!hasArg(toType)) {

    toType <- select.list(AnnotationDbi::keytypes(eval(parse(text = OrgDb))))

  }

  output <- clusterProfiler::bitr(geneID = proteins,
                                  fromType = fromType,
                                  toType = toType,
                                  OrgDb = OrgDb,
                                  drop = drop) %>%
    suppressMessages()


  output <- output[!duplicated(output[[1]]), ]
  rownames(output) <- c()

  if (fill.missing == "initial") {

    output[is.na(output[, 2]), 2] <- output[is.na(output[, 2]), 1]

  } else {

    output[is.na(output[, 2]), 2] <- fill.missing

  }

  # As vector
  output <- data2vector(output)

  # Save for next time

  add_variables_data(data = translate_id(Ids = get_variables(variables = All, dataset = dataset),
                                         fromType = fromType,
                                         toType = toType,
                                         OrgDb = OrgDb,
                                         dataset = dataset),
                     name = toType,
                     dataset = dataset)

  # Return
  return(output)

}
