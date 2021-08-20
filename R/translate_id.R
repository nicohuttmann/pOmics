#' Translates biological IDs using annotations packages
#'
#' @param Ids Ids
#' @param fromType type of given Ids
#' @param toType type Ids should be translated to
#' @param OrgDb name of organism database to use; defined by setup_annotations()
#' @param dataset dataset
#' @param drop drop missing translations
#' @param fill.missing How to handle missing Ids. By default, given Ids are filled in, other characters or NA can be used
#' @param save translated Ids in variables data frame for next time (all Ids in dataset will be translated)
#' @param silent suppress all messages
#'
#' @return
#' @export
#'
#'
translate_Ids <- function(Ids, fromType = "UNIPROT", toType, OrgDb, dataset, drop = F, fill.missing = "initial", save = T, silent = T) {

  if (!hasArg(Ids)) {

    cat("No Ids given.")

    return(invisible(NULL))

  }

  # Checks correct name of dataset
  dataset <- get_dataset(dataset, try.all = T)

  # Given toType and saved variables data column
  if (hasArg(toType) &&
      toType %in% get_variables_data_names(dataset) &&
      fromType %in% get_variables_data_names(dataset)) {

    return(pull_variables_data(variables = Ids,
                               which = toType,
                               names = fromType,
                               dataset = dataset))

  }

  # Annotation database for organism
  OrgDb <- get_OrgDb(OrgDb = OrgDb,
                     dataset = dataset)

  # If no organism database can be found
  if (is.null(OrgDb)) {

    return(invisible(NULL))

  }

  if (!hasArg(toType)) {

    toType <- select.list(AnnotationDbi::keytypes(eval(parse(text = OrgDb))))

  }

  # Check again if toType exists in variables data
  if (toType %in% get_variables_data_names(dataset) && fromType %in% get_variables_data_names(dataset)) {

    return(pull_variables_data(variables = Ids,
                               which = toType,
                               names = fromType,
                               dataset = dataset))

  }

  if (silent) {

    output <- clusterProfiler::bitr(geneID = Ids,
                                    fromType = fromType,
                                    toType = toType,
                                    OrgDb = OrgDb,
                                    drop = drop) %>%
      suppressMessages() %>%
      suppressWarnings()

  } else {

    output <- clusterProfiler::bitr(geneID = Ids,
                                    fromType = fromType,
                                    toType = toType,
                                    OrgDb = OrgDb,
                                    drop = drop)

  }


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
  if (fromType == "UNIPROT" && save) {

    add_variables_data(data = translate_Ids(Ids = get_variables(variables = All, dataset = dataset),
                                            fromType = fromType,
                                            toType = toType,
                                            OrgDb = OrgDb,
                                            dataset = dataset,
                                            save = FALSE,
                                            silent = TRUE),
                       name = toType,
                       dataset = dataset,
                       replace = FALSE)

  }

  # Return
  return(output)

}
