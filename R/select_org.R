#' Queries data from Annotation packages
#'
#' @param keys vector of protein identifiers (see keytypes_org())
#' @param columns data columns to return(see columns_org())
#' @param output format of output ("vector.keep" (default), "vector.na, "vector.rm", "mapping.na", "mapping.rm", "TERM2GENE")
#' @param keytype Id type of supplied keys (identified automatically if not provided)
#' @param OrgDb string of Annotation package name to use
#' @param dataset dataset
#' @param ... arguments for TERM2GENE function (subtype = "CC", "BP", or "MF")
#'
#' @return
#' @export
#'
#'
select_org <- function(keys, columns, output = "vector.keep", keytype, OrgDb, dataset, ...) {

  if (!hasArg(keys)) {

    cat("No keys given.")
    invisible(NULL)

  }

  # Get dataset
  dataset <- get_dataset(dataset)

  # Get OrgDb
  if (!hasArg(OrgDb))
    OrgDb <- get_dataset_attr(which = "OrgDb", dataset = dataset)

  if (is.null(OrgDb))
    stop("No annotation database setup. Use setup_annotations().")

  if (!require(OrgDb, character.only = T))
    stop(paste0("Annotations package ", OrgDb, " could not be installed."))


  # Test keytypes and keys
  if (!hasArg(keytype) || length(keytype) > 1 || !keytype %in% AnnotationDbi::keytypes(eval(parse(text = OrgDb)))) {


    keytype <- 1
    keytypes <- c("UNIPROT", "ENTREZID", "SYMBOL", AnnotationDbi::keytypes(eval(parse(text = OrgDb))))

    while (is.numeric(keytype)) {

      if (any(keys %in% AnnotationDbi::keys(eval(parse(text = OrgDb)),
                                            keytype = keytypes[keytype])))
        keytype <- keytypes[keytype]

      else
        keytype <- keytype + 1


    }

    # Test keys
  } else {

    if (all(!keys %in% AnnotationDbi::keys(eval(parse(text = OrgDb)), keytype = keytype)))
      stop("No keys match the database. Change the keytype argument or remove it.")

  }



  # Test columns
  if (!hasArg(columns)) {

    columns <- select.list(choices = AnnotationDbi::columns(eval(parse(text = OrgDb))),
                multiple = T,
                title = paste0("Select columns from ", OrgDb, ": "))

  } else if (any(!columns %in% AnnotationDbi::columns(eval(parse(text = OrgDb))))) {

    if (all(!columns %in% AnnotationDbi::columns(eval(parse(text = OrgDb))))) {

      stop("No column could be found in the database.")

    } else {

      columns <- intersect(columns, AnnotationDbi::columns(eval(parse(text = OrgDb))))
      message(paste0("Columns used: ", paste(columns, collapse = ", ")))

    }


  }


  # Query annotation database
  output.data <- AnnotationDbi::select(x = eval(parse(text = OrgDb)),
                                  keys = keys,
                                  columns = columns,
                                  keytype = keytype) %>%
    suppressMessages()


  # Process data
  if (output == "vector.keep") {

    if (ncol(output.data) > 2)
      message(paste0("Query resulted in more than one mapped column. Vector contains ", colnames(output.data)[2], "."))

    output.data <- output.data[!duplicated(output.data[, 1]), ]

    output.data[is.na(output.data[, 2]), 2] <- output.data[is.na(output.data[, 2]), 1]

    output.data <- data2vector(output.data)

    output.data <- output.data[keys]

  } else if (output == "vector.na") {

    if (ncol(output.data) > 2)
      message(paste0("Query resulted in more than one mapped column. Vector contains ", colnames(output.data)[2], "."))

    output.data <- output.data[!duplicated(output.data[, 1]), ]

    output.data <- data2vector(output.data)

    output.data <- output.data[keys]

  } else if (output == "vector.rm") {

    if (ncol(output.data) > 2)
      message(paste0("Query resulted in more than one mapped column. Vector contains ", colnames(output.data)[2], "."))

    output.data <- output.data[!is.na(output.data[, 2]), ]

    output.data <- output.data[!duplicated(output.data[, 1]), ]

    output.data <- data2vector(output.data)

    output.data <- output.data[intersect(keys, names(output.data))]

  } else if (output == "mapping.na") {

    output.data <- output.data


  } else if (output == "mapping.rm") {

    output.data <- output.data[!is.na(output.data[, 2]), ]


  } else if (output == "TERM2GENE") {

    output.data <- select_org_2_TERM2GENE(mapping = output.data, dataset = dataset, ...)


  } else {

    stop("Given output type not supported.")

  }

  # Return
  return(output.data)

}
