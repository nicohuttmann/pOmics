#' Queries data from Annotation packages
#'
#' @param keys vector of protein identifiers (see keytypes_org())
#' @param columns data columns to return(see columns_org())
#' @param output format of output ("vector.keep" (default), "vector.na,
#' "vector.rm", "mapping.na", "mapping.rm", "TERM2GENE")
#' @param keytype type of supplied keys (identified automatically if not
#' provided)
#' @param OrgDb string of Annotation package name to use
#' @param dataset dataset
#' @param ... arguments for TERM2GENE function (subtype = "CC", "BP", or "MF")
#'
#' @return
#' @export
#'
#'
select_org <- function(keys,
                       columns,
                       output = "vector.keep",
                       keytype,
                       OrgDb,
                       dataset,
                       ...) {

  if (!hasArg(keys)) {

    cat("No keys given.")
    return(invisible(NULL))

  }

  # Get dataset
  dataset <- get_dataset(dataset)

  # Get OrgDb
  if (!hasArg(OrgDb))
    OrgDb <- get_dataset_attr(which = "OrgDb", dataset = dataset)

  if (is.null(OrgDb)) {

    message("No annotation database setup. Use setup_annotations().")

    return(keys)

  }


  if (!require(OrgDb, character.only = T))
    stop(paste0("Annotations package ", OrgDb, " could not be installed."))


  # Test keytypes and keys
  if (!hasArg(keytype) ||
      length(keytype) > 1 ||
      !keytype %in% AnnotationDbi::keytypes(eval(parse(text = OrgDb)))) {


    keytype <- 1
    keytypes <- c("UNIPROT",
                  "ENTREZID",
                  "SYMBOL",
                  AnnotationDbi::keytypes(eval(parse(text = OrgDb))))

    while (is.numeric(keytype)) {

      if (any(keys %in% AnnotationDbi::keys(eval(parse(text = OrgDb)),
                                            keytype = keytypes[keytype])))
        keytype <- keytypes[keytype]

      else
        keytype <- keytype + 1


    }

    # Test keys
  } else {

    if (all(!keys %in%
            AnnotationDbi::keys(eval(parse(text = OrgDb)), keytype = keytype)))
  stop("No keys match the database. Change the keytype argument or remove it.")

  }



  # Test columns
  if (!hasArg(columns)) {

    columns <-
      select.list(choices = AnnotationDbi::columns(eval(parse(text = OrgDb))),
                  multiple = T,
                  title = paste0("Select columns from ", OrgDb, ": "))

  } else if (any(!columns %in%
                 AnnotationDbi::columns(eval(parse(text = OrgDb))))) {

    if (all(!columns %in%
            AnnotationDbi::columns(eval(parse(text = OrgDb))))) {

      stop("No column could be found in the database.")

    } else {

      columns <- intersect(columns,
                           AnnotationDbi::columns(eval(parse(text = OrgDb))))
      message(paste0("Columns used: ", paste(columns, collapse = ", ")))

    }


  }


  # Query annotation database
  output.data <- AnnotationDbi::select(x = eval(parse(text = OrgDb)),
                                  keys = keys,
                                  columns = columns,
                                  keytype = keytype) %>%
    suppressMessages()


  # Format output
  output.data <- select2output(output.data = output.data,
                               keys = keys,
                               output = output,
                               database = "org",
                               dataset = dataset,
                               ...)



  # Return
  return(output.data)

}
