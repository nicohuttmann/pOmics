select_org <- function(keys, columns, output = "vector", keytype, dataset) {

  if (!hasArg(keys)) {

    cat("No keys given.")
    invisible(NULL)

  }

  # Get dataset
  get_dataset(dataset)

  # Get OrgDb
  OrgDb <- get_dataset_attr(which = "OrgDb", dataset = dataset)


  # Test keytypes and keys
  if (!hasArg(keytype) |
      length(keytype) > 1 |
      !keytype %in% AnnotationDbi::keytypes(eval(parse(text = OrgDb)))) {

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

    if (!all(keys %in% AnnotationDbi::keys(eval(parse(text = OrgDb)), keytype = keytype)))
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
  return <- AnnotationDbi::select(x = eval(parse(text = OrgDb)),
                                  keys = keys,
                                  columns = columns,
                                  keytype = keytype)


  # Process data
  if (output == "vector") {



  } else if (output == "vector.na") {



  } else if (output == "vector.rm") {



  } else if (output == "mapping") {



  } else if (output == "mapping.na") {



  } else if (output == "mapping.rm") {



  } else {

    stop ("Given output type not supported.")

  }











}
