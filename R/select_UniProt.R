#' Improved select function for UniProt databases
#'
#' @param keys keys
#' @param columns data to query
#' @param output format of output ("vector.keep" (default), "vector.na,
#' "vector.rm", "mapping.na", "mapping.rm", "TERM2GENE")
#' @param keytype type of keys
#' @param x UniProt data base
#' @param modify (recommended) Should output values be modified to to singular
#' entries?
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
select_UniProt <- function(keys,
                           columns,
                           output = "vector.keep",
                           keytype = "UNIPROTKB",
                           x,
                           modify = T,
                           dataset) {

  # Get dataset
  dataset <- get_dataset(dataset)


  if (!hasArg(x)) {
    x <- get_database(
      id = get_dataset_attr(which = "taxId",
                            dataset = dataset),
      type = "UniProt")
  }

  # Prebuild data frame
  data <- data.frame(matrix(NA, ncol = length(columns) + 1,
                            nrow = length(keys)))
  colnames(data) <- c(keytype, columns)
  data[, keytype] <- keys

  #
  good.keys <- keys[keys %in% attr(x, "taxIdUniprots")]

  #
  if (length(good.keys) == 0) return(data)

  # Check for number of given keys;
  # UniProt does not like more than 100 keys at a time
  if (length(good.keys) <= 100) {
    # Normal query
    temp.data <- suppressMessages(
      tryCatch(UniProt.ws::select(x = x,
                                  keys = good.keys,
                                  columns = columns,
                                  keytype = keytype),
               error = function(cond) {
                 cat(paste0("ERROR: ", paste(columns, collapse = ";"),
                            " could not be retrieved.\n"))
                 NA
               }
      ))

    # Add data if no error
    if (is.data.frame(temp.data))
      data[match(temp.data[, keytype],
                 data[, keytype]),
           colnames(temp.data)] <- temp.data

    # For more than 100 keys
  } else {
    # Iterate through good.keys
    for (i in seq(1, length(good.keys), 100)) {
      # Query packs of data
      temp.data <- suppressMessages(
        tryCatch(UniProt.ws::select(
          x = x,
          # Checks that range does not exceed length of vector
          keys = good.keys[i:ifelse(i + 99 > length(good.keys),
                                    length(good.keys), i + 99)],
          columns = columns,
          keytype = keytype),
          error = function(cond) {cat(paste0("ERROR: ",
                                             paste(columns, collapse = ";"),
                                             " could not be retrieved.\n"))
            NA
          }
      ))


      # Add data if no error
      if (is.data.frame(temp.data))
        data[match(temp.data[, keytype],
                   data[, keytype]),
             colnames(temp.data)] <- temp.data
      else
        break
    }
  }

  # Modifies columns based on known schemes
  if(modify) {

    # GENES
    if ("GENES" %in% colnames(data)) {
      data[, "GENES"] <- unlist(lapply(strsplit(data[, "GENES"], split = " "),
                                       FUN = function(x) x[1]))
    }

    # PROTEIN-NAMES
    if ("PROTEIN-NAMES" %in% colnames(data)) {
      data[, "PROTEIN-NAMES"] <-
        unlist(lapply(strsplit(data[, "PROTEIN-NAMES"],
                               split = " \\("), FUN = function(x) x[1]))
    }

    # ENTREZ_GENE
    if ("ENTREZ_GENE" %in% colnames(data)) {
      data[, "ENTREZ_GENE"] <- gsub(" ", "", data[, "ENTREZ_GENE"])
    }

    # FUNCTION
    if ("FUNCTION" %in% colnames(data)) {
      data[, "FUNCTION"] <- substring(data[, "FUNCTION"], 11)
      data[, "FUNCTION"] <- sapply(
        data[, "FUNCTION"],
        FUN = function(x)
        {
          if (is.na(x)) return("")
          else if (grepl("ECO:", x)) return(substring(x,
                                                      1,
                                                      regexpr("ECO:", x) - 3))
          else return(x)
        })
    }

    # SUBCELLULAR-LOCATIONS
    if ("SUBCELLULAR-LOCATIONS" %in% colnames(data)) {
      data[, "SUBCELLULAR-LOCATIONS"] <- data[, "SUBCELLULAR-LOCATIONS"] %>%
        gsub(pattern = "SUBCELLULAR LOCATION[: ]", replacement = "", x = .) %>%
        gsub(pattern = " \\{[^\\}]*\\}[\\.;] ?", replacement = ";", x = .) %>%
        gsub(pattern = "\\[.*\\]: ?",
             replacement = "", x = .) %>%
        gsub(pattern = "[;,\\.]+ *", replacement = ";", .) %>%
        gsub(pattern = "Note=.*", replacement = "", .) %>%
        gsub(pattern = "[\\.,; ]+$", replacement = "", x = .) %>%
        gsub(pattern = "^ ", replacement = "", x = .)

    }

  }


  # Format output
  output.data <- select2output(output.data = data,
                               keys = keys,
                               output = output,
                               database = "UniProt",
                               dataset = dataset)


  return(output.data)
}
