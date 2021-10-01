#' Formats mapping from select methods
#'
#' @param output.data mapping data from select method
#' @param keys keys
#' @param output output type
#' @param database database of origin ("org" or "UniProt")
#' @param dataset dataset
#' @param ... arguments for TERM2GENE function (subtype = "CC", "BP", or "MF")
#'
#' @return
#' @export
#'
#'
select2output <- function(output.data, keys, output, database, dataset, ...) {

  # Process data
  if (output == "vector.keep") {

    if (ncol(output.data) > 2)
      message(paste0("Query resulted in more than one mapped column. Vector contains ", colnames(output.data)[2], "."))

    output.data <- output.data[!duplicated(output.data[, 1]), ]

    output.data[is.na(output.data[, 2]), 2] <- output.data[is.na(output.data[, 2]), 1]

    output.data <- pull_data(output.data)

    output.data <- output.data[keys]

  } else if (output == "vector.na") {

    if (ncol(output.data) > 2)
      message(paste0("Query resulted in more than one mapped column. Vector contains ", colnames(output.data)[2], "."))

    output.data <- output.data[!duplicated(output.data[, 1]), ]

    output.data <- pull_data(output.data)

    output.data <- output.data[keys]

  } else if (output == "vector.rm") {

    if (ncol(output.data) > 2)
      message(paste0("Query resulted in more than one mapped column. Vector contains ", colnames(output.data)[2], "."))

    output.data <- output.data[!is.na(output.data[, 2]), ]

    output.data <- output.data[!duplicated(output.data[, 1]), ]

    output.data <- pull_data(output.data)

    output.data <- output.data[intersect(keys, names(output.data))]

  } else if (output == "mapping.na") {

    output.data <- output.data


  } else if (output == "mapping.rm") {

    output.data <- output.data[!is.na(output.data[, 2]), ]


  } else if (output == "TERM2GENE" && database == "org") {

    output.data <- select_org_2_TERM2GENE(mapping = output.data,
                                          dataset = dataset,
                                          ...)


  } else if (output == "TERM2GENE" && database == "UniProt" ) {

    output.data <- select_UniProt_2_TERM2GENE(mapping = output.data)

  } else {

    message("Given output type not supported.")

    return(NULL)

  }

  return(output.data)

}
