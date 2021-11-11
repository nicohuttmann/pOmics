#' Improved select function for DisGeNet databases
#'
#' @param genes vector of genes
#'
#' @return
#' @export
#'
#'
select_DisGeNet <- function(genes) {



  # Prebuild data frame
  data <- data.frame(matrix(NA, ncol = 20, nrow = 0))
  colnames(data) <- c("diseaseid",
                      "el",
                      "year_initial",
                      "source",
                      "disease_class_name",
                      "gene_pli",
                      "disease_type",
                      "uniprotid",
                      "disease_semantic_type",
                      "score",
                      "gene_symbol",
                      "disease_class",
                      "geneid",
                      "ei",
                      "year_final",
                      "protein_class",
                      "protein_class_name",
                      "gene_dpi",
                      "disease_name",
                      "gene_dsi")



  # Check for number of given keys; UniProt does not like more than 100 keys at a time
  if (length(genes) <= 400) {
    # Normal query
    temp.data <-
      suppressWarnings(
      suppressMessages(
        tryCatch(disgenet2r::gene2disease(
          genes[1:200])@qresult[, ],
                 error = function(cond) {
                   NA
                   }
    )))

    # Add data if no error
    if (is.data.frame(temp.data))
      data <- rbind(data, temp.data)

    # For more than 100 keys
  } else {
    # Iterate through good.keys
    for (i in seq(1, length(genes), 400)) {
      # Query packs of data
      temp.data <-
        suppressWarnings(
        suppressMessages(
          tryCatch(disgenet2r::gene2disease(
            genes[i:ifelse(i + 399 > length(genes),
                           length(genes), i + 399)])@qresult[, ],
                   error = function(cond) {
                     NA
                     }
      )))


      # Add data if no error
      if (is.data.frame(temp.data))
        data <- rbind(data, temp.data)
    }
  }


  # Rename genes to protein accession
  #data[, 1] <- names(genes)[match(data[, 1], genes)]


  data.return <- data
  #data.return <- data.frame(matrix(NA, ncol = 2, nrow = length(genes)))
  #colnames(data.return) <- c("UNIPROTKB", "disease_name")
  #data.return[, 1] <- names(genes)

  #
  # for (i in 1:nrow(data.return)) {
  #   data.return[i, 2] <- paste(unique(data[data[, 1] == data.return[i, 1], 2]), collapse = "; ")
  #
  # }



  return(data.return)

}
