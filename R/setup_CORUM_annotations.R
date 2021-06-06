#' Downloads CORUM database for functional enrichment
#'
#' @return
#' @export
#'
#'
setup_CORUM_annotations <- function() {


  download.file(url = "https://mips.helmholtz-muenchen.de/corum/download/allComplexes.txt.zip",
                destfile = "allComplexes.txt.zip", quiet = T)

  import_files("allComplexes.txt.zip", silent = T)

  file.remove("allComplexes.txt.zip")




  # download.file(url = "https://mips.helmholtz-muenchen.de/corum/download/coreComplexes.txt.zip",
  #               destfile = "coreComplexes.txt.zip")
  #
  # import_files("coreComplexes.txt.zip")
  #
  # file.remove("coreComplexes.txt.zip")



  # Mapping
  import_files("https://mips.helmholtz-muenchen.de/corum/download/uniprot_corum_mapping.txt", silent = T)




  TERM2GENE <- dplyr::select(.imports[["uniprot_corum_mapping"]], c(2, 1))

  TERM2NAME <- dplyr::select(.imports[["allComplexes.txt"]], c(1, 2))


  # Save databases
  add_database(database = TERM2GENE, id = "CORUM", type = "TERM2GENE")

  add_database(database = TERM2NAME, id = "CORUM", type = "TERM2NAME")




  # Check
  if (check_database(id = "CORUM", type = "TERM2GENE") && check_database(id = "CORUM", type = "TERM2NAME")) {

    cat("CORUM annoations setup.")

    invisible(TRUE)

  } else {

    cat("CORUM annotations could not set up. Check your internet conection or contact Nico.")

    invisible(FALSE)

  }

}
