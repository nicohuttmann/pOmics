setup_CORUM_annotations <- function() {









  download.file(url = "https://mips.helmholtz-muenchen.de/corum/download/allComplexes.txt.zip",
                destfile = "allComplexes.txt.zip")

  import_files("allComplexes.txt.zip")

  file.remove("allComplexes.txt.zip")




  download.file(url = "https://mips.helmholtz-muenchen.de/corum/download/coreComplexes.txt.zip",
                destfile = "coreComplexes.txt.zip")

  import_files("coreComplexes.txt.zip")

  file.remove("coreComplexes.txt.zip")



  # Mapping
  import_files("https://mips.helmholtz-muenchen.de/corum/download/uniprot_corum_mapping.txt")




}
