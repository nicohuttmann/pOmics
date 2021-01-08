#' Prepares imported data
#'
#' @param imports imported raw data
#' @param find.taxonomy should taxonomy be determined
#' @param load.UniProt.ws Should a UniProt database be downloaded
#'
#' @return
#' @export
#'
#'
prepare_datasets <- function(imports, find.taxonomy = T, load.UniProt.ws = T) {

  # Put single data frame in list for generalization
  if (class(imports)[1] != "list") stop("Please provide a named list.")

  # .datasets file
  new_datasets_list()

  # .info file
  new_info_list()

  #
  for (i in seq_along(imports)) {
    # Stupid BS
    message(paste0("Start preparing import ", i, "."))
    cat("Preparing the system...\n")
    for(j in 3:1) {cat(j, "\r"); Sys.sleep(.5)}
    # Make datasets
    new_dataset(imports[[i]], name = names(imports)[i], load.UniProt.ws = load.UniProt.ws)
  }


}
