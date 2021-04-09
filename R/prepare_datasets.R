#' Prepares imported data
#'
#' @param imports imported raw data
#' @param data.origin Identify origin software of data
#' @param species protein origin (T or F or UniProt taxonomy ID)
#' @param load.UniProt.ws Should a UniProt database be downloaded
#' @param identifier (optional) specific vector of column/s to use as identifier
#' @param data.types types of data to be extracted
#'
#' @return
#' @export
#'
#'
prepare_datasets <- function(imports, data.origin, species, load.UniProt.ws = F, identifier, data.types) {

  # Put single data frame in list for generalization
  if (!is.list(imports) || is.data.frame(imports)) stop("Please provide a named list.")

  # .datasets file
  new_datasets_list()

  # .info file
  new_info_list()

  # .cache list
  new_cache()

  #
  for (i in seq_along(imports)) {

    # Stupid BS
    message(paste0("Start preparing import ", i, "."))
    cat("Preparing the system...\n")
    for(j in 3:1) {cat(j, "\r"); Sys.sleep(.5)}

    # Important part
    # Make datasets
    new_dataset(import = imports[[i]],
                name = names(imports)[i],
                data.origin = data.origin,
                species = species,
                load.UniProt.ws = load.UniProt.ws,
                identifier = identifier,
                data.types = data.types)
    Sys.sleep(3)
  }


}
