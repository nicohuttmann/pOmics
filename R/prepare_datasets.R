#' Prepares imported data
#'
#' @param imports imported raw data
#' @param data.origin Identify origin software of data
#' @param species should taxonomy be determined
#' @param load.UniProt.ws Should a UniProt database be downloaded
#' @param min.similarity minimum similarity oi column names
#' @param min.groupsize minimum number of samples
#'
#' @return
#' @export
#'
#'
prepare_datasets <- function(imports, data.origin, species, load.UniProt.ws = F, min.similarity = 8, min.groupsize = 6) {

  # Put single data frame in list for generalization
  if (class(imports)[1] != "list") stop("Please provide a named list.")

  # .datasets file
  new_datasets_list()

  # .info file
  new_info_list()

  # .cache list
  new_cache_list()

  #
  for (i in seq_along(imports)) {
    # Stupid BS
    message(paste0("Start preparing import ", i, "."))
    cat("Preparing the system...\n")
    for(j in 3:1) {cat(j, "\r"); Sys.sleep(.5)}
    # Make datasets
    new_dataset(x = imports[[i]],
                name = names(imports)[i],
                data.origin = data.origin,
                species = species,
                load.UniProt.ws = load.UniProt.ws,
                return = FALSE,
                min.similarity = min.similarity,
                min.groupsize = min.groupsize)
    Sys.sleep(3)
  }


}
