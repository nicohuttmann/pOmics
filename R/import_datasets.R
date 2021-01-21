#' Import files and stores in a list; changes wd if files come from new wd
#'
#' @param files files to be imported
#' @param change.wd Should wd be changed by default
#' @param prepare Should imported files be forwarded to prepare_datasets
#' @param species should taxonomy be determined
#' @param data.origin Identify origin software of data
#' @param load.UniProt.ws Should a UniProt database be downloaded
#'
#' @return
#' @export
#'
#'
import_datasets <- function(files, change.wd = F, prepare, species, data.origin, load.UniProt.ws = F) {

  # select files
  if (!hasArg(files)) files <- choose.files(default = getwd())


  # Test
  if (length(files) == 0) stop("No data to be imported.")


  # import files
  list.import <- import_files(files, return = T)

  # Change wd according to new files
  for (file in files) {

    # Check if new file is in a different dir than wd
    if (dirname(file) != getwd() || change.wd) {

      # Change wd after user input
      if (menu(c("Yes", "No"), title = "Change working directory?") == 1) {
        setwd(dirname(file))
      }

    }

  }

  # Saves imported files
  add_import(list.import)

  # Continue with prepare function
  if (hasArg(prepare)) {

    if (prepare) {
      prepare_datasets(list.import)
    }

  } else if (menu(choices = c("Yes", "No"),
                 title = paste(ifelse(length(list.import) == 1, "One object", paste(length(list.import), "objects")),
                               "imported, continue with preparing data?")) == 1) {

    prepare_datasets(imports = list.import,
                     species = species,
                     data.origin = data.origin,
                     load.UniProt.ws = load.UniProt.ws)
  }

}
