#' Import files and stores in a list; changes wd if files come from new wd
#'
#' @param files files to be imported
#'
#' @return
#' @export
#'
#'
import_datasets <- function(files) {

  # select files
  if (!hasArg(files)) files <- choose.files(default = getwd())

  # import files
  list.import <- import_anything(files)

  # Change wd according to new files
  for (file in files) {

    # Check if new file is in a different dir than wd
    if (dirname(file) != getwd()) {

      # Change wd after user input
      if (menu(c("Yes", "No"), title = "Change working directory?") == 1) {
        setwd(dirname(file))
      }

    }

  }

  # Saves data in global environment
  if ("imports" %in% objects(pos = .GlobalEnv)) {
    x <- 1

    while (paste0("imports", x) %in% objects(pos = .GlobalEnv)) {
      x <- x + 1
    }

    assign(paste0("imports", x), list.import, pos = .GlobalEnv)

  } else {
    assign("imports", list.import, pos = .GlobalEnv)
  }

  # Continue with prepare function
  if (menu(choices = c("Yes", "No"),
           title = paste(ifelse(length(list.import) == 1, "One object", paste(length(list.import), "objects")),
                         "imported, continue with preparing data?")) == 1) {

    prepare_datasets(list.import)
  }


}
