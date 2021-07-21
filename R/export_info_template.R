#' Exports observations names to csv info template file
#'
#' @param dataset dataset
#' @param path directory
#' @param observations.set observations.set
#'
#' @return
#' @export
#'
#'
export_info_template <- function(dataset, path, observations.set) {

  # Dataset
  dataset <- get_dataset(dataset)

  # Observations set
  observations.set <- get_observations_set(observations.set = observations.set,
                                           dataset = dataset)

  x <- data.frame(id = get_observations(dataset = dataset,
                                        observations.set = observations.set),
                  name = NA,
                  groups = NA)

  # Path
  if (!hasArg(path) || !dir.exists(paths = path)) {

    if (!is.null(attr(.datasets[[dataset]], "path")) &
        dir.exists(dirname(attr(.datasets[[dataset]], "path")))) {

      path <- dirname(attr(.datasets[[dataset]], "path"))

    } else {

      path <- "."

    }

  }

  # Write csv file
  write.table(x = x,
              file = paste0(path, "/", dataset, "_info.csv"),
              quote = F,
              sep = ",",
              na = "",
              row.names = F)
}
