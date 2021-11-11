#' Checks default observations labels of no column given
#'
#' @param data data frame
#' @param labels column name of labels
#' @param dataset dataset
#'
#' @return
#' @export
#'
#'
get_labels_column <- function(data, labels, dataset) {


  if (!hasArg(labels)) {

    dataset <- get_dataset(dataset)

    default_labels <- get_dataset_attr(which = "default_observations_labels",
                                       dataset = dataset)

    if (!is.na(default_labels) && default_labels %in% colnames(data)) {

      labels <- default_labels

    } else if ("observations" %in% colnames(data)) {

      labels <- "observations"

    } else {

      labels <- colnames_class(data, data.class = "character")[1]

    }


  }

  return(labels)

}
