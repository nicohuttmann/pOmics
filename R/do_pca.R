#' Performs Principal Component Analysis using the prcomp function
#'
#' @param data_ data list
#' @param scale Scale data (Z-scores)?
#' @param data.name name of data to use for analysis
#' @param plot plot results
#' @param print.summary should summary be printed
#'
#' @return
#' @export
#'
#'
do_pca <- function(data_, scale = T, data.name = "raw_data", plot = T, print.summary = F) {

  # Check input
  if (!hasArg(data_)) stop("No data list given.")

  # Check input type
  if (!is.list(data_)) stop("Given data is not a list.")

  # Check data list
  if (!data.name %in% names(data_)) stop("Data could not be found. Please specify correct <data.name>.")


  # Get data
  data <- data_[[data.name]]

  # Scale data
  if (scale) data <- do_scale(data)

  # Save data
  data_[["data"]] <- data

  # Compute PCA
  data_[["pca_object"]] <- data %>%
    dplyr::select(where(is.numeric)) %>%
    prcomp()

  # PCA summary
  data_[["pca_summary"]] <- summary(data_[["pca_object"]])

  # Print summary
  if (print.summary) print(data_[["pca_summary"]])

  # PCA summary
  data_[["pca_data"]] <- as_tibble(data_[["pca_object"]][["x"]]) %>%
    dplyr::mutate(dplyr::select(data, -where(is.numeric)), .before = where(is.numeric))

  # Plot
  if (plot) data_ <- plot_gg_pca(data_)

  # Return
  return(invisible(data_))

}
