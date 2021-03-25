#' Returns logical vector indication significant p-values
#'
#' @param p.values named vector of p-values
#' @param method method to determine significant p-values ("threshold", "bhc" (Benjamini-Hochberg-Correction))
#' @param limit limit for p.values and FDR for Benjamini-Hochberg-Correction
#' @param na.value return value for NA
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
significant <- function(p.values, method = "threshold", limit = 0.05, na.value = NA) {

  # Threshold
  if (method == "threshold") {

    # Apply threshold
    p.values <- p.values < limit

  # Benjamini-Hochberg method
  } else if (method == "bhc") {

    if (is.null(names(p.values))) names(p.values) <- seq_along(p.values)

    p.values <- bhc(p.values = p.values, FDR = limit) %>%
      dplyr::pull(var = "significant", name = "variables")

  }

  # Replace NAs with given entry
  p.values[is.na(p.values)] <- na.value

  # Return
  return(p.values)

}
