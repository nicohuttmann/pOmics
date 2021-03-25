#' Returns logical vector indicating values above or below given limit
#'
#' @param values numeric vector
#' @param min limit is a minimum/lower limit
#' @param limit cutoff value
#' @param na.value value value for NA
#'
#' @return
#' @export
#'
#'
threshold <- function(values, min = T, limit = 0, na.value = NA) {
  
  # Check input
  if (!is.logical(min)) stop("Min argument must be logical.")
  
  # Higher than limit
  if (min) {
    
    # Apply threshold
    values <- values > limit
    
  } else {
    
    # Apply threshold
    values <- values < limit
    
  }
  
  # Replace NAs with given entry
  values[is.na(values)] <- na.value
  
  # Return
  return(values)
  
}
