#' Prepares data for analysis
#'
#' @param data data
#' @param min.0 threshold to keep proteins
#' @param normalize.method method for normalization
#' @param shift shift in standard deviations
#' @param width width in standard deviations
#' @param seed seed for imputation
#'
#' @return
#' @export
#'
#'
prep_data <- function(data, min.0 = 0.5, normalize.method = "pqn", shift = 1.8, width = 0.3, seed = 123) {
  
  # 
  return(
    data %>% 
      threshold_0(min = min.0) %>% 
      pOmics::normalize(method = "pqn") %>% 
      impute_norm(shift = shift,
                  width = width,
                  seed = seed)
  )
  
}