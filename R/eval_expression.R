#' Evaluates expression analysis data containing p.value and log2.fc 
#'
#' @param data expression analysis data
#' @param significance.method method to evaluate p-values
#' @param p.value.limit cutoff or FDR for p-value
#' @param fold.change.limit absolute limit for the fold change
#'
#' @return
#' @export
#'
#'
eval_expression <- function(data, significance.method = "threshold", p.value.limit = 0.05, fold.change.abs.limit = 0) {
  
  # Transform matrix to tibble
  if (is.matrix(data)) data <- data2tibble(data = data)
  
  # Define statistically changed proteins
  data <- data %>% 
    mutate(expression = significant(
      p.value,
      method = significance.method,
      limit = p.value.limit,
      na.value = FALSE) %and% abs(log2.fc) > fold.change.abs.limit) %>% 
    mutate(expression = 
             {
               x <- expression
               x[!expression] <- "not"
               x[expression %and% (log2.fc > 0)] <- "up"
               x[expression %and% (log2.fc < 0)] <- "down"
               x
             })
  
  # Return
  return(data)
  
}
