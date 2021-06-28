#' Evaluates data from ANOVA do_anova() for significant proteins
#'
#' @param data_ data list
#' @param anova.pvalueCutoff threshold for ANOVA p-values
#' @param anova.pAdjustMethod method to adjust ANOVA p-values
#' @param tukey.pvalueCutoff threshold for Tukey p-values
#' @param tukey.pAdjustMethod method to adjust Tukey p-values
#' @param fcThreshold absolute fold-change threshold
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
eval_anova <- function(data_, anova.pvalueCutoff = 0.05, anova.pAdjustMethod = "BH",
                       tukey.pvalueCutoff = 0.05, tukey.pAdjustMethod = "BH", fcThreshold = 0.5) {

  # given analysis list
  if (!"anova" %in% names(data_)) stop("Given list must contain 'anova' data frame.")

  data <- data_[["anova"]]

  # Define test structure
  results <- data %>%
    dplyr::select(c(1, 2))

  comparisons <- colnames(data)[-c(1, 2)] %>%
    substring(first = regexpr(pattern = "_", text = .) + 1) %>%
    unique()


  # ANOVA p-value
  results <- results %>%
    dplyr::mutate(groups = significant(p.values = groups, pvalueCutoff = anova.pvalueCutoff, pAdjustMethod = anova.pAdjustMethod))


  # Log2 fold-change and Tukey p-value evaluation
  for(i in comparisons) {

    # Significance vector
    sig <- rep("not", nrow(results))

    # Positive log2 fold-change
    sig[data[[paste0("log2fc_", i)]] > fcThreshold] <- "up"

    # Negative log2 fold-change
    sig[data[[paste0("log2fc_", i)]] < -fcThreshold] <- "down"

    # ANOVA p-value
    sig[!results[["groups"]]] <- "not"

    # Tukey p-value
    sig[results[["groups"]]] <- ifelse(test = significant(data[[paste0("tukey_", i)]][results[["groups"]]], pvalueCutoff = tukey.pvalueCutoff, pAdjustMethod = tukey.pAdjustMethod),
                                       yes = sig[results[["groups"]]],
                                       no = "not")

    # Add column
    results <- results %>%
      dplyr::mutate(!!i := sig)

  }



  # Add results to
  data_[["anova_eval"]] <- results

  # Return
  return(data_)

}
