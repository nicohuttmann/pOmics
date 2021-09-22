#' Calculates correct log2 fold-changes to replace anova diff values
#'
#' @param posthoc.diff extracted output from TukeyHSD
#' @param data data used for anova
#'
#' @return
#' @export
#'
#'
correct_log2fc_anova <- function(posthoc.diff, data) {


  for (i in 1:nrow(posthoc.diff)) {

    for (j in colnames(posthoc.diff)[-1]) {

      posthoc.diff[i, j] <- log2(mean(dplyr::pull(data, posthoc.diff$variables[i])[data$groups == strsplit_(x = substring(j, 8), split = "-")[1]]) /
                                 mean(dplyr::pull(data, posthoc.diff$variables[i])[data$groups == strsplit_(x = substring(j, 8), split = "-")[2]]))

    }

  }

  # Return corrected data frame
  return(posthoc.diff)

}
