#' Classifies protein changes from two-way ANOVA with interaction
#'
#' @param data data frame output of two-way ANOVA with interactions
#' @param p.value.threshold threshold for p-values
#' @param fc.threshold absolute threshold for log2 fold-change
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
eval_2w_anova_i <- function(analysis_list, p.value.threshold = 0.05, fc.threshold = 0.5) {

  # given analysis list
  if (!"2w_anova_i_results" %in% names(analysis_list)) stop("Given list must contain '2w_anova_i_results' data frame.")

  data <- analysis_list[["2w_anova_i_results"]]

  # Define test structure
  var1 <- colnames(data)[5]
  var1 <- substring(var1, first = regexpr("_", var1) + 1)
  var12 <- strsplit(var1, "-")[[1]][2]

  var2 <- colnames(data)[6]
  var2 <- substring(var2, first = regexpr("_", var2) + 1)
  var21 <- strsplit(var2, "-")[[1]][1]
  var22 <- strsplit(var2, "-")[[1]][2]

  v <- tibble::lst(!!colnames(data)[2] := c(substr(var12, 1, regexpr(":", var12) - 1),
                                            substr(var1, 1, regexpr(":", var1) - 1)),
                   !!colnames(data)[3] := c(substring(var22, regexpr(":", var22) + 1),
                                            substring(var21, regexpr(":", var21) + 1)))
  rm(list = grep("var", objects(), value = T))


  # Setup results list
  list_classification <- tibble::lst(no = data$variables,
                                     !!names(v)[1] := c(),
                                     !!names(v)[2] := c(),
                                     !!colnames(data)[4] := c())

  groups <- names(list_classification)[-1]





  # Classify proteins
  for (i in 1:nrow(data)) {

    # anything significant
    if (sum(data[i, groups] < p.value.threshold) > 0) {

      # Factor 1
      if (dplyr::pull(data, groups[1])[i] < p.value.threshold &&
          dplyr::pull(data, paste0("tukey_", v[[1]][[2]], ":", v[[2]][[1]], "-", v[[1]][[1]], ":", v[[2]][[1]]))[i] <
          p.value.threshold &&
          abs(dplyr::pull(data, paste0("log2fc_", v[[1]][[2]], ":", v[[2]][[1]], "-", v[[1]][[1]], ":", v[[2]][[1]]))[i]) >
          fc.threshold) {

        list_classification[[2]] <- c(list_classification[[2]], data$variables[i])
        list_classification[["no"]] <- setdiff(list_classification[["no"]], data$variables[i])

      }

      # Factor 2
      if (dplyr::pull(data, groups[2])[i] < p.value.threshold &&
          dplyr::pull(data, paste0("tukey_", v[[1]][[1]], ":", v[[2]][[2]], "-", v[[1]][[1]], ":", v[[2]][[1]]))[i] <
          p.value.threshold &&
          abs(dplyr::pull(data, paste0("log2fc_", v[[1]][[1]], ":", v[[2]][[2]], "-", v[[1]][[1]], ":", v[[2]][[1]]))[i]) >
          fc.threshold) {

        list_classification[[3]] <- c(list_classification[[3]], data$variables[i])
        list_classification[["no"]] <- setdiff(list_classification[["no"]], data$variables[i])

      }

      # Combination
      if(dplyr::pull(data, groups[3])[i] < p.value.threshold) {

        # case 1: Protein only in combination deregulated
        if (#data$variables[i] %in% list_classification[["no"]] &&
          dplyr::pull(data, paste0("tukey_", v[[1]][[2]], ":", v[[2]][[2]], "-", v[[1]][[1]], ":", v[[2]][[1]]))[i] <
          p.value.threshold &&
          abs(dplyr::pull(data, paste0("log2fc_", v[[1]][[2]], ":", v[[2]][[2]], "-", v[[1]][[1]], ":", v[[2]][[1]]))[i]) >
          fc.threshold) {

          list_classification[[4]] <- c(list_classification[[4]], data$variables[i])
          list_classification[["no"]] <- setdiff(list_classification[["no"]], data$variables[i])

          # Case 2: Factor 1
        } else if (data$variables[i] %in% list_classification[[2]] &&
                   dplyr::pull(data, paste0("tukey_", v[[1]][[2]], ":", v[[2]][[2]], "-", v[[1]][[2]], ":", v[[2]][[1]]))[i] <
                   p.value.threshold &&
                   abs(dplyr::pull(data, paste0("log2fc_", v[[1]][[2]], ":", v[[2]][[2]], "-", v[[1]][[2]], ":", v[[2]][[1]]))[i]) >
                   fc.threshold) {

          list_classification[[4]] <- c(list_classification[[4]], data$variables[i])
          list_classification[["no"]] <- setdiff(list_classification[["no"]], data$variables[i])


          # Case 3: Factor 2
        } else if (data$variables[i] %in% list_classification[[3]] &&
                   dplyr::pull(data, paste0("tukey_", v[[1]][[2]], ":", v[[2]][[2]], "-", v[[1]][[1]], ":", v[[2]][[2]]))[i] <
                   p.value.threshold &&
                   abs(dplyr::pull(data, paste0("log2fc_", v[[1]][[2]], ":", v[[2]][[2]], "-", v[[1]][[1]], ":", v[[2]][[2]]))[i]) >
                   fc.threshold) {

          list_classification[[4]] <- c(list_classification[[4]], data$variables[i])
          list_classification[["no"]] <- setdiff(list_classification[["no"]], data$variables[i])

        }

      }

    }

  }

  analysis_list[["2w_anova_i_classes"]] <- list_classification


  return(analysis_list)

}
