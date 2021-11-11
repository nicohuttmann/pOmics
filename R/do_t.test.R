#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param paired a logical indicating whether you want a paired t-test (see ?t.test)
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal (see ?t.test)
#' @param group.column column to use as group identification
#' @param pAdjustMethod see p.adjust.methods
#' @param input name of input
#' @param output name of output
#'
#' @return
#' @export
#'
#'
do_t.test <- function(data_,
                      paired = F,
                      var.equal = T,
                      group.column = "groups",
                      control.group,
                      pAdjustMethod = "BH",
                      p.value.cutoff = 0.05,
                      fc.threshold = 0,
                      input,
                      output = "data_t.test") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
  }


  group <- data[[group.column]]



  data_t.test <- dplyr::tibble(variables =
                                 colnames_class(data = data,
                                                data.class = "numeric"),
                               log2.fc = NA_real_,
                               sig.log2.fc = NA,
                               p.value = NA_real_,
                               sig.p.value = NA,
                               significant = NA,
                               regulated = "not")


  list_t.test <- list()

  for (i in data_t.test[["variables"]]) {

    # t-test model
    list_t.test[[i]] <- t.test(log2(data[[i]][group == levels(group)[2]]),
                               log2(data[[i]][group == levels(group)[1]]),
                               var.equal = var.equal,
                               paired = paired)

    # log2 fold-change
    data_t.test[match(i, data_t.test[["variables"]]), "log2.fc"] <-
      log2(mean(data[[i]][group == levels(group)[2]]) /
             mean(data[[i]][group == levels(group)[1]]))

    # p-value
    data_t.test[match(i, data_t.test[["variables"]]), "p.value"] <-
      list_t.test[[i]]$p.value

  }



# Determine significant effect size by confidence interval
if (is.character(fc.threshold) && grep(fc.threshold, "confidence|interval")) {

  for (i in data_t.test[["variables"]]) {
    data_t.test[match(i, data_t.test[["variables"]]), "sig.log2.fc"] <-
      sum(list_t.test[[i]]$conf.int > 0) != 1
  }

  # Numeric threshold
} else if (is.numeric(fc.threshold)) {

  # Absolute value given
  if (length(fc.threshold) == 1) {

    data_t.test[, "sig.log2.fc"] <- abs(data_t.test[, "log2.fc"]) > fc.threshold

  # Values for positive and negative threshold given
  } else {

    data_t.test[, "sig.log2.fc"] <-
      ifelse(data_t.test[, "log2.fc"] > 0,
             data_t.test[, "log2.fc"] > fc.threshold[2],
             data_t.test[, "log2.fc"] < fc.threshold[1])

  }

} else {
  stop('fc.threshold not supported.
       Either provide a numeric threshold or indicate "interval".')
}


  if (is.character(p.value.cutoff)) stop("Please provide a numeric p-value cutoff.")

# Adjust p-value
data_t.test <- data_t.test %>%
  # Adjust p-value
  dplyr::mutate(p.adjust = p.adjust(p.value, method = pAdjustMethod),
                .after = p.value) %>%
  # Significant p-values
  dplyr::mutate(sig.p.value = p.adjust < p.value.cutoff,
                .after = p.adjust) %>%
  # Final significance
  dplyr::mutate(significant = sig.log2.fc & sig.p.value) %>%
  #
  dplyr::rowwise() %>%
  dplyr::mutate(regulated = if (significant) ifelse(log2.fc > 0, "up", "down")
                else "not") %>%
  dplyr::ungroup()



  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data_t.test
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}
