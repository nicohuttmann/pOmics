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
do_t.test <- function(data_, paired = F, var.equal = T, group.column = "groups", control.group, pAdjustMethod = "BH",
                      input = "LFQ.intensity", output = "data_t.test") {

  # Check input
  if (!hasArg(data_)) {

    message("No data given.")

    invisible(NULL)

  }

  # Check if list or dataframe given
  list.input <- !is.data.frame(data_) & is.list(data_)

  # Check list input
  if (list.input & !input %in% names(data_)) {

    message("Data could not be found. Please specify correct <input>.")

    invisible(data_)

  }

  # Get data
  if (list.input) data <- data_[[input]]

  else data <- data_


  group <- data[[group.column]]


data_t.test <- dplyr::tibble(variables = colnames_typeof(data = data),
                            log2.fc = NA_real_,
                            p.value = NA_real_)


  for(i in data_t.test[["variables"]]) {
    data_t.test[match(i, data_t.test[["variables"]]), "log2.fc"] <- log2(mean(data[[i]][group == levels(group)[2]]) / mean(data[[i]][group == levels(group)[1]]))
    data_t.test[match(i, data_t.test[["variables"]]), "p.value"] <- t.test(log2(data[[i]][group == levels(group)[2]]),
                                                                           log2(data[[i]][group == levels(group)[1]]),
                                                                           var.equal = var.equal,
                                                                           paired = paired)$p.value
  }


data_t.test <- data_t.test %>%
  dplyr::mutate(p.adjust = p.adjust(p.value, method = pAdjustMethod))



  # Prepare return
  if (list.input) data_[[output]] <- data_t.test

  else data_ <- data

  # Return
  return(data_)

}
