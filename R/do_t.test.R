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
                      input, output = "data_t.test") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }


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
