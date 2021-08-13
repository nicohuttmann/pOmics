#' Formats table for nice output (R Markdown)
#'
#' @param data data from t.test
#' @param order.by order rows by given column
#' @param descending arrange in descending order
#'
#' @return
#' @export
#'
#'
export_DT_t.test <- function(data, order.by = "p.adjust", descending = F) {

  if (!descending) {
    data <- data %>%
      dplyr::as_tibble() %>%
      dplyr::arrange(.data[[order.by]])
  } else {
    data <- data %>%
      dplyr::as_tibble() %>%
      dplyr::arrange(desc(.data[[order.by]]))
  }


  data <- data %>%
    dplyr::mutate(log2.fc = nice_number(log2.fc, digits = 2, big.mark = "")) %>%
    dplyr::mutate(p.value = nice_number(p.value, format = "e", round = F, sci.digits = 2, output = "character")) %>%
    dplyr::mutate(p.adjust = nice_number(p.adjust, format = "e", round = F, sci.digits = 2, output = "character")) %>%
    dplyr::rename(UniProt = variables,
                  `log2 FC` = log2.fc,
                  `p-value` = p.value,
                  `adj. p-value` = p.adjust) %>%
    dplyr::mutate(Gene = p2g(UniProt), .before = 1) %>%
    dplyr::mutate(Name = p2n(UniProt), .after = regulated) %>%
    dplyr::relocate(UniProt, .before = Name) %>%
    dplyr::select(-c(sig.log2.fc, sig.p.value, significant))

  if (all(data$`p-value` == data$`adj. p-value`))
    data <- dplyr::select(data, -`adj. p-value`)

  # Transform to DT datatable
  table <- DT::datatable(data,
                         escape = FALSE,
                         options = list(
                           columnDefs = list(list(className = 'dt-left',
                                                  targets = "_all"))),
                         rownames = FALSE)

  return(table)

}

