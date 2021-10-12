#' Formats table for nice output (R Markdown)
#'
#' @param data data from t.test
#' @param order.by order rows by given column
#' @param descending arrange in descending order
#' @param buttons (optional) add buttons ("copy", "csv", "excel", "pdf",
#' "print")
#' @param dom order of table elements (default: "lBfrtip", see
#' https://rstudio.github.io/DT/)
#'
#' @return
#' @export
#'
#'
export_DT_t.test <- function(data, order.by = "p.adjust", descending = F,
                             buttons, dom = "lBfrtip") {

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
    dplyr::mutate(log2.fc = nice_number(log2.fc,
                                        digits = 2,
                                        big.mark = "")) %>%
    dplyr::mutate(p.value = nice_number(p.value,
                                        format = "e", round = F,
                                        sci.digits = 2,
                                        output = "character")) %>%
    dplyr::mutate(p.adjust = nice_number(p.adjust,
                                         format = "e",
                                         round = F,
                                         sci.digits = 2,
                                         output = "character")) %>%
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

  # Generate DT w/ or w/out buttons
  if (hasArg(buttons)) {
    buttons <- intersect(buttons, c("copy", "csv", "excel", "pdf", "print"))
  } else {
    buttons <- c()
  }

  if (length(buttons) > 0) {
    # Transform to DT datatable w/ buttons
    table <- DT::datatable(data,
                           escape = FALSE,
                           extensions = "Buttons",
                           options = list(
                             columnDefs = list(list(className = 'dt-left',
                                                    targets = "_all")),
                             dom = dom,
                             buttons = buttons,
                             lengthMenu = list(c(10,25,50,-1),
                                               c(10,25,50,"All"))),
                           rownames = FALSE)
  } else {
    # Transform to DT datatable w/out buttons
    table <- DT::datatable(data,
                           escape = FALSE,
                           options = list(
                             columnDefs = list(list(className = 'dt-left',
                                                    targets = "_all"))),
                           rownames = FALSE)
  }


  return(table)

}

