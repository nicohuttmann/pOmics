#' INCOMOPLETE FUNCTION: Combines data frames with variables info
#'
#' @param input list of protein data frames
#' @param dataset dataset
#' @param buttons (optional) add buttons ("copy", "csv", "excel", "pdf",
#' "print")
#' @param dom order of table elements (default: "lBfrtip", see
#' https://rstudio.github.io/DT/)
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
total_proteins_summary <- function(input, dataset, buttons, dom = "lBfrtip") {

  dataset <- get_dataset(dataset)


  output <- .datasets[[dataset]][["variables"]]


  for (i in seq_along(input)) {

    if (tibble::is_tibble(input[[i]]) &&
        "observations" %in% names(input[[i]])) {
      dummy <- input[[i]] %>%
        include_observations_data("labels") %>%
        transpose_tibble(from.row.names = "labels")
    } else if (is.list(input[[i]]) && grepl("t.test", names(input)[i])) {

      dummy <- input[[i]][["data_t.test"]] %>%
        dplyr::select(variables,
                      log2.fc,
                      p.adjust,
                      regulated)


      } else {
      dummy <- input[[i]]
    }


    names(dummy)[-1] <- paste0(names(input)[i], "_", names(dummy)[-1])

    output <- dplyr::left_join(output, dummy, by = "variables")

  }


  # Generate DT w/ or w/out buttons
  if (hasArg(buttons)) {
    buttons <- intersect(buttons, c("copy", "csv", "excel", "pdf", "print"))
  } else {
    buttons <- c()
  }

  if (length(buttons) > 0) {
    output <- DT::datatable(output,
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
    output <- DT::datatable(output,
                            escape = FALSE,
                            options = list(
                              columnDefs = list(list(className = 'dt-left',
                                                     targets = "_all"))),
                            rownames = FALSE)
  }


  return(output)

}
