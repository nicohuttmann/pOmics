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


#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param order.by order rows by given column
#' @param descending arrange in descending order
#' @param buttons (optional) add buttons ("copy", "csv", "excel", "pdf",
#' "print")
#' @param dom order of table elements (default: "lBfrtip", see
#' https://rstudio.github.io/DT/)
#' @param add.annotations add annotations to proteins
#' @param dataset dataset
#' @param view view table
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
export_RMD_t.test <- function(data_,
                              order.by = "p.adjust",
                              descending = F,
                              buttons,
                              dom = "lBfrtip",
                              add.annotations = F,
                              dataset,
                              view = F,
                              input = "data_t.test",
                              output = "data_DT_t.test") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }


  if (add.annotations && "fun_enrich_t.test" %in% names(data_)) {

    data <- data %>%
      dplyr::mutate(Annotations ="")

    for (i in 1:2) {

      if (length(data_[["fun_enrich_t.test"]][[i]]) > 0) {

        for (j in 1:length(data_[["fun_enrich_t.test"]][[i]])) {

          if (!is.null(data_[["fun_enrich_t.test"]][[i]][[j]])) {

            dummy <- data_[["fun_enrich_t.test"]][[i]][[j]] %>%
              data.frame() %>%
              dplyr::pull(geneID, Description) %>%
              sapply(function(x) strsplit_(x, split = "/")) %>%
              {if (is.matrix(.)) as.list(as.data.frame(.))
                else topGO::inverseList(.)}

            data <- data %>%
              dplyr::rowwise() %>%
              dplyr::mutate(Annotations = paste(strsplit_(Annotations, "/"),
                                                dummy[[variables]],
                                                collapse = "/"))

          }

        }

      }

    }

  }

  #
  data <- export_DT_t.test(data = data,
                           order.by = order.by,
                           descending = descending,
                           buttons = buttons,
                           dom = dom)

  if (view) print(data)

  # Output name
  #if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}


#' Formats table for nice output (R Markdown)
#'
#' @param data data from fun_enrich (enrichResult)
#' @param buttons (optional) add buttons ("copy", "csv", "excel", "pdf",
#' "print")
#' @param dom order of table elements (default: "lBfrtip", see
#' https://rstudio.github.io/DT/)
#'
#' @return
#' @export
#'
#'
export_DT_fun_enrich <- function(data, buttons, dom = "lBfrtip") {

  # Test class (expected enrichResult object)
  if (class(data) == "enrichResult") data <- data.frame(data)

  else if (is.null(data)) data <- data.frame(ID = character(),
                                             Description = character(),
                                             GeneRatio = character(),
                                             BgRatio = character(),
                                             pvalue = double(),
                                             p.adjust = double(),
                                             qvalue = double(),
                                             geneID = character(),
                                             Count = double())


  # Test if any annotations are significant
  if (nrow(data) > 0) {

    # Prepare data frame
    data <- data %>%
      tibble::as_tibble() %>%
      dplyr::mutate(pvalue = nice_number(pvalue,
                                         format = "e",
                                         round = F,
                                         sci.digits = 2,
                                         output = "character")) %>%
      dplyr::mutate(p.adjust = nice_number(p.adjust,
                                           format = "e",
                                           round = F,
                                           sci.digits = 2,
                                           output = "character")) %>%
      dplyr::mutate(qvalue = nice_number(qvalue,
                                         format = "e",
                                         round = F,
                                         sci.digits = 2,
                                         output = "character")) %>%
      dplyr::relocate(Count, .after = qvalue) %>%
      dplyr::rename(UniProt = geneID) %>%
      dplyr::mutate(Genes = "", .before = UniProt)


    # Facilitate protein to gene translation
    protein2gene <- data$UniProt %>%
      strsplit_("/") %>%
      unique() %>%
      p2g()


    data <- data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Genes = paste(protein2gene[strsplit_(UniProt, "/")],
                                  collapse = "/"))

    # Dummy data frame
  } else {

    data <- data %>%
      tibble::as_tibble() %>%
      dplyr::relocate(Count, .after = qvalue) %>%
      dplyr::rename(UniProt = geneID) %>%
      dplyr::mutate(Genes = "", .before = UniProt)

  }

  # Generate DT w/ or w/out buttons
  if (hasArg(buttons)) {
    buttons <- intersect(buttons, c("copy", "csv", "excel", "pdf", "print"))
  } else {
    buttons <- c()
  }

  if (length(buttons) > 0) {
    # Transform to DT datatable
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
    # Transform to DT datatable
    table <- DT::datatable(data,
                           escape = FALSE,
                           options = list(
                             columnDefs = list(list(className = 'dt-left',
                                                    targets = "_all"))),
                           rownames = FALSE)
  }


  # Return
  return(table)

}


#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param buttons (optional) add buttons ("copy", "csv", "excel", "pdf",
#' "print")
#' @param dom order of table elements (default: "lBfrtip", see
#' https://rstudio.github.io/DT/)
#' @param view view table
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
export_RMD_fun_enrich <- function(data_,
                                  buttons,
                                  dom = "lBfrtip",
                                  view = F,
                                  input = "fun_enrich",
                                  output = "fun_enrich_DT") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }


  # Prepare all sub data frames
  for (i in seq_along(data)) {

    for (j in seq_along(data[[i]])) {

      data[[i]][[j]] <- export_DT_fun_enrich(data[[i]][[j]],
                                             buttons = buttons,
                                             dom = dom)

    }

  }


  if (view) print(data)

  # Output name
  #if (!hasArg(output)) output <- input

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
    attr(data_, "data") <- output
  }

  else data_ <- data

  # Return
  return(data_)

}


#' Prepares GO table for output in HTML file
#'
#' @param table GO results table
#' @param caption table caption
#'
#' @return
#' @export
#'
#'
export_GO_table <- function(table, caption = NULL) {

  # Check input
  if (is.list(table) && !is.data.frame(table) && length(table) == 1) table <-
      table[[1]]

  else if (is.list(table)&& !is.data.frame(table) && length(table) > 1) {
    message("Only first table in list used.")
    table <- table[[1]]
  } else if (!is.data.frame(table)) {
    stop("Either provide a list or a data frame.")
  }

  # Insert links
  table$GO.ID <- paste0('<a  target=_blank href=',
                        "https://www.ebi.ac.uk/QuickGO/term/",
                        table$GO.ID, '>', table$GO.ID,'</a>' )


  # Transform to DT datatable
  table <- DT::datatable(
    table,
    escape = FALSE,
    options = list(
      columnDefs = list(list(className = 'dt-left',
                             targets = "_all"))
    ),
    caption =
      htmltools::tags$caption(caption,
                              style="color:black; text-align:center"),
    rownames = FALSE)

  # Return
  return(table)

}
