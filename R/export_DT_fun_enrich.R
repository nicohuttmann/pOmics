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
      dplyr::mutate(pvalue = nice_number(pvalue, format = "e", round = F, sci.digits = 2, output = "character")) %>%
      dplyr::mutate(p.adjust = nice_number(p.adjust, format = "e", round = F, sci.digits = 2, output = "character")) %>%
      dplyr::mutate(qvalue = nice_number(qvalue, format = "e", round = F, sci.digits = 2, output = "character")) %>%
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
