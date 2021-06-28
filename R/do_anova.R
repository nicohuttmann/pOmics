#' Performs one-way ANOVA
#'
#' @param analysis_list list containing raw_data
#' @param formula formula given as string
#' @param data.name data to use for analysis
#'
#' @return
#' @export
#'
#'
do_anova <- function(analysis_list, formula = "log2(x) ~ groups", data.name = "raw_data") {

  # Check input
  if (!data.name %in% names(analysis_list)) stop("Data could not be found in list.")


  # Get data
  data <- analysis_list[[data.name]]


  # Get variables for model
  variables <- data %>%
    dplyr::select(where(is.numeric)) %>%
    colnames()


  # Formulate expression for model
  expr.left <- substr(formula, 1, regexpr("x", formula) - 1)
  expr.right <- substring(formula, regexpr("x", formula) + 1)





  # Set options for contrasts
  options(contrasts = c("contr.sum", "contr.poly"))

  # List to save models in
  model.list <- tibble::lst()

  # Apply model to proteins
  for (i in variables) {

    expr <- paste0(expr.left, i, expr.right)

    model.list[[i]] <- aov(formula = rlang::eval_tidy(rlang::parse_expr(expr)), data = data)

  }

  #
  results.list <- lapply(model.list, FUN = function(x) {
    x %>%
      summary %>%
      dplyr::first() %>%
      data2tibble(row.names = "groups") %>%
      dplyr::filter(!grepl("Residuals", groups)) %>%
      dplyr::pull(var = "Pr(>F)", name = "groups")})



  # Extract ANOVA p-values
  results.df <- results.list %>%
    list2DF() %>%
    t() %>%
    data2tibble(row.names = "variables")

  colnames(results.df)[-1] <- stringr::str_trim(names(results.list[[1]]))



  # Post-hoc test
  posthoc.list <- model.list %>%
    lapply(FUN = TukeyHSD)



  # Extract TukeyHSD p-values
  posthoc.p.value <- posthoc.list %>%
    lapply(FUN = function(x) {
      x %>%
        dplyr::last() %>%
        t() %>%
        matrix_rows(row.names = "p adj")}) %>%
    list2DF() %>%
    t() %>%
    data2tibble(row.names = "variables")

  colnames(posthoc.p.value)[-1] <- paste0("tukey_", stringr::str_trim(rownames(dplyr::last(posthoc.list[[1]]))))



  df <- dplyr::full_join(results.df, posthoc.p.value, by = "variables")


  # Extract difference
  posthoc.diff <- posthoc.list %>%
    lapply(FUN = function(x) {
      x %>%
        dplyr::last() %>%
        t() %>%
        matrix_rows(row.names = "diff")}) %>%
    list2DF() %>%
    t() %>%
    data2tibble(row.names = "variables")

  colnames(posthoc.diff)[-1] <- paste0("log2fc_", stringr::str_trim(rownames(dplyr::last(posthoc.list[[1]]))))

  # Correct log2fc
  posthoc.diff <- correct_log2fc_anova(posthoc.diff, data)


  df <- dplyr::full_join(df, posthoc.diff, by = "variables")


  analysis_list[["anova"]] <- df


  # Return
  return(invisible(analysis_list))



}
