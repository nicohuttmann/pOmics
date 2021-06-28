#' Performs a two-way ANOVA with an interaction term between variables
#'
#' @param analysis_list list containing data
#' @param formula formula for model fitting (see aov() function)
#' @param data.name name of data in list
#' @param balanced Are analysis groups balanced (same number each)?
#'
#' @return
#' @export
#'
#' @importFrom magrittr %>%
#'
#'
do_2w_anova_i <- function(analysis_list, formula = "log2(x) ~ genotype * treatment", data.name = "raw_data", balanced = T) {



  # Check input
  if (!data.name %in% names(analysis_list)) stop("Data could not be found in list.")

  if (regexpr("\\*", formula) == -1) stop("No interaction defined. Interaction between nominal variables is defined by *.")


  # Get data
  data <- analysis_list[[data.name]]


  # Get variables for model
  variables <- data %>%
    dplyr::select(where(is.numeric)) %>%
    colnames()



  # Formulate expression for model
  expr.left <- substr(formula, 1, regexpr("x", formula) - 1)
  expr.right <- substring(formula, regexpr("x", formula) + 1)
  expr.right2 <- substring(expr.right, regexpr("~", expr.right)) %>%
    stringr::str_replace(pattern = "\\*", replacement = "\\+")








  # Test if data is balanced
  if (xtabs(rlang::eval_tidy(rlang::parse_expr(expr.right2)), data = data) %>% unique() %>% length() > 1) {

    # Check user input for balanced data design
    if (balanced) {

      stop("Data is not balanced. Please read:
      https://stat.ethz.ch/~meier/teaching/anova/factorial-treatment-structure.html#unbalanced-data")

    }

  }




  # Set options for contrasts
  options(contrasts = c("contr.sum", "contr.poly"))

  # List to save models in
  model.list <- tibble::lst()

  # Apply model to proteins
  for (i in variables) {

    expr <- paste0(expr.left, i, expr.right)

    model.list[[i]] <- aov(formula = rlang::eval_tidy(rlang::parse_expr(expr)), data = data)

  }



  # Summary or model correction
  if (balanced) {

    results.list <- lapply(model.list, FUN = function(x) {
      x %>%
        summary %>%
        dplyr::first() %>%
        data2tibble(row.names = "groups") %>%
        dplyr::filter(!grepl("Residuals", groups)) %>%
        dplyr::pull(var = "Pr(>F)", name = "groups")
    })

  } else {

    results.list <- lapply(model.list, FUN = function(x) {
      x %>%
        drop1(scope = ~., test = "F", data = data) %>%
        data2tibble(row.names = "term") %>%
        dplyr::filter(!grepl("none", term)) %>%
        dplyr::pull(var = "Pr(>F)", name = "term")
    })

  }


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



  analysis_list[["2w_anova_i_results"]] <- df


  # Return
  return(invisible(analysis_list))



}
