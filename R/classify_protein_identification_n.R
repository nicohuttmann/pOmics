#' Classifies proteins based on identification threshold
#'
#' @param data.eval data from eval_ function
#' @param lower.limit values above limit are considered positive/identified
#' @param plot Plot venn diagram?
#' @param save Save results list?
#'
#' @return
#' @export
#'
#'
classify_protein_identification_n <- function(data.eval, lower.limit = 0, plot = T) {

  # Check data
  if (!ncol(data.eval) > 3 || colnames(data.eval)[1] != "variables" || mode(data.eval[[2]]) != "numeric")
    stop("Provide correct input.")



  # Prepare results list
  results.list <- tibble::lst(raw.data = data.eval,
                              data = data.eval)

  # Get groups
  groups <- colnames(data.eval)[-1]



  # Unique proteins
  for (i in groups) {

    expression <- paste0("`", i, "`", ">", lower.limit, " & ", paste(paste0("`", setdiff(groups, i), "`", "<=", lower.limit),
                                                           collapse = " & "))

    l.name <- paste0("unique_", i, "_(",paste(setdiff(groups, i), collapse = "_"), ")")

    # Evaluate proteins
    results.list[["combinations"]][[l.name]] <- data.eval %>%
      filter(rlang::eval_tidy(rlang::parse_expr(expression))) %>%
      pull(var = "variables", name = NULL)

  }



  # Common proteins (1 < number < n)
  for (n in seq(2, length(groups) - 1)) {

    # Compute combinations
    grid <- as.matrix(expand.grid(rep(list(c(FALSE, TRUE)), length(groups))))
    grid <- grid[apply(grid, 1, sum) == n, ]

    #
    for (i in seq(nrow(grid))) {

      expression <- paste0(paste(paste0("`", groups[grid[i, ]], "`", ">", lower.limit),
                                 collapse = " & "),
                           " & ",
                           paste(paste0("`", groups[!grid[i, ]], "`", "<=", lower.limit),
                                 collapse = " & "))

      l.name <- paste0("common_",
                       paste(groups[grid[i, ]], collapse = "_"),
                       "_(",
                       paste(groups[!grid[i, ]], collapse = "_"),
                       ")")

      # Evaluate proteins
      results.list[["combinations"]][[l.name]] <- data.eval %>%
        filter(rlang::eval_tidy(rlang::parse_expr(expression))) %>%
        pull(var = "variables", name = NULL)

    }

  }



  # Common to all
  l.name <- paste0("common_", paste(groups, collapse = "_"))
  expression <- paste0(paste(paste0("`", groups, "`", ">", lower.limit),
                             collapse = " & "))

  # Evaluate proteins
  results.list[["combinations"]][[l.name]] <- data.eval %>%
    filter(rlang::eval_tidy(rlang::parse_expr(expression))) %>%
    pull(var = "variables", name = NULL)



  # Prepare identification dataframe
  # Evaluate peptides
  results.list[["data"]][-1] <- results.list[["data"]][-1] > lower.limit



  # Prepare plots
  results.list[["plot"]] <- plot_upset(data = results.list[["data"]], view = plot)



  # Return
  return(invisible(results.list))

}
