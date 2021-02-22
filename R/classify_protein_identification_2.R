#' Classifies proteins based on identification threshold
#'
#' @param data.eval data from eval_ function
#' @param upper.neg upper limit for "not identified"
#' @param lower.pos lower limit for "confidently identified"
#' @param plot Plot venn diagram?
#' @param export Export ven diagram plot?
#' @param save Save results list?
#'
#' @return
#' @export
#'
#'
classify_protein_identification_2 <- function(data.eval, sig.variables, upper.neg = 0, lower.pos = 0.5, dataset, plot = T, export = F, name, save = F) {

  # Check data
  if (ncol(data.eval) != 3 || colnames(data.eval)[1] != "variables" || mode(data.eval[[2]]) != "numeric") stop("Provide correct input.")

  # Prepare results list
  results.list <- tibble::lst()

  #
  groups <- colnames(data.eval)[-1]


  results.list[[groups[1]]] <- data.eval %>%
    filter(rlang::eval_tidy(rlang::parse_expr(paste0(groups[1], ">=", lower.pos, "&", groups[2], "<=", upper.neg)))) %>%
    pull(var = "variables", name = NULL)

if (hasArg(sig.variables)) results.list[[groups[1]]] <- intersect(results.list[[groups[1]]],
                                                                  sig.variables)


  results.list[[groups[2]]] <- data.eval %>%
    filter(rlang::eval_tidy(rlang::parse_expr(paste0(groups[2], ">=", lower.pos, "&", groups[1], "<=", upper.neg)))) %>%
    pull(var = "variables", name = NULL)

  if (hasArg(sig.variables)) results.list[[groups[2]]] <- intersect(results.list[[groups[2]]],
                                                                    sig.variables)


  results.list[["common"]] <- setdiff(
    data.eval %>%
      filter(rlang::eval_tidy(rlang::parse_expr(paste0(groups[1], ">", upper.neg, "&", groups[2], ">", upper.neg)))) %>%
      pull(var = "variables", name = NULL),
    c(results.list[[groups[1]]], results.list[[groups[2]]]))


  # Plot or export
  if (plot || export) {

    list.plot <- results.list
    list.plot[["common"]] <- NULL

    for (i in groups) {
      list.plot[[i]] <- c(list.plot[[i]], results.list[["common"]])
    }

    if (plot) plot_venn(x = list.plot, name = paste("Venn", groups, sep = "_"), plot = plot, export = export)

  }

  # Save
  if (save) {
    # Save unique protein identifications
    for (i in groups) {
      add_variables_data(data = results.list[[i]], name = paste0("unique_", i), dataset = dataset, set.default = FALSE)
    }
    # Save commonly identified proteins
    name  <- ask_name(name, "Name of comparison? ")
    add_variables_data(data = results.list[["common"]], name = paste0("common_", name), dataset = dataset, set.default = F)
  }

  # Return
  invisible(results.list)

}
