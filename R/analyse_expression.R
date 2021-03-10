#' Calculates Fold change and p-value from given dataframe or defined variables, observations and groups
#'
#' @param data (optional) data frame
#' @param variables (optional) variables definition
#' @param observations (optional) observatoins definition
#' @param groups groups
#' @param control (optional) define control sample (case / control)
#' @param data.name (optional)
#' @param var.equal t-test with equal variance
#' @param paired t-test with paired samples
#' @param type data type
#' @param observations.set observations.set
#' @param dataset dataset
#' @param plot Plot?
#' @param view View results matrix?
#' @param save Save results matrix?
#'
#' @return
#' @export
#'
#'
analyse_expression <- function(data, variables = "default", observations = "default", groups, control, data.name, var.equal = T, paired = F, type = "LFQ", observations.set, dataset, plot = T, view = F, save = F) {

  # Check data input
  if (!hasArg(data) && variables == "default" && observations == "default") stop("Provide data or specify variables and observations.")

  # Check dataset
  dataset <- get_dataset(dataset)

  # Get observations set
  observations.set <- get_observations_set(observations.set = observations.set, dataset = dataset)

  # Get variables
  variables <- get_variables(variables = {{variables}},
                             dataset = dataset)

  # Get observations
  observations <- get_observations(observations = {{observations}},
                                   observations.set = observations.set,
                                   dataset = dataset)

  # No data given
  if (!hasArg(data)) {

    # Get data
    data <- get_data(variables = variables,
                     observations = observations,
                     observations.set = observations.set,
                     name = data.name,
                     type = type,
                     dataset = dataset)

  }


  # Get groups
  groups <- get_groups(groups = !!dplyr::enquo(groups),
                       control = control,
                       observations = rownames(data),
                       observations.set = observations.set,
                       dataset = dataset)


  # Check groups
  if (length(levels(groups)) != 2) stop("More than two groups defined.")


  # Data list
  expr_list <- tibble::lst(data = data)



  # Calculate fold change and p-value
  data.log2 <- log2(data)
  data.log2[is.infinite(data.log2)] <- 0




  data.ttest <- matrix(ncol = 2, nrow = ncol(data))

  rownames(data.ttest) <- colnames(data)

  colnames(data.ttest) <- c("log2.fc", "p.value")


  for(i in 1:nrow(data.ttest)) {
    data.ttest[i,"log2.fc"] <- log2(mean(data[groups == levels(groups)[2],i]) / mean(data[groups == levels(groups)[1],i]))
    data.ttest[i,"p.value"] <- t.test(data.log2[groups == levels(groups)[2],i], data.log2[groups == levels(groups)[1],i], var.equal = var.equal, paired = paired)$p.value
  }


  # Add data
  expr_list[["ttest"]] <- data.ttest


  p <- plot_volcano(x = data.ttest[,"log2.fc"],
                    y = data.ttest[,"p.value"], print = F)

  expr_list[["plot"]] <- p


  # Plot
  if (plot) {

    print(p)

  }

  # View
  if (view) save2cache(data = data.ttest, view = TRUE)

  # Save
  if (save) {

    name <- paste0(levels(groups)[2], "-", levels(groups)[1])

    add_variables_data(data = data.ttest[, "log2.fc"], name = paste0("fc_", name), dataset = dataset, set.default = FALSE)

    add_variables_data(data = data.ttest[, "p.value"], name = paste0("p_ttest_", name), dataset = dataset, set.default = FALSE)

  }





  # Return
  invisible(expr_list)

}
