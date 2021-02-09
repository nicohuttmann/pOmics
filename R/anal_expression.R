#' Calculates Fold change and p-value from given dataframe or defined variables, observations and groups
#'
#' @param data (optional) data frame
#' @param variables (optional) variables definition
#' @param observations (optional) observatoins definition
#' @param groups groups
#' @param control (optional) define control sample (case / control)
#' @param data.name (optional) 
#' @param var.equal t-test with equal variance
#' @param type data type
#' @param observations.set observations.set
#' @param dataset dataset
#' @param plot Plot?
#' @param view View results matrix?
#' @param save Save results matrix?
#' @param return Return results matrix?
#'
#' @return
#' @export
#'
#'
anal_expression <- function(data, variables = "default", observations = "default", groups, control, data.name, var.equal = T, type = "LFQ", observations.set, dataset, plot = T, view = F, save = F, return = T) {
  
  # Check dataset
  dataset <- get_dataset(dataset)
  
  # Get observations set
  observations.set <- get_observations_set(observations.set = observations.set, dataset = dataset)
  
  # 
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
  
  
  # Calculate fold change and p-value
  data.log2 <- log2(data)
  data.log2[is.infinite(data.log2)] <- 0
  
  
  
  
  data.ttest <- matrix(ncol = 2, nrow = ncol(data))
  
  rownames(data.ttest) <- colnames(data)
  
  colnames(data.ttest) <- c("log2.fc", "p.value")
  
  
  for(i in 1:nrow(data.ttest)) {
    data.ttest[i,"log2.fc"] <- log2(mean(data[groups == levels(groups)[2],i]) / mean(data[groups == levels(groups)[1],i]))
    data.ttest[i,"p.value"] <- t.test(data.log2[groups == levels(groups)[2],i], data.log2[groups == levels(groups)[1],i], var.equal = var.equal)$p.value
  }
  
  
  
  # Plot
  if (plot) {
    
    plot(data.ttest[,"log2.fc"],
         -log10(data.ttest[,"p.value"]),
         pch = 16,
         xlab = paste0("log2 fold-change (", levels(groups)[1], "-", levels(groups)[2], ")"),
         ylab = "-log10(p-value)")
    abline(h = -log10(0.05))
    
  }
  
  # View
  if (view) save2cache(data = data.ttest, view = TRUE)
  
  # Save
  #if (save) 
  
  # Return
  if (return) return(data.ttest)
  
}
