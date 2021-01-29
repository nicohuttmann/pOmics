#' Searches for GOdata or makes new
#'
#' @param proteins protein vector
#' @param ontology GO ontology
#' @param statistic statistic
#'
#' @return
#' @export
#'
#'
get_GOdata <- function(proteins, ontology) {
<<<<<<< HEAD

  # Identify statistic
  if (length(unique(proteins)) == 2) statistic <- "fisher"
  else statistic <- "ks"


  # Search existing GO objects
  GOdata.n <- unlist(lapply(X = .databases[["GOdata"]][grep(paste(ontology, statistic, sep = "_"), c(names(.databases[["GOdata"]])))],
                            FUN = function(x) {identical(sort(names(proteins)), sort(topGO::allGenes(x)))}))


=======
  
  # Identify statistic
  if (length(unique(proteins)) == 2) statistic <- "fisher"
  else statistic <- "ks"
  
  
  # Search existing GO objects
  GOdata.n <- unlist(lapply(X = .databases[["GOdata"]][grep(paste(ontology, statistics, sep = "_"), c(names(.databases[["GOdata"]])))],
                            FUN = function(x) {identical(sort(names(proteins)), sort(allGenes(x)))}))
 
  
>>>>>>> 8038c7547053747d8c5b58ae7c7fccdb642c2412
  # GOdata object found
  if (length(names(which(GOdata.n))) > 0) {
    GOdata <- .databases[["GOdata"]][[names(which(GOdata.n))[1]]]
    # Update genes; fisher
<<<<<<< HEAD
    if (statistic == "fisher") GOdata <- topGO::updateGenes(object = GOdata,
                                                            geneList = as.factor(proteins))
    # Update genes; ks
    else GOdata <- topGO::updateGenes(object = GOdata,
                                      geneList = proteins,
                                      geneSelFun = function(x) {x > mean(proteins, na.rm = T)})
=======
    if (statistic == "fisher") GOdata <- updateGenes(object = GOdata,
                                                     geneList = as.factor(proteins))
    # Update genes; ks
    else GOdata <- updateGenes(object = GOdata,
                               geneList = proteins,
                               geneSelFun = function(x) {x > mean(proteins, na.rm = T)})
>>>>>>> 8038c7547053747d8c5b58ae7c7fccdb642c2412
  }
  # No feasible GOdata object found
  else GOdata <- new_GOdata(proteins = proteins,
                            ontology = ontology,
                            statistic = statistic,
                            nodeSize = 10,
                            save = TRUE,
                            return = TRUE)
<<<<<<< HEAD

=======
  
>>>>>>> 8038c7547053747d8c5b58ae7c7fccdb642c2412
  # Return
  return(GOdata)
}
