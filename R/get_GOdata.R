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

  # Identify statistic
  if (length(unique(proteins)) == 2) statistic <- "fisher"
  else statistic <- "ks"


  # Search existing GO objects
  GOdata.n <- unlist(lapply(X = .databases[["GOdata"]][grep(paste(ontology, statistic, sep = "_"), c(names(.databases[["GOdata"]])))],
                            FUN = function(x) {identical(sort(names(proteins)), sort(topGO::allGenes(x)))}))


  # GOdata object found
  if (length(names(which(GOdata.n))) > 0) {
    GOdata <- .databases[["GOdata"]][[names(which(GOdata.n))[1]]]
    # Update genes; fisher
    if (statistic == "fisher") GOdata <- topGO::updateGenes(object = GOdata,
                                                            geneList = as.factor(proteins))
    # Update genes; ks
    else GOdata <- topGO::updateGenes(object = GOdata,
                                      geneList = proteins,
                                      geneSelFun = function(x) {x > mean(proteins, na.rm = T)})

  }
  # No feasible GOdata object found
  else GOdata <- new_GOdata(proteins = proteins,
                            ontology = ontology,
                            statistic = statistic,
                            nodeSize = 10,
                            save = TRUE)


  # Return
  return(GOdata)
}
