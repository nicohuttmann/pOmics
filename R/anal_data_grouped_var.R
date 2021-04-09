#' Evalueates data groupwise and produces venn diagrams and table
#'
#'
#' @param variables variables
#' @param observations observations
#' @param groups group definition
#' @param expr function(x) how groups should be evaluated
#' @param name name
#' @param observations.set set of observations
#' @param data.name data name
#' @param dataset dataset
#' @param save.separete save results from group evaluation in variables data
#' @param save.variable.groups save variable groups in variables data
#' @param show show plot
#' @param export.plot exports plot to pdf
#' @param export.table exports table of proteins
#'
#' @return
#' @export
#'
#'
anal_data_grouped_var <- function(variables, observations, groups, expr, name, observations.set, data.name, dataset, save.separate = T,
                                  save.variable.groups = T, show = T, export.plot = T, export.table = F) {


  data.grouped <- eval_data_grouped_var(variables = variables,
                                        observations = variables,
                                        groups = groups,
                                        expr = expr,
                                        name = name,
                                        observations.set,
                                        data.name = data.name,
                                        dataset = dataset,
                                        save = save.separate)




  plot_venn(x = data.grouped,
            plot = show,
            export = export.plot)



  if(save.variable.groups) {

  }

# Return export table
  invisible(table)


}
