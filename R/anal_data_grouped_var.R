#' Evalueates data groupwise and produces venn diagrams and table
#'
#' @param variables variables
#' @param observations observations
#' @param groups group definition
#' @param expr1 function(x) how columns should be evaluated
#' @param expr2 function(x) how groups should be evaluated
#' @param name name
#' @param observations.set set of observations
#' @param data.name data name
#' @param type data type
#' @param dataset dataset
#' @param save.separete save results from group evaluation
#' @param save save venn diagram groups
#' @param export.table export table
#' @param return.export.table return exported table
#'
#'
#' @return
#' @export
#'
#'
anal_data_grouped_var <- function(variables, observations, groups, expr1, expr2, name, observations.set, data.name, type, dataset, save.separete = T,
                                  save.variable.groups = T, show = T, export.plot = T, export.table = F, return.export.table = F) {


  data.grouped <- eval_data_grouped_var(variables = variables,
                                        observations = variables,
                                        groups = groups,
                                        expr1 = expr1,
                                        expr2 = expr2,
                                        name = name,
                                        observations.set,
                                        data.name = data.name,
                                        type = type,
                                        dataset = dataset,
                                        set.default = F,
                                        save = save.separate,
                                        return = T)



  plot_venn(x = x,
            plot = show,
            export = export.plot)



  if(save.variable.gorups) {

  }


  if(return.export.table) return(table)

















}
