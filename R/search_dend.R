search_dend_ <- function(cor_list, variables, name, k = 20) {
  
  table_p <- cor_list[["table"]][, 1:(k + 1)]
  table <- table_p
  table_p[, -1] <- 0
  
  # 
  for (k in 1:k) {
    
    for (cluster in 1:k) {
      
      table_p[table[[k + 1]] == cluster, ] <- fisher_pvalue(a = table[[k + 1]] == cluster,
                                                            b = variables)
      
    }
    
  }
  
  
  
}