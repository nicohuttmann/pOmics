overlap <- function(cor_list, proteins) {
  
  
  
  cor_list <- cor.mcf10a
  
  
  
  # Get adjacency matrix from list
  adjacency.matrix <- cor_list[["adjacency"]]
  
  
  proteins <- targets
  
  # Get proteins in adjacency matrix
  proteins <- intersect(proteins,
                        colnames(adjacency.matrix))
  
  # check rsidual proteins
  if (length(proteins) == 0) stop("No given protein in correlation matrix.")
  
  
  
  #proteins <- colnames(adjacency.matrix)[sample(x = ncol(adjacency.matrix), size = 2)]
  
  
  
  
  # Set diagonal to 0
  adjacency.matrix <- set_diagonal(data = adjacency.matrix, diag = 0)
  
  
  # Sum coefficients
  con <- apply(X = adjacency.matrix[proteins, ], MARGIN = 2, FUN = sum) / apply(X = adjacency.matrix[proteins, ], MARGIN = 2, FUN = function(x) sum(x > 0))
    
  
  
  
  # Scale
  con <- con / ( apply(X = adjacency.matrix, MARGIN = 2, FUN = sum) / (ncol(adjacency.matrix) - 1) )
  
  hist(con)
  
  
  sort(con, decreasing = T) %>% head %>% names %>%  summary_proteins_2()
  
  
  
}
