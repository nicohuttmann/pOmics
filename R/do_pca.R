do_pca <- function(data, group.column = "groups") {
  
  
  # PCA
  data <- get_data(observations = All) %>% 
    threshold_variables() %>% 
    
    prep_data(min.0 = 0.25, normalize.method = "none")
    include_groups(groups = groups)
  
  
  
  
  
  
  data <- pca_data[["data"]]
  
  
  
  pca_data <- data %>% 
    scale_
  
  
  pca <- pca_data %>% 
    dplyr::select(where(is.numeric)) %>% 
    prcomp()
  
  
  pca_data <- as_tibble(pca[["x"]]) %>% 
    dplyr::bind_cols(pca_data)
  
  
  ggplot(pca_data, aes(x = PC1, y = PC2, color = groups)) +
    geom_point()
  
  
  
}
