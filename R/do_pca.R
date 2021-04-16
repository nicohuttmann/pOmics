do_pca <- function(data, color = "groups") {

  # Check data input
  if (!color %in% colnames(data)) stop("Column indicationg individual or grouped coloring could not be found.")



  get_data(data.name = "LFQ.imp", variables = , observations = ) %>%
    include_groups(groups = groups) -> data



  pca_data <- data %>%
    scale_


  pca <- pca_data %>%
    dplyr::select(where(is.numeric)) %>%
    prcomp()


  pca_data <- as_tibble(pca[["x"]]) %>%
    dplyr::bind_cols(pca_data)

  pca_data <- pca_data %>%
    dplyr::mutate(color = rlang::eval_tidy(rlang::parse_expr(color)), .before = where(is.numeric))


  ggplot(pca_data, aes(x = PC1, y = PC2, color = color)) +
    geom_point()



}
