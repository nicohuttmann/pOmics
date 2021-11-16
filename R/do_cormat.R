#' Do a correlation matrix analysis as described by Horvath et al.
#'
#' @param data_ list or tibble
#' @param cor.method function to compute correlation coefficients
#' @param similarity.method similarity function ("absolute", "preserve", "none")
#' @param diag0 set autocorrelation to 0
#' @param adjacency.method adjacency function ("sigmoid", "power", "none")
#' @param alpha alpha parameter for sigmoid function
#' @param theta theta parameter for sigmoid function
#' @param beta beta parameter for power function
#' @param distance.m ethod method to calculate distance; see ?dist for options
#' @param clustering.method method to cluster columns and rows ("complete",
#' "average"); see ?hclust for options
#' @param plot generate default plot with plot_cor_matrix
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
do_cormat <- function(data_,
                      cor.method = "pearson",
                      diag0 = T,
                      similarity.method = "none",
                      adjacency.method = "none",
                      alpha,
                      theta,
                      beta,
                      distance.method = "euclidean",
                      clustering.method = "complete",
                      dataset,
                      plot = T,
                      input,
                      output = "data_cormat") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used
  }


  # Compute correlation measure
  col_names <- colnames(data)

  data <- data %>%
    tibble2matrix() %>%
    cor(method = cor.method) %>%
    # Calculate similarity from correlation
    similarity(method = similarity.method) %>%
    # Set autocorrelation to 0
    {if (diag0) {diag(.) <- 0; .}
      else .} %>%
    # Calculate adjacency from similarity
    adjacency(method = adjacency.method) %>%
    matrix2tibble(to.row.names = setdiff(col_names, colnames(.)))




  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[output]] <- data
  }

  else data_ <- data



  # Compute dendrograms
  data_ <- data_ %>%
    do_hclust(scale = FALSE,
              distance.method = distance.method,
              clustering.method = clustering.method,
              plot = F,
              input = output,
              output = output)


  # Plot cor matrix
  if (plot) {
    data_ <- plot_cormat(data_ = data_, view = T)
  }


  # Return
  return(data_)

}
