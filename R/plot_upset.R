#' Visualizes identification data as UpSet plot
#'
#' @param data data frame
#' @param order order of samples
#' @param view view plot
#'
#' @return
#' @export
#'
#'
plot_upset <- function(data, order, view = T) {

  # Transform dataframe to list
  data.list <- logical_df2list(data)


  # Make plot
  if (hasArg(order)) {
    plot <- UpSetR::upset(UpSetR::fromList(data.list),
                          order.by = "freq",
                          sets = rev(order),
                          nsets = length(data.list),
                          keep.order = T)
  } else {
    plot <- UpSetR::upset(UpSetR::fromList(data.list),
                          order.by = "freq",
                          nsets = length(data.list),
                          keep.order = T)
  }



  # View plot
  print(plot)

  # Return
  invisible(plot)

}
