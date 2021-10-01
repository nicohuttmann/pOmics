#' Template for functions that accept either a data frame or a list
#'
#' @param data_ list or tibble
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param point.size size of data points
#' @param view view plot
#' @param input name of input data
#' @param output.data name for output data
#' @param output.plot name for output plot data
#'
#' @return
#' @export
#'
#'
do_abundance_summary <- function(data_, xlab = "Protein rank", ylab = "median log10 LFQ",
                                 point.size = 1,
                                 view = T, input, output.data = "data_abundance",
                                 output.plot = "plot_abundance") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used
    
  }


  median.abundance <- lapply(X = data[, -1], FUN = function(x) {median(x[x>0])}) %>%
    unlist()

  median.abundance[is.na(median.abundance)] <- 0





  data.abundance <- dplyr::tibble(variables = names(median.abundance),
                                  median.abundance = median.abundance) %>%
    dplyr::mutate(order = rank(-median.abundance, )) %>%
    dplyr::mutate(labels = p2g(names(median.abundance))) %>%
    dplyr::arrange(order)

  data.plot <- data.abundance %>%
    dplyr::filter(median.abundance > 0)

  # protein.rank <- data.abundance %>%
  #   dplyr::filter(median.abundance > 0) %>%
  #   dplyr::pull(order, variables)


  # Labels
  data.plot$labels[6:nrow(data.plot)] <- NA
  data.plot$label.pos <- 200



  p <- ggplot(data.plot, aes(x = order, y = log10(median.abundance), label = labels)) +
    #geom_vline(xintercept = xint, linetype = "dashed", color = "grey") +
    ggrepel::geom_text_repel(nudge_x = 200, box.padding = .3, point.padding = 0.3, force = 0.2) +
    geom_point(size = point.size) +
    scale_x_continuous(name = xlab) +
    theme_hjv_half_open() +
    theme() +
    xlab(xlab) +
    ylab(ylab)





  # #quantile(median.abundance, probs = seq(0, 1, 0.2))
  # quantiles <- distribute_quantiles(protein.rank, n = 4) %>%
  #   fun_enrich(database = "CC", maxGSSize = 4000, pAdjustMethod = "BH")
  #
  # xint <- distribute_quantiles_inner_limits(protein.rank, n = 4)


  if (view) print(p)


  # Prepare return
  if (input_list[["list.input"]]) {

    data_[[output.data]] <- data.abundance
    data_[[output.plot]] <- p

  }


  else data_ <- p

  # Return
  return(data_)

}
