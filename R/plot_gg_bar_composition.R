#' Plots data as bar plot with ggplot2
#'
#' @param data_ data_
#' @param TERMS TERMS to plot
#' @param color names vector of colors
#' @param xlab labels of x-axis
#' @param ylab label of y-axis
#' @param legend.name name of lagend
#' @param legend.position position of legend ("none", "right", "bottom", "top", "left")
#' @param aspect.ratio aspect ratio of x- and y-axis
#' @param view view plot after generation
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#' @import ggplot2
#'
plot_gg_bar_composition <- function(data_, TERMS = "mitochondrion", color, xlab = "Fraction of Proteins", ylab = "",
                                    legend.name = "", legend.position = "right", aspect.ratio = 1, view = T,
                                    input = "data_annotation_composition", output = "plot_annotation_composition") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]]
    list.input <- input_list[["list.input"]]
  }


  data.melt_1 <- data %>%
    dplyr::filter(TERM %in% TERMS) %>%
    dplyr::select(c(TERM, starts_with("count"))) %>%
    reshape2::melt() %>%
    dplyr::mutate(variable = gsub("count.", "", variable))

  data.melt_2 <- data %>%
    dplyr::filter(TERM %in% TERMS) %>%
    dplyr::select(c(starts_with("percent"))) %>%
    reshape2::melt() %>%
    dplyr::mutate(variable = gsub("percent.", "", variable))

  #data.melt <- dplyr::full_join(data.melt_1, data.melt_2, "variable")
  data.melt <- cbind(data.melt_1, dplyr::select(data.melt_2, -variable))

  names(data.melt) <- c("TERM", "variable", "Count", "Fraction")

  # Change names of annotations
  if (length(names(TERMS)) > 0) {

    names(TERMS)[names(TERMS) == ""] <- TERMS[names(TERMS) == ""]

    data.melt$TERM <- names(TERMS)[match(data.melt$TERM, TERMS)]

  }


  data.melt$variable <- factor(data.melt$variable, levels = rev(unique(data.melt$variable)))
  data.melt$TERM <- factor(data.melt$TERM, levels = rev(unique(data.melt$TERM)))


  if (!hasArg(color)) color <- rev(RColorBrewer::brewer.pal(9, "Blues")[c(3,5,7)])

  # Plot
  p <- ggplot(data = data.melt, aes(x = TERM, y = Fraction, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(labels = scales::percent) +
    scale_fill_manual(name = legend.name, values = color, guide = guide_legend(reverse=TRUE)) +
    theme_hjv_half_open() +
    theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
          axis.text.y = element_text(vjust = 0.5),
          aspect.ratio = aspect.ratio,
          legend.position = legend.position) +
    coord_flip() +
    xlab(ylab) +
    ylab(xlab)

  # Print plot
  if (view) print(p)



  # Prepare return
  if (list.input) data_[[output]] <- p

  else data_ <- p

  # Return
  return(invisible(data_))
}
