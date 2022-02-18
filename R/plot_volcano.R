#' Plots a volcano plot with ggplot2
#'
#' @param data_ data_
#' @param color column for color of points
#' @param p.value.cutoff p-value limit for coloring
#' @param pos.log2fc.cutoff positive log2 fold-change limit for coloring
#' @param neg.log2fc.cutoff negative log2 fold-change limit for coloring
#' @param highlight.variables variables to highlight by point.size
#' @param highlight.color color to use to highlight proteins
#' @param x.axis.title title of x-axis
#' @param y.axis.title title of y-axis
#' @param text.size size of text in points (5-8)
#' @param text.color color of text
#' @param point.size point size (0.5-2)
#' @param point.alpha transparency (0-1)
#' @param highlight.point.size size of point of highlighted variables
#' @param highlight.point.alpha transparency of points to be highlighted
#' @param x.axis.breaks break size between ticks of x-axis
#' @param y.axis.breaks break size between ticks of y-axis
#' @param axis.line.size width of axes lines
#' @param axis.color color of axes lines
#' @param axis.ticks.size width of axis ticks
#' @param axis.title.size size of axis title
#' @param axis.text.size size of axis labels
#' @param aspect.ratio y/x ratio
#' @param use.plotly make interactive plots with ggplotly() (default: T if html
#' document)
#' @param view view plot
#' @param input name of input data
#' @param output name of output data
#'
#' @return
#' @export
#'
#'
#'
plot_volcano <- function(data_,
                         color = "regulated",
                         p.value.cutoff = 0.05,
                         pos.log2fc.cutoff = 0,
                         neg.log2fc.cutoff = 0,
                         highlight.variables = NULL,
                         highlight.color = NULL,
                         x.axis.title = "log2 fold-change",
                         y.axis.title = "-log10(p-value)",
                         text.size = 6,
                         text.color = "black",
                         point.size = 2,
                         point.alpha = 0.8,
                         highlight.point.size = 3,
                         highlight.point.alpha = 0.8,
                         x.axis.breaks = 1,
                         y.axis.breaks = 1,
                         axis.line.size = 0.5,
                         axis.color = "black",
                         axis.ticks.size = 0.3,
                         axis.title.size = 8,
                         axis.text.size = 6,
                         aspect.ratio = 0.8,
                         use.plotly,
                         view = T,
                         input = "data_t.test",
                         output = "plot_t.test") {

  # Handle input
  input_list <- data_input(data_ = data_, input = input)

  if (input_list[["error"]]) return(invisible(input_list[["data"]]))

  else {
    data <- input_list[["data"]]
    input <- input_list[["input"]] # Remove if not used

  }



  # Add color column☺
  data <- data %>%
    dplyr::rowwise() %>%
    dplyr::rename(color = !!color) %>%
    {if (!is.null(highlight.color))
        dplyr::mutate(., color = ifelse(variables %in% highlight.variables,
                                 "highlight",
                                 color))
      else .} %>%
    dplyr::mutate(point.size =
                    ifelse(variables %in% highlight.variables,
                           highlight.point.size,
                           point.size)) %>%
    dplyr::mutate(alpha =
                    ifelse(variables %in% highlight.variables,
                           highlight.point.alpha,
                           point.alpha)) %>%
    dplyr::arrange(point.size, -p.value)



  # Identify if R Markdown output is html or pdf
  if (!hasArg(use.plotly)) use.plotly <- knitr::is_html_output()

  # Add text column for interactive plots
  if (use.plotly) {
    data$text <- paste(p2g(data$variables),
                               p2n(data$variables),
                               data$variables, sep = "\n")
  } else {
    #data$text <- p2g(data$variables)
    data$text <- "" #data$variables
  }


  # Line width factor
  lwf <- 1 / (ggplot2::.pt * 72.27 / 96)


  p <- ggplot(data = data,
              aes(x = log2.fc,
                  y = -log10(p.value),
                  col = color,
                  size = point.size,
                  text = text,
                  alpha = alpha)) +
    geom_point(shape = 16, stroke = 0) +
    scale_size_continuous(range = range(data$point.size)) +
    scale_alpha_identity() +
    theme(aspect.ratio = aspect.ratio,
          axis.line = element_line(size = 0.5 * lwf, color = axis.color),
          panel.background = element_blank(),
          axis.text = element_text(size = axis.text.size, color = text.color),
          axis.ticks = element_line(size = axis.ticks.size * lwf,
                                    color = axis.color),
          axis.title = element_text(size = axis.title.size, color = text.color),
          legend.position = 0) +
    scale_color_manual(values = c("highlight" = highlight.color,
                                  "up" = "red",
                                  "down" = "blue",
                                  "not" = "grey")) +
    scale_x_continuous(
      limits = axis_limit_breaks(plot_limits = range(data$log2.fc),
                                 break.space = x.axis.breaks)$limits,
      breaks = axis_limit_breaks(plot_limits = range(data$log2.fc),
                                 break.space = x.axis.breaks)$breaks,
      expand = c(0, 0)) +
    scale_y_continuous(
      limits = axis_limit_breaks(plot_limits = range(-log10(data$p.value)),
                                 break.space = y.axis.breaks)$limits,
      breaks = axis_limit_breaks(plot_limits = range(-log10(data$p.value)),
                                 break.space = y.axis.breaks)$breaks,
      expand = c(0, 0)) +
    xlab(x.axis.title) +
    ylab(y.axis.title)


  # Generate interactive plot with ggplotly()
  if (use.plotly) {
    p <- plotly::ggplotly(p, tooltip = "text")
  }


  # View plot
  if (view) {
    if (knitr::is_html_output() || knitr::is_latex_output())
      return(p)
    else
      print(p)
    }

  # Prepare return
  if (input_list[["list.input"]]) {
    data_[[paste0("data_", output)]] <- data
    data_[[output]] <- p
  }

  else data_ <- list(data = data,
                     plot = p)

  # Return
  return(invisible(data_))

}
