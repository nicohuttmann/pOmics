#' Plots stacked bar plot to indicate significantly changed proteins from eval_anova output
#'
#' @param data data
#' @param columns columns to use for plot
#' @param labels x-axis labels for columns
#' @param x.axis.title title of x-axis
#' @param y.axis.title title of y-axis
#' @param text.size size of text in points (5-8)
#' @param text.color color of text
#' @param y.axis.breaks break size between ticks of y-axis
#' @param axis.line.size width of axes lines
#' @param axis.color color of axes lines
#' @param axis.ticks.size width of axis ticks
#' @param axis.title.size size of axis title
#' @param axis.text.size size of axis labels
#' @param aspect.ratio y/x ratio
#'
#' @return
#' @export
#'
#'
plot_gg_bar_anova_eval <- function(data, columns, labels, 
                                    x.axis.title = "Group", y.axis.title = "Proteins",
                                    text.size = 6, text.color = "black", y.axis.breaks = 1,
                                    axis.line.size = 0.5, axis.color = "black", axis.ticks.size = 0.3,
                                    axis.title.size = 8, axis.text.size = 6, aspect.ratio = 0.8) {
  
  
  regulation <- c("up", "down")
  
  if (!hasArg(labels)) labels <- columns
  
  
  
  # Prepare data frame
  data.plot <- tibble::tibble(groups = factor(rep(labels, each = 2), levels = labels),
                              regulation = factor(rep(regulation, times = length(labels)), levels = regulation),
                              count = 0)
  
  # Count proteins
  for (i in seq_along(columns)) {
    
    for (j in regulation) {
      
      # Count proteins
      data.plot[(labels[i] == data.plot$groups) %and% (j == data.plot$regulation), "count"] <- sum(data[[columns[i]]] == j)
      
    }
    
  }
  
  
  lwf <- 1 / (ggplot2::.pt * 72.27 / 96)
  
  # Stacked
  ggplot(data.plot, aes(x = groups, y = count, fill = regulation)) + 
    geom_bar(position="stack", stat="identity") +
    theme(aspect.ratio = aspect.ratio,
          axis.line = element_line(size = 0.5 * lwf, color = axis.color),
          panel.background = element_blank(),
          axis.text = element_text(size = axis.text.size, color = text.color),
          axis.ticks = element_line(size = axis.ticks.size * lwf, color = axis.color),
          axis.title = element_text(size = axis.title.size, color = text.color),
          legend.position = 0) +
    scale_fill_manual(values = c("up" = "red", "down" = "blue"))

}

