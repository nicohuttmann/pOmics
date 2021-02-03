#' Plots cormat with y-axis dendrogram
#'
#' @param data data
#' @param dend_y y-axis dendrogram
#' @param scale should data be scaled
#' @param transform should data be transformed
#'
#' @return
#' @export
#'
#' @import ggplot2
#' @importFrom magrittr %>% 
#'
plot_cormat_enrichment <- function(cor_list, name) {
  
  
  # Get adjacency matrix
  data <- cor_list[["adjacency"]]
  
  
  # Save names
  x_names <- colnames(data)
  y_names <- rownames(data)
  
  
  
  # y-axis dendrogram
  dend_y <- cor_list[["dendrogram"]] %>% 
    as.dendrogram()
    
  
  
  # Get dendrogram data
  dend_data_y <- ggdendro::dendro_data(dend_y)
  
  # Invert layout observations
  segment_data_y <- with(ggdendro::segment(dend_data_y),
                         data.frame(x = y, y = x, xend = yend, yend = xend))
  
  # Position observations
  pos_table_y <- with(dend_data_y$labels,
                      data.frame(y_center = x,
                                 y = as.character(label),
                                 height = 1))
  
  # Position x
  pos_table_x <- with(dend_data_y$labels,
                      data.frame(x_center = x,
                                 x = as.character(label),
                                 width = 1))
  
  
  
  # Construct heatmap df
  data_heatmap <- data %>%
    set_diagonal(diag = 1) %>% 
    reshape2::melt(value.name = "expr", varnames = c("y", "x")) %>%
    dplyr::left_join(pos_table_x, by = "x") %>%
    dplyr::left_join(pos_table_y, by = "y")
  
  
  
  # Limits for the vertical axes
  axis_limits_y <- with(
    pos_table_y,
    c(min(y_center - 0.5 * height), max(y_center + 0.5 * height))
  )
  
  # Limits for the horizontal axes
  # axis_limits_x <- <- data.frame(sample = sample_names) %>%
  #   mutate(x_center = (1:n()), 
  #          width = 1)
  # 
  
  
  
  
  # Get enrichment table
  dend.enrich <- cor_list[[paste0("dend.enrich_", name)]]
  
  segment_data_y <- add_segment_pvalue(segment_data_y, dend.enrich = dend.enrich)
  
  
  
  
  # Indicate highest enrichment cluster
  protein.cluster.pos <- pos_table_x[pos_table_x[, 2] %in% dend.enrich[["proteins"]], 1]
  cluster_table <- tibble(xmin = min(protein.cluster.pos) - 0.5,
                          xmax = max(protein.cluster.pos) + 0.5,
                          ymin = min(protein.cluster.pos) - 0.5,
                          ymax = max(protein.cluster.pos) + 0.5)
  
  
  
  
  
  
  # Heatmap plot
  p_hmap <- ggplot(data_heatmap,
                   aes(x = x_center, y = y_center, fill = expr,
                       height = height, width = width)) +
    geom_tile() +
    geom_rect(data = cluster_table, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
              fill = NA, colour = "red", size = 4/.pt, inherit.aes = FALSE) +
    scale_fill_gradient2("expr", high = "darkred", mid = "white", low = "darkblue") + #low = "navyblue", mid = "white", high = "red4"
    scale_x_reverse(breaks = rev(pos_table_x$x_center),
                    #labels = pos_table_x$x,
                    #limits = axis_limits_x,
                    expand = c(0, 0)) +
    # For the y axis, alternatively set the labels as: gene_position_table$gene
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       #labels = rep("", nrow(pos_table_y)),
                       limits = axis_limits_y,
                       expand = c(0, 0)) +
    #labs(x = "Sample", y = "") +
    #theme_bw() +
    theme(axis.text.x = element_blank(), #, hjust = 1, angle = 45
          axis.text.y = element_blank(),
          # margin: top, right, bottom, and left
          plot.margin = unit(c(0, 0, 0, 0), "cm"), # unit(c(1, 0.2, 0.2, -0.7)
          panel.grid.minor = element_blank(),
          panel.border = element_rect(colour = "black", fill=NA, size=1/.pt),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          legend.position = "none")
  
  
  
  
  
  # Dendrogram plot y
  p_dend_y <- ggplot(segment_data_y) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend, color = logpvalue), size = 3/.pt) +
    scale_x_reverse(expand = c(0, 0)) +
    scale_y_continuous(breaks = pos_table_y$y_center,
                       #                   labels = pos_table_y$y,
                       limits = axis_limits_y,
                       expand = c(0, 0)) +
    #labs(x = "Distance", y = "", colour = "", size = "") +
    #theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          plot.background = element_blank(),
          axis.text = element_blank(),
          axis.line.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.margin = unit(c(0, 0, 0, 0), "cm"),
          legend.position = "left") +
    scale_color_gradient(low = "yellow", high = "red")
  
  
  
  
  
  print(cowplot::plot_grid(p_dend_y, p_hmap, align = 'h', rel_widths = c(.5, 1)))
  
  
}
  