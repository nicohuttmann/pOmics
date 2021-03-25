analyse_pca <- function() {
  
  
  
  data <- get_data(variables = get_GO_proteins("lipid droplet", include.child.terms = T))
  
  
  
  # 
  
  
  #data <- scale(data) #Add protein groups
  
  
  data <- data.frame(groups, data)
  
  #data <- data_to_tibble(data, row.names = "observations")
  
  
  data.pca <- prcomp(data[-1], scale. = T)
  autoplot(data.pca)
  
  pca.plot <- data.frame(groups,
                         data.pca[["x"]])
  boxplot(PC1 ~ groups, data = pca.plot)
  
  pca.plot <- data.pca[["x"]]
  
  
  groups <- get_groups(groups, observations = clean)
  
  
  
  #pca.plot$groups <- factor(pca.plot$groups, levels = c("wt ND", "wt HID", "Hjv-/- ND", "Hjv-/- IDD"))
  
  
  p <- ggplot(pca.plot, aes(x = PC1, y = PC2, group = groups)) +
    geom_point(aes(shape = groups, color = groups), size = 3) +
    scale_shape_manual(values = c(16,15,16,15)) +
    scale_color_manual(values = c("grey50", "grey50", "black", "black")) +
    #stat_ellipse(aes(linetype = group, color = group), size = 1, show.legend = F) +
    #scale_linetype_manual(values = c(1,2,1,2)) +
    theme(aspect.ratio = 1,
          panel.background = element_blank(),
          panel.border = element_rect(fill = NA, size = 1),
          plot.title = element_text(size = 14, face = "bold", color = "black"),
          plot.subtitle = element_text(size = 12, face = "bold", color = "black"),
          axis.text = element_text(size = 12, face = "bold", color = "black"),
          axis.ticks = element_line(size = 1, color = "black"),
          axis.title.y = element_text(face = "bold", size = 12),
          axis.title.x = element_text(face = "bold", size = 12),
          legend.key = element_blank(),
          legend.text = element_text(size = 12, face = "bold", color = "black"),
          legend.title = element_blank()) +
    labs(x = "X-variate 1: xx% expl. var",
         y = "Y-variate 2: xx% expl. var")
  
  p
  
  
  
  
  
  
  
}