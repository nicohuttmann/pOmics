#' PLots heatmap
#'
#' @param variables variables (see get variables)
#' @param observations observations (see get observations)
#' @param groups groups
#' @param observations.set set of observations
#' @param data.name data name
#' @param dataset dataset
#' @param scale should data be scaled
#' @param grouping should data be grouped
#' @param clustering.method clustering method
#' @param variables.labels labels of variables
#' @param observations.labels labels of observations
#' @param variables.order should variables be ordered by dendrogram
#' @param observations.order should observations be ordered by dendrogram
#' @param ratio y/x-ratio of tiles
#'
#' @return
#' @export
#'
#'
plot_heatmap <- function(variables, observations, groups, observations.set, data.name, dataset,
                         scale = T, grouping, clustering.method = "complete", variables.labels, observations.labels,
                         variables.order = T, observations.order = T, ratio = 3) {



  # Get data
  data <- get_data(variables = !!dplyr::enquo(variables),
                   observations = !!dplyr::enquo(observations),
                   observations.set = observations.set,
                   data.name = data.name,
                   dataset = dataset,
                   tidy = F)

  # Get groups
  if (hasArg(groups)) {
    groups <- get_groups(observations = rownames(data),
                       groups = !!dplyr::enquo(groups),
                       observations.set = observations.set,
                       dataset = dataset)
  }




  # Scale (Z-scores)
  if (scale) data <- scale(data)

  data <- data[, apply(data, MARGIN = 2, function(x) !any(is.na(x)))]



  # Combine groups
  if (hasArg(grouping) && hasArg(groups)) data <- combine_data_groups(data = data, groups = groups, method = grouping)




  # Start clustering
  # Observations
  dendro.observations <- hclust(d = dist(x = data), method = clustering.method)


  # Variables
  dendro.variables <- hclust(d = dist(x = t(data)), method = clustering.method)



  #
  if (variables.order) data.heatmap.melt <- data[, dendro.variables$order]

  else data.heatmap.melt <- data
  # Prepare heatmap data
  # Rename
  # colnames(data.heatmap.melt) <- raw.data[[2]][colnames(data.heatmap.melt), "gene"]




  #Melt dataframe
  #df.heatmap <- cbind(row.names(df.heatmap),df.heatmap)
  data.heatmap.melt <- reshape2::melt(data.heatmap.melt, id.vars = rownames(data.heatmap.melt))
  names(data.heatmap.melt) <- c("X", "Y", "value")


  #Sort samples
  if (observations.order) data.heatmap.melt$X <- factor(data.heatmap.melt$X,
                                                        levels = data.heatmap.melt$X[dendro.observations$order])

  else data.heatmap.melt$X<-factor(data.heatmap.melt$X, levels = unique(data.heatmap.melt$X))




  # Calculate ratio of tiles
  ratio <- nrow(data) / ncol(data) * ratio



  #Just plot
  p <- ggplot(data.heatmap.melt, aes(X, Y)) +
    geom_tile(aes(fill = value)) +
    scale_fill_gradient2(low = "navyblue", mid = "white", high = "red4") + #, limits = c(-1.6,1.6)
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
          #axis.text.y = element_text(color = "black", face = "bold"),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_text(),
          panel.border = element_rect(colour = "black", fill=NA, size=1/.pt)
    ) +
    scale_x_discrete(expand = c(0,0)) +
    scale_y_discrete(expand = c(0,0)) +
    coord_fixed(ratio = ratio) +
    scale_y_continuous(breaks = pos_table_y[, "y_center"],
                       labels = ylabels,
                       expand = c(0, 0),
                       position = "right")

  print(p)



  # Plot dendrogram
  #print(ggdendrogram(data = as.dendrogram(dendro.samples), rotate = FALSE))
  #print(ggdendrogram(data = as.dendrogram(dendro.proteins), rotate = TRUE, labels = FALSE))

}
