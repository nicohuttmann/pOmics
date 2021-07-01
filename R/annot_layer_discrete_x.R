#' Creates discrete annotation layer
#'
#' @param data data
#' @param pos_table_x pos_table_x (function specific)
#' @param x x-axis column
#' @param value value column
#' @param colors color vector
#' @param legend.direction direction of legend entries ("horizontal", "vertical")
#' @param legend.title title of legend
#'
#' @return
#' @export
#'
#'
annot_layer_discrete_x <- function(data, pos_table_x, x = "observations", value = "groups", colors = NULL,
                                   legend.direction = "vertical", legend.title = "") {


  data_ann_layer <- data %>%
    dplyr::select(c(!!x, !!value)) %>%
    dplyr::rename(x = !!x, value = !!value) %>%
    dplyr::left_join(pos_table_x, by = "x") %>%
    dplyr::mutate(y_center = 1) %>%
    dplyr::mutate(height = 1)


  if (is.null(colors)) colors <- RColorBrewer::brewer.pal(n = length(unique(data_ann_layer[["value"]])),
                                                          name = "Accent")



    p <- ggplot(data_ann_layer,
           aes(x = x_center, y = y_center, fill = value,
               height = height, width = width)) +
    geom_tile() +
    scale_fill_manual(values = colors, name = legend.title) +
    scale_x_continuous(breaks = pos_table_x$x_center,
                       limits = with(
                         pos_table_x,
                         c(min(x_center - 0.5 * width), max(x_center + 0.5 * width))
                       ),
                       expand = c(0, 0)) +
    scale_y_continuous(#breaks = pos_table_y[, "y_center"],
                       #labels = labels.y,
                       #limits = axis_limits_y,
                       expand = c(0, 0)) +
    theme_iDC_annot_layer() +#coord_fixed(ratio = ratio.hm) +
    theme(axis.text.x = element_blank(), legend.direction = legend.direction)

    return(p)

}
