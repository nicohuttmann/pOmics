#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_hjv_framed <- function(font_size = 8, font_family = "", line_size = .5,
                             rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = small_size),
      axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_text(margin = margin(r = small_size / 4), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_line(color = "black", size = line_size),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
        margin = margin(t = half_line / 2),
        vjust = 1
      ),
      axis.title.x.top  = element_text(
        margin = margin(b = half_line / 2),
        vjust = 0
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_rect(color = "black", size = line_size_orig),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_hjv_framed_no_axes <- function(font_size = 8, font_family = "", line_size = .5,
                                     rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_blank(),
      axis.text.x       = NULL,
      axis.text.x.top   = NULL,
      axis.text.y       = NULL,
      axis.text.y.right = NULL,
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
        margin = margin(t = half_line / 2),
        vjust = 1
      ),
      axis.title.x.top  = element_text(
        margin = margin(b = half_line / 2),
        vjust = 0
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_rect(color = "black", size = line_size_orig),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_hjv_half_open <- function(font_size = 8, font_family = "", line_size = .5,
                                rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = small_size),
      axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_text(margin = margin(r = small_size / 4), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_line(color = "black", size = line_size),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
        margin = margin(t = half_line / 2),
        vjust = 1
      ),
      axis.title.x.top  = element_text(
        margin = margin(b = half_line / 2),
        vjust = 0
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}


#' Create a theme for heatmaps for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#' @return The theme.
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_hjv_heatmap <- function(font_size = 8, font_family = "", line_size = .5,
                              rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = small_size),
      axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      axis.text.x.top   = NULL,
      axis.text.y       = element_text(margin = margin(r = small_size / 4), hjust = 1),
      axis.text.y.right = NULL,
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
        margin = margin(t = half_line / 2),
        vjust = 1
      ),
      axis.title.x.top  = element_text(
        margin = margin(b = half_line / 2),
        vjust = 0
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_rect(color = "black", size = line_size_orig),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_hjv_heatmap_only <- function(font_size = 8, font_family = "", line_size = .5,
                                   rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(),
      # axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      # axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_blank(), element_text(margin = margin(r = small_size / 4), hjust = 1),
      # axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title = element_blank(),
      # axis.title.x      = element_text(
      #   margin = margin(t = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.x.top  = element_text(
      #   margin = margin(b = half_line / 2),
      #   vjust = 0
      # ),
      # axis.title.y      = element_text(
      #   angle = 90,
      #   margin = margin(r = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.y.right = element_text(
      #   angle = -90,
      #   margin = margin(l = half_line / 2),
      #   vjust = 0
      # ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "none",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_rect(color = "black", size = line_size_orig),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin = unit(c(0, 0, 0, 0), "null"),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_hjv_heatmap_only_noframe <- function(font_size = 8, font_family = "", line_size = .5,
                                           rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(),
      # axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      # axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_blank(), element_text(margin = margin(r = small_size / 4), hjust = 1),
      # axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title = element_blank(),
      # axis.title.x      = element_text(
      #   margin = margin(t = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.x.top  = element_text(
      #   margin = margin(b = half_line / 2),
      #   vjust = 0
      # ),
      # axis.title.y      = element_text(
      #   angle = 90,
      #   margin = margin(r = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.y.right = element_text(
      #   angle = -90,
      #   margin = margin(l = half_line / 2),
      #   vjust = 0
      # ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "none",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin = unit(c(0, 0, 0, 0), "null"),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_hjv_overlap_heatmap <- function(font_size = 8, font_family = "", line_size = .5,
                                      rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = small_size),
      axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_text(margin = margin(r = small_size / 4), hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
        margin = margin(t = half_line / 2),
        vjust = 1
      ),
      axis.title.x.top  = element_text(
        margin = margin(b = half_line / 2),
        vjust = 0
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_rect(color = "black", size = line_size_orig),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_iDC_dendrogram <- function(font_size = 8, font_family = "", line_size = .5,
                                 rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_blank(),
      # axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      # axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      #axis.text.y       = element_blank(), element_text(margin = margin(r = small_size / 4), hjust = 1),
      # axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.title = element_blank(),
      # axis.title.x      = element_text(
      #   margin = margin(t = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.x.top  = element_text(
      #   margin = margin(b = half_line / 2),
      #   vjust = 0
      # ),
      # axis.title.y      = element_text(
      #   angle = 90,
      #   margin = margin(r = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.y.right = element_text(
      #   angle = -90,
      #   margin = margin(l = half_line / 2),
      #   vjust = 0
      # ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "none",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin = unit(c(0, 0, 0, 0), "null"),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_iDC_annot_layer <- function(font_size = 8, font_family = "", line_size = .5,
                                  rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_blank(),
      # axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      # axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_blank(),
      # axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.title = element_blank(),

      # axis.title.x      = element_text(
      #   margin = margin(t = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.x.top  = element_text(
      #   margin = margin(b = half_line / 2),
      #   vjust = 0
      # ),
      # axis.title.y      = element_text(
      #   angle = 90,
      #   margin = margin(r = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.y.right = element_text(
      #   angle = -90,
      #   margin = margin(l = half_line / 2),
      #   vjust = 0
      # ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin = unit(c(0, 0, 0, 0), "null"),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_iDC_heatmap_labels_x <- function(font_size = 8, font_family = "", line_size = .5,
                                       rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = small_size),
      axis.text.x       = element_text(hjust = 1, angle = 45),
      #axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_blank(),
      #axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.title        = element_blank(),
      # axis.title.x.top  = element_text(
      #   margin = margin(b = half_line / 2),
      #   vjust = 0
      # ),
      # axis.title.y      = element_text(
      #   angle = 90,
      #   margin = margin(r = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.y.right = element_text(
      #   angle = -90,
      #   margin = margin(l = half_line / 2),
      #   vjust = 0
      # ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_blank(),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = unit(c(0, 0, 0, 0), "cm"),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_iDC_heatmap_labels_y <- function(font_size = 8, font_family = "", line_size = .5,
                                       rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = small_size),
      axis.text.x       = element_blank(),
      #axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      axis.text.y       = element_text(hjust = 0),
      #axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.title        = element_blank(),
      # axis.title.x.top  = element_text(
      #   margin = margin(b = half_line / 2),
      #   vjust = 0
      # ),
      # axis.title.y      = element_text(
      #   angle = 90,
      #   margin = margin(r = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.y.right = element_text(
      #   angle = -90,
      #   margin = margin(l = half_line / 2),
      #   vjust = 0
      # ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_blank(),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = unit(c(0, 0, 0, 0), "cm"),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_iDC_heatmap_w_legend <- function(font_size = 8, font_family = "", line_size = .5,
                                       rel_small = 12/14, rel_tiny = 11/14, rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_blank(),
      # axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      # axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      #axis.text.y       = element_blank(),
      # axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.title = element_blank(),
      # axis.title.x      = element_text(
      #   margin = margin(t = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.x.top  = element_text(
      #   margin = margin(b = half_line / 2),
      #   vjust = 0
      # ),
      # axis.title.y      = element_text(
      #   angle = 90,
      #   margin = margin(r = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.y.right = element_text(
      #   angle = -90,
      #   margin = margin(l = half_line / 2),
      #   vjust = 0
      # ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_rect(color = "black", size = line_size_orig),
      panel.grid        = element_blank(),
      # panel.grid.major  = NULL,
      # panel.grid.minor  = NULL,
      # panel.grid.major.x = NULL,
      # panel.grid.major.y = NULL,
      # panel.grid.minor.x = NULL,
      # panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin = unit(c(0, 0, 0, 0), "null"),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_size_small Relative size of small text (e.g., axis tick labels)
#' @param font_size_tiny Relative size of tiny text (e.g., caption)
#' @param font_size_large Relative size of large text (e.g., title)
#' @param font_family Font family for plot title, axis titles and labels,
#' legend texts, etc.
#' @param line_size Line size for axis lines.
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_phosprot_half_open <- function(font_size = 8,
                                     font_size_small = 7,
                                     font_size_tiny = 6,
                                     font_size_large = 10,
                                     font_family = "",
                                     line_size = .5) {


  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black",
                                       size = line_size,
                                       linetype = 1,
                                       lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA,
                                       size = line_size,
                                       linetype = 1),
      text              = element_text(family = font_family,
                                       face = "plain",
                                       color = "black",
                                       size = font_size,
                                       hjust = 0.5,
                                       vjust = 0.5,
                                       angle = 0,
                                       lineheight = .9,
                                       margin = margin(),
                                       debug = FALSE),

      axis.line         = element_line(color = "black",
                                       size = line_size,
                                       lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = font_size_small),
      axis.text.x       = element_text(margin = margin(t = font_size_small / 4),
                                       vjust = 1),
      axis.text.x.top   = element_text(margin = margin(b = font_size_small / 4),
                                       vjust = 0),
      axis.text.y       = element_text(margin = margin(r = font_size_small / 4),
                                       hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = font_size_small / 4),
                                       hjust = 0),
      axis.ticks        = element_line(color = "black", size = line_size),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
        margin = margin(t = half_line / 2),
        vjust = 1
      ),
      axis.title.x.top  = element_text(
        margin = margin(b = half_line / 2),
        vjust = 0
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = font_size_small),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0, size = font_size_small),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = font_size_small,
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = font_size_large,
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = font_size_small,
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = font_size_tiny,
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_size_small Relative size of small text (e.g., axis tick labels)
#' @param font_size_tiny Relative size of tiny text (e.g., caption)
#' @param font_size_large Relative size of large text (e.g., title)
#' @param font_family Font family for plot title, axis titles and labels,
#' legend texts, etc.
#' @param line_size Line size for axis lines.
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_thesis_framed_no_axis <- function(font_size = 8,
                                        font_size_small = 7,
                                        font_size_tiny = 6,
                                        font_size_large = 10,
                                        font_family = "",
                                        line_size = .5) {


  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black",
                                       size = line_size,
                                       linetype = 1,
                                       lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA,
                                       size = line_size,
                                       linetype = 1),
      text              = element_text(family = font_family,
                                       face = "plain",
                                       color = "black",
                                       size = font_size,
                                       hjust = 0.5,
                                       vjust = 0.5,
                                       angle = 0,
                                       lineheight = .9,
                                       margin = margin(),
                                       debug = FALSE),

      axis.line         = element_blank(),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_blank(),
      axis.text.x       = NULL,
      axis.text.x.top   = NULL,
      axis.text.y       = NULL,
      axis.text.y.right = NULL,
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
        margin = margin(t = half_line / 2),
        vjust = 1
      ),
      axis.title.x.top  = element_text(
        margin = margin(b = half_line / 2),
        vjust = 0
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = font_size),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0, size = font_size),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_rect(color = "black", size = line_size),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = font_size_small,
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = font_size_large,
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = font_size_small,
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = font_size_tiny,
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}


#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_family Font family for plot title, axis titles and labels, legend texts, etc.
#' @param line_size Line size for axis lines.
#' @param rel_small Relative size of small text (e.g., axis tick labels)
#' @param rel_tiny Relative size of tiny text (e.g., caption)
#' @param rel_large Relative size of large text (e.g., title)
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_thesis_cor_matrix <- function(font_size = 8,
                                    font_family = "",
                                    line_size = .5,
                                    rel_small = 12/14,
                                    rel_tiny = 11/14,
                                    rel_large = 16/14) {
  line_size_orig <- line_size
  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2
  small_size <- rel_small * font_size

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black", size = line_size, linetype = 1, lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA, size = line_size, linetype = 1),
      text              = element_text(family = font_family, face = "plain", color = "black",
                                       size = font_size, hjust = 0.5, vjust = 0.5, angle = 0, lineheight = .9,
                                       margin = margin(), debug = FALSE),

      axis.line         = element_blank(), #element_line(color = "black", size = line_size, lineend = "square"),
      axis.line.x       = NULL,
      axis.line.y       = NULL,
      axis.text         = element_blank(),
      # axis.text.x       = element_text(margin = margin(t = small_size / 4), vjust = 1),
      # axis.text.x.top   = element_text(margin = margin(b = small_size / 4), vjust = 0),
      #axis.text.y       = element_blank(),
      # axis.text.y.right = element_text(margin = margin(l = small_size / 4), hjust = 0),
      axis.ticks        = element_blank(),
      axis.ticks.length = unit(0, "pt"),
      axis.title = element_blank(),
      # axis.title.x      = element_text(
      #   margin = margin(t = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.x.top  = element_text(
      #   margin = margin(b = half_line / 2),
      #   vjust = 0
      # ),
      # axis.title.y      = element_text(
      #   angle = 90,
      #   margin = margin(r = half_line / 2),
      #   vjust = 1
      # ),
      # axis.title.y.right = element_text(
      #   angle = -90,
      #   margin = margin(l = half_line / 2),
      #   vjust = 0
      # ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = rel(rel_small)),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0),
      legend.title.align = NULL,
      legend.position   = "none",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_rect(color = "black", size = line_size_orig),
      panel.grid        = element_blank(),
      # panel.grid.major  = NULL,
      # panel.grid.minor  = NULL,
      # panel.grid.major.x = NULL,
      # panel.grid.major.y = NULL,
      # panel.grid.minor.x = NULL,
      # panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = rel(rel_small),
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = rel(rel_large),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = rel(rel_small),
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = rel(rel_tiny),
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin = unit(c(0, 0, 0, 0), "null"),

      complete = TRUE
    )
}



#' Create a theme for half open plots for Hjv project
#'
#'
#' @param font_size Overall font size.
#' @param font_size_small Relative size of small text (e.g., axis tick labels)
#' @param font_size_tiny Relative size of tiny text (e.g., caption)
#' @param font_size_large Relative size of large text (e.g., title)
#' @param font_family Font family for plot title, axis titles and labels,
#' legend texts, etc.
#' @param line_size Line size for axis lines.
#'
#' @return The theme.
#' @export
#'
#' @importFrom ggplot2 margin theme_grey theme %+replace%
#' @importFrom grid unit
#'
theme_epitope_peptides_plot <- function(font_size = 8,
                                        font_size_small = 7,
                                        font_size_tiny = 6,
                                        font_size_large = 10,
                                        font_family = "",
                                        line_size = .5) {


  line_size <- line_size / (ggplot2::.pt * 72.27 / 96)
  half_line <- font_size / 2

  # work off of theme_grey just in case some new theme element comes along
  theme_grey(base_size = font_size, base_family = font_family) %+replace%
    theme(
      line              = element_line(color = "black",
                                       size = line_size,
                                       linetype = 1,
                                       lineend = "butt"),
      rect              = element_rect(fill = NA, color = NA,
                                       size = line_size,
                                       linetype = 1),
      text              = element_text(family = font_family,
                                       face = "plain",
                                       color = "black",
                                       size = font_size,
                                       hjust = 0.5,
                                       vjust = 0.5,
                                       angle = 0,
                                       lineheight = .9,
                                       margin = margin(),
                                       debug = FALSE),

      axis.line         = element_line(color = "black",
                                       size = line_size,
                                       lineend = "square"),
      axis.line.x       = element_blank(),
      axis.line.y       = NULL,
      axis.text         = element_text(color = "black", size = font_size_small),
      axis.text.x       = element_text(margin = margin(t = font_size_small / 4),
                                       vjust = 1),
      axis.text.x.top   = element_text(margin = margin(b = font_size_small / 4),
                                       vjust = 0),
      axis.text.y       = element_text(margin = margin(r = font_size_small / 4),
                                       hjust = 1),
      axis.text.y.right = element_text(margin = margin(l = font_size_small / 4),
                                       hjust = 0),
      axis.ticks        = element_line(color = "black", size = line_size),
      axis.ticks.length = unit(half_line / 2, "pt"),
      axis.title.x      = element_text(
        margin = margin(t = half_line / 2),
        vjust = 1
      ),
      axis.title.x.top  = element_text(
        margin = margin(b = half_line / 2),
        vjust = 0
      ),
      axis.title.y      = element_text(
        angle = 90,
        margin = margin(r = half_line / 2),
        vjust = 1
      ),
      axis.title.y.right = element_text(
        angle = -90,
        margin = margin(l = half_line / 2),
        vjust = 0
      ),


      legend.background = element_blank(),
      legend.spacing    = unit(font_size, "pt"),
      legend.spacing.x  = NULL,
      legend.spacing.y  = NULL,
      legend.margin     = margin(0, 0, 0, 0),
      legend.key        = element_blank(),
      legend.key.size   = unit(1.1 * font_size, "pt"),
      legend.key.height = NULL,
      legend.key.width  = NULL,
      legend.text       = element_text(size = font_size_small),
      legend.text.align = NULL,
      legend.title      = element_text(hjust = 0, size = font_size_small),
      legend.title.align = NULL,
      legend.position   = "right",
      legend.direction  = NULL,
      legend.justification = c("left", "center"),
      legend.box        = NULL,
      legend.box.margin =  margin(0, 0, 0, 0),
      legend.box.background = element_blank(),
      legend.box.spacing = unit(font_size, "pt"),

      panel.background  = element_blank(),
      panel.border      = element_blank(),
      panel.grid        = element_blank(),
      panel.grid.major  = NULL,
      panel.grid.minor  = NULL,
      panel.grid.major.x = NULL,
      panel.grid.major.y = NULL,
      panel.grid.minor.x = NULL,
      panel.grid.minor.y = NULL,
      panel.spacing     = unit(half_line, "pt"),
      panel.spacing.x   = NULL,
      panel.spacing.y   = NULL,
      panel.ontop       = FALSE,

      strip.background  = element_rect(fill = "grey80"),
      strip.text        = element_text(
        size = font_size_small,
        margin = margin(half_line / 2, half_line / 2,
                        half_line / 2, half_line / 2)
      ),
      strip.text.x      = NULL,
      strip.text.y      = element_text(angle = -90),
      strip.placement   = "inside",
      strip.placement.x =  NULL,
      strip.placement.y =  NULL,
      strip.switch.pad.grid = unit(half_line / 2, "pt"),
      strip.switch.pad.wrap = unit(half_line / 2, "pt"),

      plot.background   = element_blank(),
      plot.title        = element_text(
        face = "bold",
        size = font_size_large,
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.subtitle     = element_text(
        size = font_size_small,
        hjust = 0, vjust = 1,
        margin = margin(b = half_line)
      ),
      plot.caption      = element_text(
        size = font_size_tiny,
        hjust = 1, vjust = 1,
        margin = margin(t = half_line)
      ),
      plot.tag           = element_text(
        face = "bold",
        hjust = 0, vjust = 0.7
      ),
      plot.tag.position = c(0, 1),
      plot.margin       = margin(half_line, half_line, half_line, half_line),

      complete = TRUE
    )
}

