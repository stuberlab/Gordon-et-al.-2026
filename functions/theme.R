# theme.R
# ggplot2 theme, plotting utilities, and stats output helpers used across all
# figure scripts.  These were previously sourced from two external lab-tools
# repos (tidy_lab_tools/functions/r_plots.R and r_stats.R).  Bringing them
# in-house makes the repo fully self-contained.
#
# Source this file after loading libraries (ggplot2, tidyverse, ggh4x, broom,
# afex, emmeans, janitor are assumed to be loaded).

# ── ggplot2 themes ─────────────────────────────────────────────────────────────

theme_ag01 <- function(general_font_size = NULL) {
  aes.axis_line_size       <- 0.25
  aes.axis_font_size       <- if (!is.null(general_font_size)) general_font_size else 8
  aes.axis_title_font_size <- if (!is.null(general_font_size)) general_font_size else 8
  aes.title_font_size      <- if (!is.null(general_font_size)) general_font_size else 8
  base_size                <- if (!is.null(general_font_size)) general_font_size else 10

  theme_bw(base_size = base_size) %+replace%
    theme(
      axis.line         = element_line(colour = "black", size = aes.axis_line_size),
      axis.ticks        = element_line(size = aes.axis_line_size),
      axis.ticks.length = unit(1, "mm"),
      axis.title        = element_text(color = "black", size = aes.axis_title_font_size),
      axis.text.x       = element_text(color = "black", size = aes.axis_font_size, hjust = 0.5),
      axis.text.y       = element_text(color = "black", size = aes.axis_font_size, hjust = 1, vjust = 0.3, margin = margin(r = 1)),
      legend.title      = element_text(color = "black", size = aes.axis_title_font_size),
      legend.text       = element_text(color = "black", size = aes.axis_font_size),
      plot.title        = element_text(color = "black", size = aes.title_font_size, hjust = 0.5),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      panel.border      = element_blank(),
      panel.background  = element_blank(),
      strip.background  = element_blank(),
      strip.text        = element_text(color = 'black', size = aes.axis_title_font_size)
    )
}

theme_ag_raster <- function() {
  aes.axis_line_size       <- 0.25
  aes.axis_font_size       <- 8
  aes.axis_title_font_size <- 8
  aes.title_font_size      <- 8

  theme_bw(base_size = 10) %+replace%
    theme(
      axis.ticks        = element_line(size = aes.axis_line_size),
      axis.title        = element_text(color = "black", size = aes.axis_title_font_size),
      legend.title      = element_text(color = "black", size = aes.axis_title_font_size),
      legend.text       = element_text(color = "black", size = aes.axis_font_size),
      plot.title        = element_text(color = "black", size = aes.title_font_size, hjust = 0.5, face = "bold"),
      panel.border      = element_blank(),
      panel.background  = element_blank(),
      strip.background  = element_blank(),
      strip.text        = element_text(color = 'black', size = aes.axis_title_font_size),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.line.x        = element_blank(),
      axis.title.x       = element_blank(),
      axis.text.x        = element_blank(),
      axis.ticks.x       = element_blank()
    )
}

# ── Axis helpers ───────────────────────────────────────────────────────────────

remove_x_all <- function(plt) {
  plt +
    theme(
      axis.text.x  = element_blank(),
      axis.line.x  = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.x = element_blank()
    )
}

remove_y_all <- function(plt) {
  plt +
    theme(
      axis.text.y  = element_blank(),
      axis.line.y  = element_blank(),
      axis.ticks.y = element_blank(),
      axis.title.y = element_blank()
    )
}

# ── Saving ─────────────────────────────────────────────────────────────────────

save_pdf <- function(dir, fn, w, h) {
  ggsave(
    filename  = str_c(dir, fn),
    device    = NULL,
    path      = NULL,
    scale     = 1,
    width     = w,
    height    = h,
    units     = "in",
    dpi       = 300,
    useDingbats = FALSE
  )
}

# ── Plot layout helpers ────────────────────────────────────────────────────────

plt_string_facet <- function(plt, var_facet_y, var_facet_x, scales, facet_spacing) {
  if (!is.na(var_facet_y) & !is.na(var_facet_x)) {
    plt <- plt + facet_grid(as.formula(str_c(var_facet_y, '~', var_facet_x)), scales = scales) +
      theme(panel.spacing = unit(facet_spacing, "lines"))
  }
  if (!is.na(var_facet_y) &  is.na(var_facet_x)) {
    plt <- plt + facet_grid(as.formula(str_c(var_facet_y, '~.')), scales = scales) +
      theme(panel.spacing = unit(facet_spacing, "lines"))
  }
  if ( is.na(var_facet_y) & !is.na(var_facet_x)) {
    plt <- plt + facet_grid(as.formula(str_c('.~', var_facet_x)), scales = scales) +
      theme(panel.spacing = unit(facet_spacing, "lines"))
  }
  return(plt)
}

plt_manual_dims <- function(plt, plt_dims) {
  if (length(plt_dims) > 1) {
    plt <- plt +
      force_panelsizes(
        rows = unit(plt_dims[1], "cm"),
        cols = unit(plt_dims[2], "cm")
      )
  }
  return(plt)
}

plt_manual_scale_cartesian <- function(plt, plt_manual_scale_x, plt_manual_scale_y) {
  if (sum(is.na(plt_manual_scale_x) == 0) & sum(is.na(plt_manual_scale_y) == 0)) {
    plt <- plt + coord_cartesian(xlim = plt_manual_scale_x, ylim = plt_manual_scale_y, expand = FALSE, clip = 'off')
  }
  if (sum(is.na(plt_manual_scale_x) == 0) & !sum(is.na(plt_manual_scale_y) == 0)) {
    plt <- plt + coord_cartesian(xlim = plt_manual_scale_x, expand = FALSE, clip = 'off')
  }
  if (!sum(is.na(plt_manual_scale_x) == 0) & sum(is.na(plt_manual_scale_y) == 0)) {
    plt <- plt + coord_cartesian(ylim = plt_manual_scale_y, expand = FALSE, clip = 'off')
  }
  return(plt)
}

add_scale_bar <- function(plt, origin, x_len, y_len) {
  scale_bar_x <- data.frame(x = c(origin[1], origin[1] + x_len), y = c(origin[2], origin[2]), facet = factor(1))
  scale_bar_y <- data.frame(x = c(origin[1], origin[1]),         y = c(origin[2], origin[2] + y_len), facet = factor(1))
  plt <- plt +
    geom_path(data = scale_bar_x, aes(x = x, y = y), color = "black", size = 0.5, inherit.aes = FALSE) +
    geom_path(data = scale_bar_y, aes(x = x, y = y), color = "black", size = 0.5, inherit.aes = FALSE)
  plt %>% remove_x_all() %>% remove_y_all()
}

# ── Custom geoms ───────────────────────────────────────────────────────────────
# Horizontal point-line geom (sourced from wilkelab/ungeviz)

geom_hpline <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  layer(
    data = data, mapping = mapping, stat = stat, geom = GeomHpline,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

GeomHpline <- ggproto("GeomHpline", GeomSegment,
  required_aes    = c("x", "y"),
  non_missing_aes = c("size", "colour", "linetype", "width"),
  default_aes     = aes(width = 0.5, colour = "black", size = 2, linetype = 1, alpha = NA),
  draw_panel = function(self, data, panel_params, coord,
                        arrow = NULL, arrow.fill = NULL,
                        lineend = "butt", linejoin = "round", na.rm = FALSE) {
    data <- mutate(data, x = x - width / 2, xend = x + width, yend = y)
    ggproto_parent(GeomSegment, self)$draw_panel(
      data, panel_params, coord,
      arrow = arrow, arrow.fill = arrow.fill,
      lineend = lineend, linejoin = linejoin, na.rm = na.rm
    )
  }
)

# ── Stats output helpers ───────────────────────────────────────────────────────
# These were previously sourced from tidy_lab_tools/functions/r_stats.R.

# Append significance labels to a dataframe with a p_value column.
quick_sig <- function(stats_df) {
  stats_df %>%
    mutate(
      sig_text   = ifelse(p_value < 0.05, 'sig', ''),
      sig_symbol = ifelse(p_value < 0.001, '***',
                   ifelse(p_value < 0.01,  '**',
                   ifelse(p_value < 0.05,  '*', 'n.s.')))
    )
}

# Write ANOVA summary, pairwise HSD, and raw data to CSV files.
save_aov <- function(dir_output, prefix, anova_summary, pairs_hsd, df) {
  if (!is.na(dir_output)) {
    fn <- str_c(dir_output, '/', prefix, '_aov.csv')
    anova_summary %>% write_csv(fn)
    print(str_c('saved file: ', fn))

    fn <- str_c(dir_output, '/', prefix, '_aov_hsd.csv')
    pairs_hsd %>% write_csv(fn)
    print(str_c('saved file: ', fn))

    fn <- str_c(dir_output, '/', prefix, '_data.csv')
    df %>% write_csv(fn)
    print(str_c('saved file: ', fn))
  }
}

save_ttest <- function(dir_output, prefix, stats_result) {
  if (!is.na(dir_output)) {
    fn <- str_c(dir_output, '/', prefix, '_ttest.csv')
    stats_result %>% write_csv(fn)
    print(str_c('saved file: ', fn))
  }
}
