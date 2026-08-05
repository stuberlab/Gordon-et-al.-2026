# r_plots.R
# Plotting functions sourced from tidy_lab_tools/functions/r_plots.R.
# Functions already present in theme.R (theme_ag01, theme_ag_raster,
# remove_x_all, remove_y_all, save_pdf, add_scale_bar, alpha_transform,
# geom_hpline/GeomHpline, plt_string_facet, plt_manual_dims,
# plt_manual_scale_cartesian) and stats.R (plt_hsd_matrix) are omitted here
# to avoid duplication.

# ── Additional themes ──────────────────────────────────────────────────────────


plt_raster <- function(df, x, y, id, facet_x, facet_y, xlab, ylab,
                       plt_y_lims, plt_x_lims, plt_dims, var_color, plt_scale_color) {
  plt <- df %>%
    ungroup() %>%
    mutate(event_unique = row_number()) %>%
    select(na.omit(c(facet_y, facet_x, x, var_color, 'event_unique', y))) %>%
    mutate(y_dummy = !!as.name(y) + 1) %>%
    gather('id', 'y_pos', !!as.name(y):y_dummy) %>%
    ggplot(aes(event_ts_rel, y_pos, group = event_unique))

  if ( is.na(var_color)) { plt <- plt + geom_line(size = 0.25) }
  if (!is.na(var_color)) { plt <- plt + geom_line(size = 0.25, aes(color = !!as.name(var_color))) }

  if (!is.na(plt_scale_color) & !is.na(var_color)) {
    if (plt_scale_color == 'mako')    { plt <- plt + scale_color_viridis_d(option = 'mako',    end = 0.9) }
    if (plt_scale_color == 'inferno') { plt <- plt + scale_color_viridis_d(option = 'inferno', end = 0.9) }
    if (plt_scale_color == 'viridis') { plt <- plt + scale_color_viridis_d(option = 'viridis', end = 0.9) }
  }

  if (!is.na(facet_x) & !is.na(facet_y)) { plt <- plt + facet_grid(eval(expr(!!ensym(facet_y) ~ !!ensym(facet_x)))) }
  if (!is.na(facet_x) &  is.na(facet_y)) { plt <- plt + facet_grid(eval(expr(. ~ !!ensym(facet_x)))) }
  if ( is.na(facet_x) & !is.na(facet_y)) { plt <- plt + facet_grid(eval(expr(!!ensym(facet_y) ~ .))) }

  plt +
    theme_ag01() +
    coord_cartesian(ylim = plt_y_lims, xlim = plt_x_lims, expand = FALSE) +
    force_panelsizes(rows = unit(plt_dims[1], "cm"), cols = unit(plt_dims[2], "cm")) +
    xlab(xlab) +
    ylab(ylab)
}

plt_heatmap_trial_split <- function(df, var_x, var_fill, var_facet, var_trial,
                                    limits_fill, bin_seq, plt_dim, return_df) {
  if (!hasArg(return_df)) { return_df <- 0 }

  df_split <- df %>%
    mutate(trial_split = cut(!!as.name(var_trial), bin_seq, label = bin_seq[1:length(bin_seq) - 1])) %>%
    group_by_at(na.omit(c('trial_split', var_x, var_facet))) %>%
    summarise_at(vars(var_fill), mean) %>%
    mutate(trial_split = trial_split %>% as.character() %>% as.double()) %>%
    ungroup() %>%
    mutate(trial_split = trial_split + (bin_seq[2] / 2)) %>%
    filter(!is.na(!!as.name(var_x)))

  var_x_count <- df_split %>% select(!!as.name(var_x)) %>% unique() %>% nrow()

  plt <- df_split %>%
    ggplot(aes_string(x = var_x, y = 'trial_split', fill = var_fill)) +
    geom_tile() +
    theme_ag01() +
    coord_cartesian(ylim = c(0, max(bin_seq)), xlim = c(0.5, var_x_count + 0.5), expand = FALSE) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.3, hjust = 1)) +
    ylab('Trial Bin')

  if (sum(!is.na(limits_fill)) == 0) {
    plt <- plt + scale_fill_continuous(low = 'black', high = 'white', oob = scales::squish)
  } else {
    plt <- plt + scale_fill_continuous(low = 'black', high = 'white', limits = limits_fill, oob = scales::squish)
  }

  if (sum(!is.na(var_facet)) > 0) {
    plt <- plt + facet_grid(as.formula(paste(".~", var_facet)))
  }
  plt <- plt %>% plt_manual_dims(plt_dim)

  if (return_df) { return(list(plt, df_split)) } else { return(plt) }
}

plt_mean_trial_split <- function(df, var_y, var_facet, var_trial,
                                 limits_fill, bin_seq, plt_dim, return_df) {
  if (!hasArg(return_df)) { return_df <- 0 }

  df_split <- df %>%
    mutate(trial_split = cut(!!as.name(var_trial), bin_seq, label = bin_seq[1:length(bin_seq) - 1])) %>%
    group_by_at(na.omit(c('trial_split', 'subject', var_facet))) %>%
    summarise_at(vars(var_y), mean) %>%
    mutate(trial_split = trial_split %>% as.character() %>% as.double()) %>%
    ungroup() %>%
    mutate(trial_split = trial_split + (bin_seq[2] / 2))

  plt <- df_split %>%
    ggplot(aes_string(x = 'trial_split', y = var_y)) +
    geom_line(alpha = 1/3, size = 0.25, aes(group = subject)) +
    stat_summary(fun = 'mean', geom = 'line', aes(group = 1), size = 0.25) +
    stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0, aes(group = 1), size = 0.25) +
    theme_ag01() +
    coord_cartesian(xlim = c(0, max(bin_seq)), expand = FALSE) +
    xlab('Trial Bin')

  if (sum(!is.na(limits_fill)) == 0) {
    plt <- plt + scale_fill_continuous(low = 'black', high = 'white', oob = scales::squish)
  } else {
    plt <- plt + scale_fill_continuous(low = 'black', high = 'white', limits = limits_fill, oob = scales::squish)
  }

  if (sum(!is.na(var_facet)) > 0) {
    plt <- plt + facet_grid(as.formula(paste(".~", var_facet)))
  }
  plt <- plt %>% plt_manual_dims(plt_dim)

  if (return_df) { return(list(plt, df_split)) } else { return(plt) }
}

