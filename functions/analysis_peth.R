# analysis_peth.R
# functions for combining session-level fp and behavioral data and computing
# peri-event time histogram (peth) summaries.
#
# pipeline usage: sourced by 02_aggregate.Rmd
# plotting functions (plt_*) are used by figure scripts, not the pipeline

# generic loader: stacks a single csv or feather file across sessions
combine_session_files <- function(process_blocknames, dir_localdata_sessions, fn_suffix) {

  for(process_blockname in process_blocknames){
    dir_session_process <- str_c(dir_localdata_sessions, process_blockname, '/')

    if(fn_suffix %>% str_detect('.csv')){
      df <- read.csv(str_c(dir_session_process, process_blockname, fn_suffix))
    } else if(fn_suffix %>% str_detect('.feather')){
      df <- read_feather(str_c(dir_session_process, process_blockname, fn_suffix))
    }

    if(process_blockname == process_blocknames[1]){
      df_combined <- df
    } else {
      df_combined <- df %>% bind_rows(df_combined, .)
    }
  }

  return(df_combined)
}


# loads peth feathers and joins trial-level lick info; drops unused signal columns for memory
combine_session_fp <- function(process_blocknames, dir_localdata_sessions) {
  for(process_blockname in process_blocknames){
    dir_session_process <- str_c(dir_localdata_sessions, process_blockname, '/')

    df <- read_feather(str_c(dir_session_process, process_blockname, '_streams_peth_preprocessed.feather')) %>%
      filter(event_id_char %in% c('access_period', 'spout_extended')) %>%
      rename(trial_num = event_number) %>%
      select(any_of(c("blockname", "region", "trial_num", "time_rel",
                      "delta_signal_poly", "zscore", "zscore_blsub")))

    df_trial_info <- read.csv(str_c(dir_session_process, process_blockname, '_data_trial_summary.csv')) %>%
      select(blockname, trial_num, spout, solution, trial_lick, lick_count, lick_ts_first, lick_ts_last) %>%
      mutate(solution_previous = lag(solution))

    df <- df %>%
      left_join(df_trial_info, by = join_by(blockname, trial_num))

    if(process_blockname == process_blocknames[1]){
      df_combined <- df
    } else {
      df_combined <- df %>% bind_rows(df_combined, .)
    }
  }

  return(df_combined)
}

# loads 100 ms binned lick counts and joins trial_lick flag and solution_previous
combine_session_binnedlick <- function(process_blocknames, dir_localdata_sessions) {

  for(process_blockname in process_blocknames){
    dir_session_process <- str_c(dir_localdata_sessions, process_blockname, '/')

    df <- read.csv(str_c(dir_session_process, process_blockname, '_data_trial_binned.csv'))

    df_trial_info <- read.csv(str_c(dir_session_process, process_blockname, '_data_trial_summary.csv')) %>%
      mutate(solution_previous = lag(solution)) %>%
      select(blockname, trial_num, trial_lick, solution_previous)

    df <- df %>%
      left_join(df_trial_info, by = join_by(blockname, trial_num))

    if(process_blockname == process_blocknames[1]){
      df_combined <- df
    } else {
      df_combined <- df %>% bind_rows(df_combined, .)
    }
  }

  return(df_combined)
}

# loads preprocessed baseline feathers across sessions
combine_session_fp_bl <- function(process_blocknames, dir_localdata_sessions) {

  for(process_blockname in process_blocknames){
    dir_session_process <- str_c(dir_localdata_sessions, process_blockname, '/')

    df <- read_feather(str_c(dir_session_process, process_blockname, '_streams_baseline_preprocessed.feather')) %>%
      select(any_of(c("blockname", "region", "time", "signal", "delta_signal_poly", "zscore", "zscore_blsub")))

    if(process_blockname == process_blocknames[1]){
      df_combined <- df
    } else {
      df_combined <- df %>% bind_rows(df_combined, .)
    }
  }

  return(df_combined)
}


get_mean_signals <- function(df) {
  df %>%
    summarise(
      delta_signal_poly_sem  = sd(delta_signal_poly) / sqrt(n()),
      delta_signal_poly_mean = delta_signal_poly %>% mean(),
      zscore_sem             = sd(zscore) / sqrt(n()),
      zscore_mean            = zscore %>% mean(),
      zscore_blsub_sem       = sd(zscore_blsub) / sqrt(n()),
      zscore_blsub_mean      = zscore_blsub %>% mean(),
      sample_count           = n(),
      .groups = 'drop'
    )
}


get_peth_binned_summary <- function(df_peth, peth_tm_bins, grouping_vars) {

  for(bin_id in seq(1, nrow(peth_tm_bins))){

    peth_tm_bin <- peth_tm_bins[bin_id, ]

    if(bin_id == 1){
      df_fp_peth_summary <- df_peth %>%
        filter(time_rel > peth_tm_bin$tm_bin_start, time_rel < peth_tm_bin$tm_bin_end) %>%
        mutate(time_bin = peth_tm_bin$tm_bin_id) %>%
        group_by(across(all_of(grouping_vars))) %>%
        get_mean_signals()
    } else {
      df_fp_peth_summary <- df_peth %>%
        filter(time_rel > peth_tm_bin$tm_bin_start, time_rel < peth_tm_bin$tm_bin_end) %>%
        mutate(time_bin = peth_tm_bin$tm_bin_id) %>%
        group_by(across(all_of(grouping_vars))) %>%
        get_mean_signals() %>%
        bind_rows(df_fp_peth_summary, .)
    }
  }

  # append a baseline_subsequent bin: pre-access activity (-3 to 0 s) grouped by
  # the solution that will appear on the *next* trial, allowing cross-trial comparisons
  if(sum('solution' %in% grouping_vars)){
    grouping_vars_previous <- grouping_vars
    grouping_vars_previous[grouping_vars_previous == 'solution'] <- 'solution_previous'

    if(sum('solution_previous' %in% names(df_fp_peth)) == 1){
      df_fp_peth_summary <- df_peth %>%
        filter(time_rel > -3, time_rel < 0) %>%
        mutate(time_bin = 'baseline_subsequent') %>%
        group_by(across(all_of(grouping_vars_previous))) %>%
        get_mean_signals() %>%
        rename(solution = solution_previous) %>%
        filter(!is.na(solution)) %>%
        bind_rows(df_fp_peth_summary, .)
    }
  }

  return(df_fp_peth_summary)
}


plt_hsd_matrix_2contrasts_between <- function(df_pairs, heat_limits = NA) {
  n_contrasts <- df_pairs %>%
    pull(contrast1_between) %>%
    unique() %>%
    length()

  df_pairs <- df_pairs %>%
    rename(contrast1_between_org = contrast1_between,
           contrast2_between_org = contrast2_between) %>%
    rename(contrast2_between = contrast1_between_org,
           contrast1_between = contrast2_between_org) %>%
    mutate(effect_size = -effect_size) %>%
    bind_rows(df_pairs, .)

  if (sum(is.na(heat_limits))) {
    max_abs_d  <- max(abs(df_pairs$effect_size))
    heat_limits <- c(-max_abs_d, max_abs_d)
  }

  df_pairs %>%
    mutate(sig_symbol = ifelse(sig_symbol == 'n.s.', '', sig_symbol)) %>%
    ggplot(aes(contrast2_between, contrast1_between, fill = effect_size, label = sig_symbol)) +
    geom_tile() +
    geom_text() +
    theme_ag01() +
    scale_fill_gradientn(colors = c('#0016EC', 'white', '#EC008C'), limits = heat_limits, oob = scales::squish) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.3)
    ) +
    coord_cartesian(xlim = c(0.5, n_contrasts + 1.5), ylim = c(0.5, n_contrasts + 1.5), expand = F)
}


