# stats_compiler.R
# Compiles all panel-level statistics from analysis_and_plots/ into a single xlsx.
# Run interactively from the project root:
#   source('./stats_compilation/stats_compiler.R')
# Output: ./stats_compilation/stats_compiled.xlsx

library(tidyverse)
library(openxlsx)

# key_regions mirrors the definition in functions/params.R.
# Update here if params.R is changed.
key_regions <- tibble(
  region       = c('lha_gaba', 'lha_glut', 'lha_ratio',
                   'nac',      'nac_corr', 'nacsh_med', 'nac_shelllat', 'nacsh_lat',
                   'dms', 'dls', 'ts'),
  region_label = c('LHA:GABA', 'LHA:Glut', 'LHA:Ratio',
                   'NAcCR',    'NAcCC',    'NAcShM',    'NAcShL',       'NAcShL',
                   'DMS', 'DLS', 'TS')
)

dir_aap <- './analysis_and_plots/'
dir_out <- './stats_compilation/'
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

wb <- createWorkbook()


# ── helpers ───────────────────────────────────────────────────────────────────

safe_read <- function(path) {
  if (!file.exists(path)) { warning('missing: ', path); return(NULL) }
  read.csv(path, check.names = FALSE)
}

# Prepend a constant column to a data frame (base R; compatible with dplyr < 1.0)
prepend_col <- function(df, col_name, col_val) {
  if (is.null(df)) return(NULL)
  df[[col_name]] <- col_val
  df[c(col_name, setdiff(names(df), col_name))]
}

# Read and combine a named character vector of file paths.
# Names become the leading ID column values.
combine_named <- function(files, id_col = 'id') {
  map_dfr(names(files), function(id) {
    df <- safe_read(files[[id]])
    prepend_col(df, id_col, id)
  })
}

# Columns that may contain internal region codes (key_regions$region).
# Other region-like columns (comp1/comp2, region_stim) already use display labels.
region_code_cols <- c('region', 'contrast1_within', 'contrast2_within')

# For one column that holds internal region codes: filter to key regions and
# insert a *_label column immediately after it (base R; dplyr < 1.0 compatible).
decode_region_col <- function(df, col) {
  lut <- key_regions %>% select(region, region_label) %>% distinct()
  label_col <- if (col == 'region') 'region_label' else str_c(col, '_label')
  names(lut)[names(lut) == 'region']        <- col
  names(lut)[names(lut) == 'region_label']  <- label_col
  df <- left_join(df, lut, by = col)
  col_pos   <- which(names(df) == col)
  rest      <- setdiff(names(df), label_col)
  df[c(rest[seq_len(col_pos)], label_col, rest[seq(col_pos + 1, length(rest))])]
}

# For any recognised column that contains internal region codes: filter rows to
# key_regions and decode to label. Columns with display labels are unchanged.
filter_decode_regions <- function(df) {
  if (is.null(df)) return(NULL)
  for (col in intersect(region_code_cols, names(df))) {
    if (any(df[[col]] %in% key_regions$region, na.rm = TRUE)) {
      df <- df[df[[col]] %in% key_regions$region, , drop = FALSE]
      df <- decode_region_col(df, col)
    }
  }
  df
}

add_sheet <- function(wb, sheet_name, df) {
  df <- filter_decode_regions(df)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet = sheet_name, df)
  invisible(wb)
}

# Header index: one row per xlsx sheet
header_rows <- list()

record <- function(figure, panel, test_description, stratified_by, sheet_name) {
  header_rows[[length(header_rows) + 1]] <<- tibble(
    figure_id        = figure,
    panel_id         = panel,
    statistical_test = test_description,
    stratified_by    = stratified_by,
    sheet_name       = sheet_name
  )
}


# ── fig01 ─────────────────────────────────────────────────────────────────────
# Panel e — licking (rmANOVA, solution × set_id)

add_sheet(wb, 'fig01e_1way_aov',
  safe_read(str_c(dir_aap, 'fig01/e/solution_vs_licking_stats_aov_oneway_main.csv')))
record('fig01', 'e', 'rmANOVA 1-within (solution)', 'set_id', 'fig01e_1way_aov')

add_sheet(wb, 'fig01e_1way_aov_ph',
  safe_read(str_c(dir_aap, 'fig01/e/solution_vs_licking_stats_aov_oneway_ph.csv')))
record('fig01', 'e', 'rmANOVA 1-within posthoc (Tukey HSD)', 'set_id', 'fig01e_1way_aov_ph')

add_sheet(wb, 'fig01e_2way_aov',
  safe_read(str_c(dir_aap, 'fig01/e/solution_vs_licking_stats_aov_twoway_main.csv')))
record('fig01', 'e', 'rmANOVA 2-within (solution × set_id)', 'none', 'fig01e_2way_aov')

add_sheet(wb, 'fig01e_2way_aov_ph',
  safe_read(str_c(dir_aap, 'fig01/e/solution_vs_licking_stats_aov_twoway_ph.csv')))
record('fig01', 'e', 'rmANOVA 2-within posthoc (Tukey HSD)', 'none', 'fig01e_2way_aov_ph')

# Panel j — LHA sustained (rmANOVA × solution, per set_id; ratio / GABA+Glut)
add_sheet(wb, 'fig01j_aov_ratio',
  combine_named(c(
    'WR:Suc'  = str_c(dir_aap, 'fig01/j/fp_lha_sustained_wrsuc_ratio_aov.csv'),
    'FR:Suc'  = str_c(dir_aap, 'fig01/j/fp_lha_sustained_frsuc_ratio_aov.csv'),
    'WR:NaCl' = str_c(dir_aap, 'fig01/j/fp_lha_sustained_wrnacl_ratio_aov.csv')
  ), id_col = 'set_id'))
record('fig01', 'j', 'rmANOVA 1-within (solution, LHA:Ratio)', 'set_id', 'fig01j_aov_ratio')

add_sheet(wb, 'fig01j_aov_ratio_hsd',
  combine_named(c(
    'WR:Suc'  = str_c(dir_aap, 'fig01/j/fp_lha_sustained_wrsuc_ratio_aov_hsd.csv'),
    'FR:Suc'  = str_c(dir_aap, 'fig01/j/fp_lha_sustained_frsuc_ratio_aov_hsd.csv'),
    'WR:NaCl' = str_c(dir_aap, 'fig01/j/fp_lha_sustained_wrnacl_ratio_aov_hsd.csv')
  ), id_col = 'set_id'))
record('fig01', 'j', 'rmANOVA 1-within posthoc (Tukey HSD, LHA:Ratio)', 'set_id', 'fig01j_aov_ratio_hsd')

# GABA/Glut/interaction tests (p-corrected) per set_id
add_sheet(wb, 'fig01j_aov_gabaglut',
  combine_named(c(
    'WR:Suc'  = str_c(dir_aap, 'fig01/j/fp_lha_sustained_wrsuc_gabaglut.csv'),
    'FR:Suc'  = str_c(dir_aap, 'fig01/j/fp_lha_sustained_frsuc_gabaglut.csv'),
    'WR:NaCl' = str_c(dir_aap, 'fig01/j/fp_lha_sustained_wrnacl_gabaglut.csv')
  ), id_col = 'set_id'))
record('fig01', 'j', 'rmANOVA (GABA / Glut / region x solution interaction, p-corrected)',
       'set_id', 'fig01j_aov_gabaglut')

# Panel k — LHA post (same structure as j)
add_sheet(wb, 'fig01k_aov_ratio',
  combine_named(c(
    'WR:Suc'  = str_c(dir_aap, 'fig01/k/fp_lha_post_wrsuc_ratio_aov.csv'),
    'FR:Suc'  = str_c(dir_aap, 'fig01/k/fp_lha_post_frsuc_ratio_aov.csv'),
    'WR:NaCl' = str_c(dir_aap, 'fig01/k/fp_lha_post_wrnacl_ratio_aov.csv')
  ), id_col = 'set_id'))
record('fig01', 'k', 'rmANOVA 1-within (solution, LHA:Ratio)', 'set_id', 'fig01k_aov_ratio')

add_sheet(wb, 'fig01k_aov_ratio_hsd',
  combine_named(c(
    'WR:Suc'  = str_c(dir_aap, 'fig01/k/fp_lha_post_wrsuc_ratio_aov_hsd.csv'),
    'FR:Suc'  = str_c(dir_aap, 'fig01/k/fp_lha_post_frsuc_ratio_aov_hsd.csv'),
    'WR:NaCl' = str_c(dir_aap, 'fig01/k/fp_lha_post_wrnacl_ratio_aov_hsd.csv')
  ), id_col = 'set_id'))
record('fig01', 'k', 'rmANOVA 1-within posthoc (Tukey HSD, LHA:Ratio)', 'set_id', 'fig01k_aov_ratio_hsd')

add_sheet(wb, 'fig01k_aov_gabaglut',
  combine_named(c(
    'WR:Suc'  = str_c(dir_aap, 'fig01/k/fp_lha_post_wrsuc_gabaglut.csv'),
    'FR:Suc'  = str_c(dir_aap, 'fig01/k/fp_lha_post_frsuc_gabaglut.csv'),
    'WR:NaCl' = str_c(dir_aap, 'fig01/k/fp_lha_post_wrnacl_gabaglut.csv')
  ), id_col = 'set_id'))
record('fig01', 'k', 'rmANOVA (GABA / Glut / region x solution interaction, p-corrected)',
       'set_id', 'fig01k_aov_gabaglut')


# ── fig02 / sfig04 ────────────────────────────────────────────────────────────
# Panel c — opto stim bar (Wilcoxon vs. 0, initial and sustained epochs combined)

add_sheet(wb, 'fig02c_wilcox', {
  d1 <- prepend_col(
    safe_read(str_c(dir_aap, 'fig02_sfig04/c/opto_stim_20hz_region_bar_initial_stats.csv')),
    'epoch', 'initial')
  d2 <- prepend_col(
    safe_read(str_c(dir_aap, 'fig02_sfig04/c/opto_stim_20hz_region_bar_sustained_stats.csv')),
    'epoch', 'sustained')
  bind_rows(d1, d2)
})
record('fig02', 'c', 'Wilcoxon signed-rank vs. 0 (per region x group)', 'epoch', 'fig02c_wilcox')

# Panel e — placement x stim response (Pearson correlation)
add_sheet(wb, 'fig02e_cor',
  safe_read(str_c(dir_aap, 'fig02_sfig04/e/placement_vs_zscore_sustained_corr_stats.csv')))
record('fig02', 'e', 'Pearson correlation (placement x stim response, sustained)',
       'none', 'fig02e_cor')

# Panel h — opto inhibition bar (Wilcoxon vs. 0, sustained)
add_sheet(wb, 'fig02h_wilcox',
  safe_read(str_c(dir_aap, 'fig02_sfig04/h/opto_inh_region_bar_sustained_stats.csv')))
record('fig02', 'h', 'Wilcoxon signed-rank vs. 0 (per region x group, sustained)',
       'none', 'fig02h_wilcox')

# Panel j — placement x inhibition response (Pearson correlation)
add_sheet(wb, 'fig02j_cor',
  safe_read(str_c(dir_aap, 'fig02_sfig04/j/placement_vs_zscore_sustained_corr_stats.csv')))
record('fig02', 'j', 'Pearson correlation (placement x inhibition response, sustained)',
       'none', 'fig02j_cor')


# ── fig03 ─────────────────────────────────────────────────────────────────────
# Panel e — freq x region heat (Friedman, per stim_region x region pair)

add_sheet(wb, 'fig03e_friedman',
  safe_read(str_c(dir_aap, 'fig03/e/heat_frequency_vs_region_stats.csv')))
record('fig03', 'e', 'Friedman rank sum test (frequency x zscore, per region pair)',
       'stim_region x recorded_region', 'fig03e_friedman')

# Panel g — 20 Hz region x zscore (Friedman + posthoc Wilcoxon)
add_sheet(wb, 'fig03g_friedman',
  safe_read(str_c(dir_aap, 'fig03/g/line_region_vs_zscore_20hz_stats_main.csv')))
record('fig03', 'g', 'Friedman rank sum test (region x zscore, 20 Hz)',
       'stim_region', 'fig03g_friedman')

add_sheet(wb, 'fig03g_friedman_ph',
  safe_read(str_c(dir_aap, 'fig03/g/line_region_vs_zscore_20hz_stats_posthoc.csv')))
record('fig03', 'g', 'Friedman posthoc (pairwise Wilcoxon, per stim_region)',
       'stim_region', 'fig03g_friedman_ph')


# ── fig04 (opto inhibition, acd cohort) ──────────────────────────────────────
# Panel d — stim x mean lick count (rmANOVA)

add_sheet(wb, 'fig04d_aov',
  safe_read(str_c(dir_aap, 'fig04_sfig05/d/stim_vs_lickcount_line_aov.csv')))
record('fig04', 'd', 'rmANOVA (stim x lick count)', 'none', 'fig04d_aov')

add_sheet(wb, 'fig04d_aov_hsd',
  safe_read(str_c(dir_aap, 'fig04_sfig05/d/stim_vs_lickcount_line_aov_hsd.csv')))
record('fig04', 'd', 'rmANOVA posthoc (Tukey HSD)', 'none', 'fig04d_aov_hsd')

# Panel f — region x inhibition difference (Wilcoxon posthoc, pre-laser bl)
add_sheet(wb, 'fig04f_wilcox_ph',
  safe_read(str_c(dir_aap, 'fig04_sfig05/f/inh_access_prelaserbl_region_vs_difference_stats_ph.csv')))
record('fig04', 'f', 'Wilcoxon posthoc (region x inhibition difference, pre-laser bl)',
       'none', 'fig04f_wilcox_ph')

# Panel h — placement x inhibition response (Pearson correlation)
add_sheet(wb, 'fig04h_cor',
  safe_read(str_c(dir_aap, 'fig04_sfig05/h/inh_access_prelaserbl_placement_vs_deltasummary_corr_stats.csv')))
record('fig04', 'h', 'Pearson correlation (placement x inhibition response)',
       'none', 'fig04h_cor')


# ── fig05 (STR PETH) ──────────────────────────────────────────────────────────
# Panel e — lick vs no-lick prelick (paired t-test per region)

add_sheet(wb, 'fig05e_ttest',
  safe_read(str_c(dir_aap, 'fig05/e/fp_str_summary_lickvsnolick_prelick_stats.csv')))
record('fig05', 'e', 't-test paired (lick vs no-lick, prelick, per region)',
       'none', 'fig05e_ttest')

# Panel h — combined sustained (rmANOVA 1-within and 2-within)
add_sheet(wb, 'fig05h_1way_aov',
  safe_read(str_c(dir_aap, 'fig05/h/fp_str_summary_combined_sustained_stats_aov_oneway.csv')))
record('fig05', 'h', 'rmANOVA 1-within (solution, per set_id x region)',
       'none', 'fig05h_1way_aov')

add_sheet(wb, 'fig05h_1way_aov_hsd',
  safe_read(str_c(dir_aap, 'fig05/h/fp_str_summary_combined_sustained_stats_hsd_oneway.csv')))
record('fig05', 'h', 'rmANOVA 1-within posthoc (Tukey HSD)',
       'none', 'fig05h_1way_aov_hsd')

add_sheet(wb, 'fig05h_2way_aov',
  safe_read(str_c(dir_aap, 'fig05/h/fp_str_summary_combined_sustained_stats_aov_interaction.csv')))
record('fig05', 'h', 'rmANOVA 2-within (solution x region interaction)',
       'none', 'fig05h_2way_aov')

add_sheet(wb, 'fig05h_2way_aov_hsd',
  safe_read(str_c(dir_aap, 'fig05/h/fp_str_summary_combined_sustained_stats_hsd_interaction.csv')))
record('fig05', 'h', 'rmANOVA 2-within posthoc (Tukey HSD)',
       'none', 'fig05h_2way_aov_hsd')

# Panel k — combined post (same structure as h)
add_sheet(wb, 'fig05k_1way_aov',
  safe_read(str_c(dir_aap, 'fig05/k/fp_str_summary_combined_post_stats_aov_oneway.csv')))
record('fig05', 'k', 'rmANOVA 1-within (solution, per set_id x region)',
       'none', 'fig05k_1way_aov')

add_sheet(wb, 'fig05k_1way_aov_hsd',
  safe_read(str_c(dir_aap, 'fig05/k/fp_str_summary_combined_post_stats_hsd_oneway.csv')))
record('fig05', 'k', 'rmANOVA 1-within posthoc (Tukey HSD)',
       'none', 'fig05k_1way_aov_hsd')

add_sheet(wb, 'fig05k_2way_aov',
  safe_read(str_c(dir_aap, 'fig05/k/fp_str_summary_combined_post_stats_aov_interaction.csv')))
record('fig05', 'k', 'rmANOVA 2-within (solution x region interaction)',
       'none', 'fig05k_2way_aov')

add_sheet(wb, 'fig05k_2way_aov_hsd',
  safe_read(str_c(dir_aap, 'fig05/k/fp_str_summary_combined_post_stats_hsd_interaction.csv')))
record('fig05', 'k', 'rmANOVA 2-within posthoc (Tukey HSD)',
       'none', 'fig05k_2way_aov_hsd')

# Panels g/j/m — placement x response range (Pearson correlation, per time bin)
add_sheet(wb, 'fig05gjm_cor',
  combine_named(c(
    'prelick'   = str_c(dir_aap, 'fig05/gjm/placement_vs_range_WRNaCl_prelick_corr_stats.csv'),
    'sustained' = str_c(dir_aap, 'fig05/gjm/placement_vs_range_WRNaCl_sustained_corr_stats.csv'),
    'post'      = str_c(dir_aap, 'fig05/gjm/placement_vs_range_WRNaCl_post_corr_stats.csv')
  ), id_col = 'time_bin'))
record('fig05', 'g/j/m', 'Pearson correlation (placement x response range, WR:NaCl)',
       'time_bin', 'fig05gjm_cor')


# ── fig06 (GLM delta-R) ───────────────────────────────────────────────────────
# Panel c — R² true vs shuffled, per region (paired t-test)

add_sheet(wb, 'fig06c_ttest',
  safe_read(str_c(dir_aap, 'fig06/c/r2_delta_ttest.csv')))
record('fig06', 'c', 'Paired t-test (R2 true vs shuffled)',
       'none', 'fig06c_ttest')

# Panels d/e/f/g left — region x delta-R line (rmANOVA 1-between, per predictor)

glm_predictors <- c('conc', 'lick', 'history', 'trial')

add_sheet(wb, 'fig06defg_aov',
  map_dfr(glm_predictors, function(v) {
    df <- safe_read(str_c(dir_aap, 'fig06/d_g_left/glm_deltar_str_overall_line_', v, '_aov.csv'))
    prepend_col(df, 'predictor', v)
  }))
record('fig06', 'd/e/f/g', 'rmANOVA 1-between (region x delta-R)',
       'predictor', 'fig06defg_aov')

add_sheet(wb, 'fig06defg_aov_hsd',
  map_dfr(glm_predictors, function(v) {
    df <- safe_read(str_c(dir_aap, 'fig06/d_g_left/glm_deltar_str_overall_line_', v, '_aov_hsd.csv'))
    prepend_col(df, 'predictor', v)
  }))
record('fig06', 'd/e/f/g', 'rmANOVA 1-between posthoc (Tukey HSD)',
       'predictor', 'fig06defg_aov_hsd')

# Panels d/e/f/g right — placement x delta-R (Pearson correlation, per predictor)
add_sheet(wb, 'fig06defg_cor',
  map_dfr(glm_predictors, function(v) {
    df <- safe_read(str_c(dir_aap, 'fig06/g_right/placement_vs_deltar_', v, '_corr_stats.csv'))
    prepend_col(df, 'predictor', v)
  }))
record('fig06', 'd/e/f/g', 'Pearson correlation (placement x delta-R, per predictor)',
       'predictor', 'fig06defg_cor')


# ── fig07 (inter-regional correlations) ──────────────────────────────────────
# Panel d — set_id x pairwise R (rmANOVA 1-within)

add_sheet(wb, 'fig07d_aov',
  safe_read(str_c(dir_aap, 'fig07/d/cor_peth_setid_summary_sustained_aov.csv')))
record('fig07', 'd', 'rmANOVA 1-within (set_id x mean pairwise R, sustained)',
       'none', 'fig07d_aov')

add_sheet(wb, 'fig07d_aov_hsd',
  safe_read(str_c(dir_aap, 'fig07/d/cor_peth_setid_summary_sustained_aov_hsd.csv')))
record('fig07', 'd', 'rmANOVA 1-within posthoc (Tukey HSD)',
       'none', 'fig07d_aov_hsd')

# Panel e — region x pairwise R (rmANOVA 1-within)
add_sheet(wb, 'fig07e_aov',
  safe_read(str_c(dir_aap, 'fig07/e/cor_peth_sustained_pairwisecor_aov.csv')))
record('fig07', 'e', 'rmANOVA 1-within (region pair x pairwise R, sustained)',
       'none', 'fig07e_aov')

add_sheet(wb, 'fig07e_aov_hsd',
  safe_read(str_c(dir_aap, 'fig07/e/cor_peth_sustained_pairwisecor_aov_hsd.csv')))
record('fig07', 'e', 'rmANOVA 1-within posthoc (Tukey HSD)',
       'none', 'fig07e_aov_hsd')

# Panel f — fiber distance x pairwise R (Pearson correlation)
add_sheet(wb, 'fig07f_cor',
  safe_read(str_c(dir_aap, 'fig07/f/cor_peth_pairwise_cor_vs_euclidian_stats_cor.csv')))
record('fig07', 'f', 'Pearson correlation (fiber distance x pairwise R)',
       'none', 'fig07f_cor')

# Panel h — LHA:GABA vs LHA:Glut baseline R (unpaired t-test, per STR region)
add_sheet(wb, 'fig07h_ttest',
  safe_read(str_c(dir_aap, 'fig07/h/cor_baseline_wrnacl_lha_vs_r_facet_str_bar_summary_stats_ttest.csv')))
record('fig07', 'h', 't-test unpaired (LHA:GABA vs LHA:Glut baseline R, per STR region)',
       'none', 'fig07h_ttest')

# Panel i — LHA/STR PETH corr summary (unpaired t-test, per STR region)
add_sheet(wb, 'fig07i_ttest',
  safe_read(str_c(dir_aap, 'fig07/i/cor_peth_summary_facet_bar_stats_ttest.csv')))
record('fig07', 'i', 't-test unpaired (LHA:GABA vs LHA:Glut PETH R, per STR region)',
       'none', 'fig07i_ttest')


# ── fig08 (VTA ChR2 opto behavior) ───────────────────────────────────────────
# Panel d — session/bout lick metrics (rmANOVA 1-between 1-within, per group x variable)

group_prefixes <- c('vta', 'nac', 'dms', 'dls', 'ts', 'all')
d_vars    <- c('mean_sesssion_lick_count', 'mean_bout_lick_count', 'mean_bout_n')
d_labels  <- c('session_lick_count',       'bout_lick_count',      'bout_count')

add_sheet(wb, 'fig08d_aov',
  map_dfr(seq_along(d_vars), function(i) {
    map_dfr(group_prefixes, function(pfx) {
      df <- safe_read(str_c(dir_aap, 'fig08/d/', pfx, '_', d_vars[i], '_aov.csv'))
      df <- prepend_col(df, 'variable', d_labels[i])
      prepend_col(df, 'group', pfx)
    })
  }))
record('fig08', 'd', 'rmANOVA 1-between 1-within (group x laser_state)',
       'group x variable', 'fig08d_aov')

add_sheet(wb, 'fig08d_aov_hsd',
  map_dfr(seq_along(d_vars), function(i) {
    map_dfr(group_prefixes, function(pfx) {
      df <- safe_read(str_c(dir_aap, 'fig08/d/', pfx, '_', d_vars[i], '_aov_hsd.csv'))
      df <- prepend_col(df, 'variable', d_labels[i])
      prepend_col(df, 'group', pfx)
    })
  }))
record('fig08', 'd', 'rmANOVA posthoc (Tukey HSD)',
       'group x variable', 'fig08d_aov_hsd')

# Panel g — trial lick count/proportion (rmANOVA 1-between 1-within)
g_vars   <- c('trial_lick_count', 'trial_lick_prop')
g_labels <- c('licks_per_trial',  'proportion_trials_with_lick')

add_sheet(wb, 'fig08g_aov',
  map_dfr(seq_along(g_vars), function(i) {
    map_dfr(group_prefixes, function(pfx) {
      df <- safe_read(str_c(dir_aap, 'fig08/g/', pfx, '_', g_vars[i], '_aov.csv'))
      df <- prepend_col(df, 'variable', g_labels[i])
      prepend_col(df, 'group', pfx)
    })
  }))
record('fig08', 'g', 'rmANOVA 1-between 1-within (group x laser_state)',
       'group x variable', 'fig08g_aov')

add_sheet(wb, 'fig08g_aov_hsd',
  map_dfr(seq_along(g_vars), function(i) {
    map_dfr(group_prefixes, function(pfx) {
      df <- safe_read(str_c(dir_aap, 'fig08/g/', pfx, '_', g_vars[i], '_aov_hsd.csv'))
      df <- prepend_col(df, 'variable', g_labels[i])
      prepend_col(df, 'group', pfx)
    })
  }))
record('fig08', 'g', 'rmANOVA posthoc (Tukey HSD)',
       'group x variable', 'fig08g_aov_hsd')

# Summation — delta licking (paired t-test)
add_sheet(wb, 'fig08_summation_ttest',
  safe_read(str_c(dir_aap, 'fig08/summation/delta_licking_ttest.csv')))
record('fig08', 'summation', 't-test paired (summation delta licking)',
       'none', 'fig08_summation_ttest')


# ── sfig01 ────────────────────────────────────────────────────────────────────
# Panel k — LHA GABA/Glut PETH correlation (rmANOVA)
add_sheet(wb, 'sfig01k_aov',
  safe_read(str_c(dir_aap, 'sfig01/k/peth_corblsub_summary_aov.csv')))
record('sfig01', 'k', 'rmANOVA (LHA GABA/Glut PETH corr x set_id)',
       'none', 'sfig01k_aov')

add_sheet(wb, 'sfig01k_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig01/k/peth_corblsub_summary_aov_hsd.csv')))
record('sfig01', 'k', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig01k_aov_hsd')


# ── sfig04 ────────────────────────────────────────────────────────────────────
# Panel h — CNO x opto delta summary (rmANOVA)

add_sheet(wb, 'sfig04h_aov',
  safe_read(str_c(dir_aap, 'fig02_sfig04/sfig04h/opto20hz_timerel_vs_delta_summary_aov.csv')))
record('sfig04', 'h', 'rmANOVA (time_rel x delta zscore, CNO effect)', 'none', 'sfig04h_aov')

add_sheet(wb, 'sfig04h_aov_hsd',
  safe_read(str_c(dir_aap, 'fig02_sfig04/sfig04h/opto20hz_timerel_vs_delta_summary_aov_hsd.csv')))
record('sfig04', 'h', 'rmANOVA posthoc (Tukey HSD)', 'none', 'sfig04h_aov_hsd')


# ── sfig05 ────────────────────────────────────────────────────────────────────
# Panel b — baseline period: region x inhibition difference (Wilcoxon)

add_sheet(wb, 'sfig05c_wilcox_ph',
  safe_read(str_c(dir_aap, 'fig04_sfig05/sfig05c/inh_baseline_region_vs_difference_stats_ph.csv')))
record('sfig05', 'c', 'Wilcoxon posthoc (region x baseline inh. difference)',
       'none', 'sfig05c_wilcox_ph')

# Panel e — pre-access bl: region x inhibition difference (Wilcoxon)
add_sheet(wb, 'sfig05e_wilcox_ph',
  safe_read(str_c(dir_aap, 'fig04_sfig05/sfig05e/inh_access_preaccessbl_region_vs_difference_stats_ph.csv')))
record('sfig05', 'e', 'Wilcoxon posthoc (region x inh. difference, pre-access bl)',
       'none', 'sfig05e_wilcox_ph')


# ── sfig07 (onset / range / lick) ─────────────────────────────────────────────
# Panel b — placement x response range (Pearson correlation, per time bin)

add_sheet(wb, 'sfig07b_cor',
  combine_named(c(
    'prelick'   = str_c(dir_aap, 'sfig07/b/placement_vs_range_WRNaCl_prelick_corr_stats.csv'),
    'sustained' = str_c(dir_aap, 'sfig07/b/placement_vs_range_WRNaCl_sustained_corr_stats.csv'),
    'post'      = str_c(dir_aap, 'sfig07/b/placement_vs_range_WRNaCl_post_corr_stats.csv')
  ), id_col = 'time_bin'))
record('sfig07', 'b', 'Pearson correlation (placement x response range, WR:NaCl)',
       'time_bin', 'sfig07b_cor')

# Panel f — onset time by region (rmANOVA 1-within)
add_sheet(wb, 'sfig07f_aov',
  safe_read(str_c(dir_aap, 'sfig07/f/fp_onset_summary_aov.csv')))
record('sfig07', 'f', 'rmANOVA 1-within (region x onset time)',
       'none', 'sfig07f_aov')

add_sheet(wb, 'sfig07f_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig07/f/fp_onset_summary_aov_hsd.csv')))
record('sfig07', 'f', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig07f_aov_hsd')

# Panel h — zscore vs lick_count per-subject R-squared (rmANOVA 2-between)
add_sheet(wb, 'sfig07h_aov',
  safe_read(str_c(dir_aap, 'sfig07/h/fp_str_cor_zscore_vs_lick_subject_summary_combined_aov.csv')))
record('sfig07', 'h', 'rmANOVA 2-between (region x set_id x R-squared)',
       'none', 'sfig07h_aov')

add_sheet(wb, 'sfig07h_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig07/h/fp_str_cor_zscore_vs_lick_subject_summary_combined_aov_hsd.csv')))
record('sfig07', 'h', 'rmANOVA 2-between posthoc (Tukey HSD)',
       'none', 'sfig07h_aov_hsd')

# Panel i — lick count bin x solution heat map (mixed model)
add_sheet(wb, 'sfig07i_mm',
  safe_read(str_c(dir_aap, 'sfig07/i/fp_str_solution_vs_lickcountbinned_heat_stats_mm.csv')))
record('sfig07', 'i', 'Mixed model (lick_count x solution, per region x set_id)',
       'none', 'sfig07i_mm')


# ── sfig08 (STR pairwise correlation) ────────────────────────────────────────
# Panel b — mean pairwise R x set_id (rmANOVA 1-within)

add_sheet(wb, 'sfig08b_aov',
  safe_read(str_c(dir_aap, 'sfig08/b/cor_baseline_setid_summary_aov.csv')))
record('sfig08', 'b', 'rmANOVA 1-within (set_id x mean pairwise R)',
       'none', 'sfig08b_aov')

add_sheet(wb, 'sfig08b_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig08/b/cor_baseline_setid_summary_aov_hsd.csv')))
record('sfig08', 'b', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig08b_aov_hsd')

# Panel c — pairwise R by region pair (rmANOVA 1-within)
add_sheet(wb, 'sfig08c_aov',
  safe_read(str_c(dir_aap, 'sfig08/c/cor_baseline_setid_pairwise_cor_aov.csv')))
record('sfig08', 'c', 'rmANOVA 1-within (region pair x pairwise R)',
       'none', 'sfig08c_aov')

add_sheet(wb, 'sfig08c_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig08/c/cor_baseline_setid_pairwise_cor_aov_hsd.csv')))
record('sfig08', 'c', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig08c_aov_hsd')

# Panel d right — fiber distance x pairwise R (Pearson)
add_sheet(wb, 'sfig08d_cor',
  safe_read(str_c(dir_aap, 'sfig08/d/cor_baseline_setid_pairwise_cor_vs_euclidian_stats_cor.csv')))
record('sfig08', 'd (right)', 'Pearson correlation (fiber distance x pairwise R)',
       'none', 'sfig08d_cor')


# ── sfig09 ────────────────────────────────────────────────────────────────────
# Panel b — saccharine licking (rmANOVA + Pearson correlation sacc20 vs sucrose10)

add_sheet(wb, 'sfig09b_aov',
  safe_read(str_c(dir_aap, 'sfig09/b/fp_sacc_summary_combined_licking_aov.csv')))
record('sfig09', 'b', 'rmANOVA (saccharine licking x solution)',
       'none', 'sfig09b_aov')

add_sheet(wb, 'sfig09b_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig09/b/fp_sacc_summary_combined_licking_aov_hsd.csv')))
record('sfig09', 'b', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig09b_aov_hsd')

add_sheet(wb, 'sfig09b_cor',
  safe_read(str_c(dir_aap, 'sfig09/b/fp_sacc_summary_combined_licking_corr_sacc_vs_sucrose10_stats_cor.csv')))
record('sfig09', 'b', 'Pearson correlation (sacc20 vs sucrose10 licking)',
       'none', 'sfig09b_cor')

# Panel c — sacc20 vs sucrose10 FP sustained (paired t-test)
add_sheet(wb, 'sfig09c_ttest',
  safe_read(str_c(dir_aap, 'sfig09/c/fp_sacc_summary_combined_sustained_sacc_vs_sucrose10_stats_ttest.csv')))
record('sfig09', 'c', 't-test paired (sacc20 vs sucrose10 FP, sustained)',
       'none', 'sfig09c_ttest')

# Panel d — sacc delta x region (rmANOVA, STR and LHA combined with leading system column)
add_sheet(wb, 'sfig09d_aov', {
  d_str <- prepend_col(
    safe_read(str_c(dir_aap, 'sfig09/d/fp_sacc_summary_combined_sustained_sacc_vs_sucrose10_delta_str_aov.csv')),
    'system', 'STR')
  d_lha <- prepend_col(
    safe_read(str_c(dir_aap, 'sfig09/d/fp_sacc_summary_combined_sustained_sacc_vs_sucrose10_delta_lha_aov.csv')),
    'system', 'LHA')
  bind_rows(d_str, d_lha)
})
record('sfig09', 'd', 'rmANOVA (sacc delta x region, STR and LHA separate)',
       'system (STR/LHA)', 'sfig09d_aov')

add_sheet(wb, 'sfig09d_aov_hsd', {
  d_str <- prepend_col(
    safe_read(str_c(dir_aap, 'sfig09/d/fp_sacc_summary_combined_sustained_sacc_vs_sucrose10_delta_str_aov_hsd.csv')),
    'system', 'STR')
  d_lha <- prepend_col(
    safe_read(str_c(dir_aap, 'sfig09/d/fp_sacc_summary_combined_sustained_sacc_vs_sucrose10_delta_lha_aov_hsd.csv')),
    'system', 'LHA')
  bind_rows(d_str, d_lha)
})
record('sfig09', 'd', 'rmANOVA posthoc (Tukey HSD)',
       'system (STR/LHA)', 'sfig09d_aov_hsd')

# Panel f — water licking x set_id (rmANOVA)
add_sheet(wb, 'sfig09f_aov',
  safe_read(str_c(dir_aap, 'sfig09/f/fp_str_water_summary_licking_aov.csv')))
record('sfig09', 'f', 'rmANOVA (water licking x set_id)',
       'none', 'sfig09f_aov')

add_sheet(wb, 'sfig09f_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig09/f/fp_str_water_summary_licking_aov_hsd.csv')))
record('sfig09', 'f', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig09f_aov_hsd')

# Panel g — water FP x set_id x region (rmANOVA)
add_sheet(wb, 'sfig09g_aov',
  safe_read(str_c(dir_aap, 'sfig09/g/fp_str_water_summary_peth_sustained_aov.csv')))
record('sfig09', 'g', 'rmANOVA (water FP sustained x set_id, per region)',
       'none', 'sfig09g_aov')

add_sheet(wb, 'sfig09g_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig09/g/fp_str_water_summary_peth_sustained_aov_hsd.csv')))
record('sfig09', 'g', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig09g_aov_hsd')

# Panel i — lick_count bin x set_id heat (mixed model)
add_sheet(wb, 'sfig09i_mm',
  safe_read(str_c(dir_aap, 'sfig09/i/fp_str_water_heat_setid_vs_binnedlick_stats_mmanova.csv')))
record('sfig09', 'i', 'Mixed model (set_id x lick_count interaction, per region)',
       'none', 'sfig09i_mm')

# Panel j — satiation licking x trial_bin x set_id (rmANOVA)
add_sheet(wb, 'sfig09j_aov',
  safe_read(str_c(dir_aap, 'sfig09/j/fp_satiation_water_trialbin_setid_licking_aov.csv')))
record('sfig09', 'j', 'rmANOVA (satiation licking x trial_bin x set_id)',
       'none', 'sfig09j_aov')

# Panel l — satiation rank slopes (rmANOVA 1-between 1-within)
add_sheet(wb, 'sfig09l_aov',
  safe_read(str_c(dir_aap, 'sfig09/l/fp_satiation_rank_trialbin_zsvszs_slope_aov.csv')))
record('sfig09', 'l', 'rmANOVA 1-between 1-within (solution_rank x region x zs slope)',
       'none', 'sfig09l_aov')

add_sheet(wb, 'sfig09l_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig09/l/fp_satiation_rank_trialbin_zsvszs_slope_aov_hsd.csv')))
record('sfig09', 'l', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig09l_aov_hsd')

# Panel m — satiation rank slope summary (rmANOVA 2-within)
add_sheet(wb, 'sfig09m_aov',
  safe_read(str_c(dir_aap, 'sfig09/m/fp_satiation_rank_trialbin_slope_aov.csv')))
record('sfig09', 'm', 'rmANOVA 2-within (solution_rank x region, slope of zs vs trial_bin)',
       'none', 'sfig09m_aov')

add_sheet(wb, 'sfig09m_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig09/m/fp_satiation_rank_trialbin_slope_aov_hsd.csv')))
record('sfig09', 'm', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig09m_aov_hsd')


# ── sfig10 (electric tail shock) ─────────────────────────────────────────────
# Panel c — ETS FP summary (rmANOVA 1-within, mA x region)

add_sheet(wb, 'sfig10c_aov',
  safe_read(str_c(dir_aap, 'sfig10/c/ets_summary_aov.csv')))
record('sfig10', 'c', 'rmANOVA 1-within (mA x region, ETS FP summary)',
       'none', 'sfig10c_aov')

add_sheet(wb, 'sfig10c_aov_hsd',
  safe_read(str_c(dir_aap, 'sfig10/c/ets_summary_aov_hsd.csv')))
record('sfig10', 'c', 'rmANOVA posthoc (Tukey HSD)',
       'none', 'sfig10c_aov_hsd')

# Panel e — ETS x consumption FP (Pearson correlation, per solution)
add_sheet(wb, 'sfig10e_cor',
  safe_read(str_c(dir_aap, 'sfig10/e/ets_corr_consumption_stats.csv')))
record('sfig10', 'e', 'Pearson correlation (ETS zscore x consumption FP)',
       'solution', 'sfig10e_cor')

# Panel f — ETS x LHA stim response (Pearson correlation, per group_combined)
add_sheet(wb, 'sfig10f_cor',
  safe_read(str_c(dir_aap, 'sfig10/f/ets_corr_lhastim_stats.csv')))
record('sfig10', 'f', 'Pearson correlation (ETS zscore x LHA stim response)',
       'group_combined', 'sfig10f_cor')


# ── index sheet ───────────────────────────────────────────────────────────────

df_index <- bind_rows(header_rows)

addWorksheet(wb, 'index')
writeData(wb, sheet = 'index', df_index)

# Move index to first position
n <- length(wb$worksheets)
worksheetOrder(wb) <- c(n, seq_len(n - 1))

saveWorkbook(wb, file = str_c(dir_out, 'stats_compiled.xlsx'), overwrite = TRUE)
message('done -- ', nrow(df_index), ' sheets written to ', dir_out, 'stats_compiled.xlsx')
