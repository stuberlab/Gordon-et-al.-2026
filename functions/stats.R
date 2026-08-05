# stats.R
# Statistical helper functions for bootstrap CIs, correlation matrices, and
# ANOVA wrappers.
#
# NOTE: quick_sig() and save_aov() live in theme.R (they are output-formatting
# utilities also needed by plotting code). This file depends on theme.R being
# sourced first.

# ── Bootstrap ──────────────────────────────────────────────────────────────────

boot_mean <- function(data, indices) {
  d <- data[indices]
  mean(d)
}

bootstrap_mean <- function(df, var_iv) {
  require(boot)

  data_observed <- df %>% pull(var_iv)
  mean_observed <- mean(data_observed)

  boot_res <- boot(data_observed, statistic = boot_mean, R = 1000)

  boot_ci <- boot.ci(boot_res, type = "perc")$percent[4:5]

  p_value_less    <- mean(boot_res$t <= 0)
  p_value_greater <- mean(boot_res$t >= 0)
  boot_pvalue     <- 2 * min(p_value_less, p_value_greater)

  tibble(
    boot_var_iv        = var_iv,
    boot_ci_low        = boot_ci[1],
    boot_ci_high       = boot_ci[2],
    boot_mean_observed = mean_observed,
    boot_pvalue        = boot_pvalue
  )
}

filter_consecutive_ones <- function(vector, consecutive_filt_width) {
  r <- rle(vector)
  is_short_run <- r$values == 1 & r$lengths < consecutive_filt_width
  r$values[is_short_run] <- 0
  inverse.rle(r)
}

get_bootstrap_sig <- function(df, var_pvalue, pvalue_treshold, consecutive_filt_width) {
  df %>%
    mutate(sig = ifelse(!!sym(var_pvalue) < pvalue_treshold, 1, 0)) %>%
    mutate(sig = filter_consecutive_ones(sig, consecutive_filt_width = 3))
}

# ── Correlation matrices ───────────────────────────────────────────────────────
# tidy_cor_output_reshape / tidy_cor_output / get_cormatrix were previously
# duplicated in clustering.R.  The canonical versions live here; clustering.R
# imports them.

tidy_cor_output_reshape <- function(df, n_table, var_name) {
  df_tidy <- df[[n_table]] %>% as_tibble()
  df_tidy %>%
    mutate(var_y = colnames(df_tidy)) %>%
    gather('var_x', !!as.name(var_name), -var_y)
}

tidy_cor_output <- function(df) {
  tidy_cor_output_reshape(df, 1, 'r') %>%
    left_join(tidy_cor_output_reshape(df, 2, 'n'), by = c("var_y", "var_x")) %>%
    left_join(tidy_cor_output_reshape(df, 3, 'p'), by = c("var_y", "var_x"))
}

get_cormatrix <- function(df) {
  df_corr <- rcorr(as.matrix(df))
  tidy_cor_output(df_corr)
}

tidy_cormatrix <- function(df, var_cors, var_groups = NA) {
  require(Hmisc)

  df <- df %>% ungroup()

  if (sum(is.na(var_groups)) > 0) {
    df <- df %>% select(var_cors)
    df %>% group_modify(~ get_cormatrix(.))
  } else {
    df <- df %>% select(var_cors, var_groups)
    df %>%
      group_by_at(var_groups) %>%
      group_modify(~ get_cormatrix(.))
  }
}

# ── Correlation plot ───────────────────────────────────────────────────────────

plt_corrmatrix <- function(df) {
  df %>%
    ggplot(aes(var_x, var_y, fill = r)) +
    geom_tile() +
    scale_fill_gradientn(colors = c('blue', 'white', 'red'), limits = c(-1, 1), oob = scales::squish) +
    theme_ag01() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.3)
    )
}

# ── HSD matrix plots ───────────────────────────────────────────────────────────

aov_rm_one_between <- function(df, dir_output, prefix, var_id, var_dependent, var_between) {
  require(afex)
  require(emmeans)

  anova_model <- aov_ez(
    id          = var_id,
    dv          = var_dependent,
    data        = df,
    between     = var_between,
    anova_table = list(correction = "none", es = "none")
  )

  anova_summary <- summary(anova_model) %>%
    broom::tidy() %>%
    rename("effect"   = "term",
           "df_num"   = "num.Df",
           "df_den"   = "den.Df",
           "mse"      = "MSE",
           "f"        = "statistic",
           "p_value"  = "p.value") %>%
    mutate(stat        = 'aov_rm_one_between',
           var_dv      = var_dependent,
           var_id      = var_id,
           var_between = var_between) %>%
    select(stat, var_dv, var_id, var_between, everything()) %>%
    quick_sig()

  interaction <- paste("~", var_between, sep = "")
  emm         <- emmeans(anova_model, formula(interaction))

  pairs_hsd <- pairs(emm) %>%
    as_tibble() %>%
    separate(contrast, into = c('contrast1_between', 'contrast2_between'), sep = '-') %>%
    mutate_if(is.character, str_trim) %>%
    rename('p_value' = 'p.value', 't_ratio' = 't.ratio') %>%
    quick_sig()

  pairs_effect_size <- eff_size(emm,
    sigma = sqrt(mean(sigma(anova_model$lm)^2)),
    edf   = df.residual(anova_model$lm)) %>%
    as_tibble() %>%
    separate(contrast, into = c('contrast1_between', 'contrast2_between'), sep = '-') %>%
    mutate_if(is.character, str_trim) %>%
    clean_names() %>%
    rename(es_se = se, es_lower_cl = lower_cl, es_upper_cl = upper_cl) %>%
    select(-df)

  pairs_hsd <- pairs_hsd %>%
    left_join(pairs_effect_size, by = join_by(contrast1_between, contrast2_between))

  save_aov(dir_output, prefix, anova_summary, pairs_hsd, df)

  return(list(anova_model, anova_summary, pairs_hsd))
}

# ── General helpers ────────────────────────────────────────────────────────────

do_ttest_paired <- function(df, var1, var2) {
  t.test(df %>% pull(var1), df %>% pull(var2), paired = TRUE, alternative = 'two.sided') %>%
    broom::tidy() %>%
    mutate(ag_stat = 'do_ttest_paired')
}

tidy_ttest_paired <- function(df, dir_output, prefix, var1, var2, ...) {
  stats_result <- df %>%
    group_by_(...) %>%
    do(do_ttest_paired(., var1, var2)) %>%
    mutate(var1 = var1, var2 = var2) %>%
    select(var1, var2, everything()) %>%
    clean_names()
  save_ttest(dir_output, prefix, stats_result)
  return(stats_result)
}

do_ttest_unpaired <- function(df, var1, var2) {
  t.test(df %>% pull(var1), df %>% pull(var2), paired = FALSE, alternative = 'two.sided') %>%
    broom::tidy() %>%
    mutate(ag_stat = 'do_ttest_unpaired')
}

tidy_ttest_unpaired <- function(df, dir_output, prefix, var1, var2, ...) {
  stats_result <- df %>%
    group_by_(...) %>%
    do(do_ttest_unpaired(., var1, var2)) %>%
    mutate(var1 = var1, var2 = var2) %>%
    select(..., var1, var2, everything()) %>%
    clean_names()
  save_ttest(dir_output, prefix, stats_result)
  return(stats_result)
}

do_ttest_unpaired_grouped <- function(df, var_iv, var_dv) {
  df_var_iv_levels <- df %>% pull(var_iv) %>% unique()
  df <- df %>% spread(!!as.name(var_iv), !!as.name(var_dv))
  t.test(df %>% pull(!!as.name(df_var_iv_levels[1])),
         df %>% pull(!!as.name(df_var_iv_levels[2])),
         paired = FALSE, alternative = 'two.sided') %>%
    broom::tidy() %>%
    clean_names() %>%
    mutate(ag_stat = 'do_ttest_unpaired',
           var1    = df_var_iv_levels[1],
           var2    = df_var_iv_levels[2]) %>%
    select(ag_stat, var1, var2, everything())
}

do_cor <- function(df, var1, var2) {
  tidy_cor_test <- cor.test(df %>% pull(var1), df %>% pull(var2), method = 'pearson') %>%
    broom::tidy() %>%
    mutate(ag_stat = 'do_cor') %>%
    rename('p_value' = p.value, 'conf_low' = conf.low, 'con_high' = conf.high)

  model <- lm(df %>% pull(var2) ~ df %>% pull(var1))
  model %>%
    broom::tidy() %>%
    clean_names() %>%
    mutate(parameter = ifelse(term == '(Intercept)', 'intercept', 'slope')) %>%
    mutate(parameter = str_c('estimate_', parameter)) %>%
    select(parameter, estimate) %>%
    spread(parameter, estimate) %>%
    bind_cols(tidy_cor_test)
}

tidy_cor <- function(df, var1, var2, ...) {
  df %>% group_by_(...) %>% do(do_cor(., var1, var2)) %>% quick_sig()
}

# ── Cross-correlation ──────────────────────────────────────────────────────────

do_cross_correlation <- function(df, var1, var2, lag) {
  ccf(df %>% pull(var1), df %>% pull(var2), lag = lag, plot = FALSE) %>%
    broom::tidy() %>%
    mutate(ag_stat = 'do_cross_correlation')
}

tidy_cross_correlation <- function(df, var1, var2, lag, ...) {
  df %>%
    group_by_(...) %>%
    do(do_cross_correlation(., var1, var2, lag)) %>%
    mutate(ccf = str_c(var1, ' & ', var2))
}

# ── Kruskal-Wallis ─────────────────────────────────────────────────────────────

do_wilcox_unpaired <- function(df, var1, var2) {
  wilcox.test(df %>% pull(var1), df %>% pull(var2), paired = FALSE, alternative = 'two.sided') %>%
    broom::tidy() %>%
    mutate(ag_stat = 'do_wilcox_unpaired')
}

tidy_wilcox_unpaired <- function(df, var1, var2, ...) {
  df %>% group_by_(...) %>% do(do_wilcox_unpaired(., var1, var2))
}

aov_rm_one_within <- function(df, dir_output, prefix, var_id, var_dependent, var_within) {
  require(afex)

  anova_model <- aov_ez(
    id = var_id, dv = var_dependent, data = df, within = var_within,
    anova_table = list(correction = "none", es = "none"))

  label_anova <- c("intercept", var_within) %>% as_tibble()

  anova_summary <- summary(anova_model)[[4]][] %>%
    as_tibble() %>%
    bind_cols(label_anova, .) %>%
    rename("effect"   = "value", "ss"       = "Sum Sq",
           "df_num"   = "num Df", "ss_error" = "Error SS",
           "df_den"   = "den Df", "f"        = "F value",
           "p_value"  = "Pr(>F)") %>%
    mutate(stat       = 'aov_rm_one_within',
           var_dv     = var_dependent,
           var_id     = var_id,
           var_within = var_within) %>%
    select(stat, var_dv, var_id, var_within, everything()) %>%
    quick_sig()

  interaction <- paste("~", var_within, sep = "")
  emm         <- emmeans(anova_model, formula(interaction))

  pairs_hsd <- pairs(emm) %>%
    as_tibble() %>%
    separate(contrast, into = c('contrast1_within', 'contrast2_within'), sep = '-') %>%
    mutate_if(is.character, str_trim) %>%
    rename('p_value' = 'p.value', 't_ratio' = 't.ratio') %>%
    quick_sig()

  pairs_effect_size <- eff_size(emm,
    sigma = sqrt(mean(sigma(anova_model$lm)^2)),
    edf   = df.residual(anova_model$lm)) %>%
    as_tibble() %>%
    separate(contrast, into = c('contrast1_within', 'contrast2_within'), sep = '-') %>%
    mutate_if(is.character, str_trim) %>%
    clean_names() %>%
    rename(es_se = se, es_lower_cl = lower_cl, es_upper_cl = upper_cl) %>%
    select(-df)

  pairs_hsd <- pairs_hsd %>%
    left_join(pairs_effect_size, by = join_by(contrast1_within, contrast2_within))

  save_aov(dir_output, prefix, anova_summary, pairs_hsd, df)
  return(list(anova_model, anova_summary, pairs_hsd))
}

aov_rm_one_between_one_within <- function(df, dir_output, prefix, var_id,
                                          var_dependent, var_between, var_within) {
  require(afex)

  anova_model <- aov_ez(
    id = var_id, dv = var_dependent, data = df,
    within = var_within, between = var_between,
    anova_table = list(correction = "none", es = "none"))

  label_anova <- c("intercept", var_between, var_within,
                   str_c(var_between, '*', var_within)) %>% as_tibble()

  anova_summary <- summary(anova_model)[[4]][] %>%
    as_tibble() %>%
    bind_cols(label_anova, .) %>%
    rename("effect"   = "value", "ss"       = "Sum Sq",
           "df_num"   = "num Df", "ss_error" = "Error SS",
           "df_den"   = "den Df", "f"        = "F value",
           "p_value"  = "Pr(>F)") %>%
    mutate(stat        = 'aov_rm_one_between_one_within',
           var_dv      = var_dependent, var_id      = var_id,
           var_between = var_between,   var_within  = var_within) %>%
    select(stat, var_dv, var_id, var_between, var_within, everything()) %>%
    quick_sig()

  interaction <- paste("~", var_within, '*', var_between, sep = "")
  emm         <- emmeans(anova_model, formula(interaction))

  pairs_hsd <- pairs(emm) %>%
    as_tibble() %>%
    separate(contrast, into = c('contrast1', 'contrast2'), sep = '-') %>%
    mutate_if(is.character, str_trim) %>%
    separate(contrast1, into = c('contrast1_within', 'contrast1_between'), sep = ' ') %>%
    separate(contrast2, into = c('contrast2_within', 'contrast2_between'), sep = ' ') %>%
    mutate_if(is.character, str_trim) %>%
    rename('p_value' = 'p.value', 't_ratio' = 't.ratio') %>%
    quick_sig()

  save_aov(dir_output, prefix, anova_summary, pairs_hsd, df)
  return(list(anova_model, anova_summary, pairs_hsd))
}

aov_rm_two_within <- function(df, dir_output, prefix, var_id, var_dependent,
                               var_within1, var_within2) {
  require(afex)

  anova_model <- aov_ez(
    id = var_id, dv = var_dependent, data = df,
    within = c(var_within1, var_within2),
    anova_table = list(correction = "none", es = "none"))

  label_anova <- c("intercept", var_within1, var_within2,
                   str_c(var_within1, '*', var_within2)) %>% as_tibble()

  anova_summary <- summary(anova_model)[[4]][] %>%
    as_tibble() %>%
    bind_cols(label_anova, .) %>%
    rename("effect"   = "value", "ss"       = "Sum Sq",
           "df_num"   = "num Df", "ss_error" = "Error SS",
           "df_den"   = "den Df", "f"        = "F value",
           "p_value"  = "Pr(>F)") %>%
    mutate(stat        = 'aov_rm_two_within',
           var_dv      = var_dependent, var_id      = var_id,
           var_within1 = var_within1,   var_within2 = var_within2) %>%
    select(stat, var_dv, everything()) %>%
    quick_sig()

  interaction <- paste("~", var_within1, '*', var_within2, sep = "")
  emm         <- emmeans(anova_model, formula(interaction))

  pairs_hsd <- pairs(emm) %>%
    as_tibble() %>%
    separate(contrast, into = c('contrast1', 'contrast2'), sep = '-') %>%
    mutate_if(is.character, str_trim) %>%
    separate(contrast1, into = c('contrast1_within1', 'contrast1_within2'), sep = ' ') %>%
    separate(contrast2, into = c('contrast2_within1', 'contrast2_within2'), sep = ' ') %>%
    mutate_if(is.character, str_trim) %>%
    rename('p_value' = 'p.value', 't_ratio' = 't.ratio') %>%
    quick_sig()

  save_aov(dir_output, prefix, anova_summary, pairs_hsd, df)
  return(list(anova_model, anova_summary, pairs_hsd))
}

aov_rm_two_between <- function(df, dir_output, prefix, var_id, var_dependent,
                                var_between1, var_between2) {
  require(afex)

  anova_model <- aov_ez(
    id = var_id, dv = var_dependent, data = df,
    between = c(var_between1, var_between2),
    anova_table = list(correction = "none", es = "none"))

  label_anova <- c(var_between1, var_between2,
                   str_c(var_between1, '*', var_between2)) %>% as_tibble()

  anova_summary <- summary(anova_model)[] %>%
    as_tibble() %>%
    bind_cols(label_anova, .) %>%
    rename("effect"    = "value", "df_num"    = "num Df",
           "df_den"    = "den Df", "mse"       = "MSE",
           "f"         = "F",     "p_value"   = "Pr(>F)") %>%
    mutate(stat         = 'aov_rm_two_between',
           var_dv       = var_dependent, var_id       = var_id,
           var_between1 = var_between1,  var_between2 = var_between2) %>%
    select(stat, var_dv, everything()) %>%
    quick_sig()

  interaction <- paste("~", var_between1, '*', var_between2, sep = "")
  emm         <- emmeans(anova_model, formula(interaction))

  pairs_hsd <- pairs(emm) %>%
    as_tibble() %>%
    separate(contrast, into = c('contrast1', 'contrast2'), sep = '-') %>%
    mutate_if(is.character, str_trim) %>%
    separate(contrast1, into = c('contrast1_between1', 'contrast1_between2'), sep = ' ') %>%
    separate(contrast2, into = c('contrast2_between1', 'contrast2_between2'), sep = ' ') %>%
    mutate_if(is.character, str_trim) %>%
    rename('p_value' = 'p.value', 't_ratio' = 't.ratio') %>%
    quick_sig()

  save_aov(dir_output, prefix, anova_summary, pairs_hsd, df)
  return(list(anova_model, anova_summary, pairs_hsd))
}

aov_rm_one_between_two_within <- function(df, dir_output, prefix, var_id, var_dependent,
                                          var_between, var_within1, var_within2) {
  require(afex)

  anova_model <- aov_ez(
    id = var_id, dv = var_dependent, data = df,
    within = c(var_within1, var_within2), between = var_between,
    anova_table = list(correction = "none", es = "none"))

  label_anova <- c("intercept", var_between, var_within1,
                   str_c(var_between, '*', var_within1),
                   var_within2,
                   str_c(var_between, '*', var_within2),
                   str_c(var_within1, '*', var_within2),
                   str_c(var_between, '*', var_within1, '*', var_within2)) %>% as_tibble()

  anova_summary <- summary(anova_model)[[4]][] %>%
    as_tibble() %>%
    bind_cols(label_anova, .) %>%
    rename("effect"   = "value", "ss"       = "Sum Sq",
           "df_num"   = "num Df", "ss_error" = "Error SS",
           "df_den"   = "den Df", "f"        = "F value",
           "p_value"  = "Pr(>F)") %>%
    mutate(stat        = 'aov_rm_one_between_two_within',
           var_dv      = var_dependent, var_id      = var_id,
           var_between = var_between,   var_within1 = var_within1,
           var_within2 = var_within2) %>%
    select(stat, var_dv, var_id, var_between, var_within1, var_within2, everything()) %>%
    quick_sig()

  interaction <- paste("~", var_within2, '*', var_within1, '*', var_between, sep = "")
  emm         <- emmeans(anova_model, formula(interaction))

  pairs_hsd <- pairs(emm) %>%
    as_tibble() %>%
    separate(contrast, into = c('contrast1', 'contrast2'), sep = '-') %>%
    mutate_if(is.character, str_trim) %>%
    separate(contrast1, into = c('contrast1_within2', 'contrast1_within1', 'contrast1_between'), sep = ' ') %>%
    separate(contrast2, into = c('contrast2_within2', 'contrast2_within1', 'contrast2_between'), sep = ' ') %>%
    mutate_if(is.character, str_trim) %>%
    rename('p_value' = 'p.value', 't_ratio' = 't.ratio') %>%
    quick_sig()

  save_aov(dir_output, prefix, anova_summary, pairs_hsd, df)
  return(list(anova_model, anova_summary, pairs_hsd))
}

# ── HSD matrix (2-contrast version) ───────────────────────────────────────────

plt_hsd_matrix_2contrasts <- function(df_pairs, heat_limits = NA) {
  n_contrasts <- df_pairs %>% pull(contrast1_within) %>% unique() %>% length()

  df_pairs <- df_pairs %>%
    rename(contrast1_within_org = contrast1_within, contrast2_within_org = contrast2_within) %>%
    rename(contrast2_within = contrast1_within_org, contrast1_within = contrast2_within_org) %>%
    mutate(effect_size = -effect_size) %>%
    bind_rows(df_pairs, .)

  if (sum(is.na(heat_limits))) {
    max_abs_d  <- max(abs(df_pairs$effect_size))
    heat_limits <- c(-max_abs_d, max_abs_d)
  }

  df_pairs %>%
    mutate(sig_symbol = ifelse(sig_symbol == 'n.s.', '', sig_symbol)) %>%
    ggplot(aes(contrast2_within, contrast1_within, fill = effect_size, label = sig_symbol)) +
    geom_tile() + geom_text() +
    theme_ag01() +
    scale_fill_gradientn(colors = c('#0016EC', 'white', '#EC008C'),
                         limits = heat_limits, oob = scales::squish) +
    theme(axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.text.x  = element_text(angle = 90, hjust = 1, vjust = 0.3)) +
    coord_cartesian(xlim = c(0.5, n_contrasts + 1.5),
                    ylim = c(0.5, n_contrasts + 1.5), expand = F)
}


# ── LHA Bonferroni correction ───────────────────────────────────────────────
# Applies 3-test Bonferroni correction across GABA solution, Glut solution,
# and GABA x Glut interaction tests.  Optionally writes the corrected table.

anova_pvalue_correction <- function(dir_output, prefix, stats_results_gaba, stats_results_glut, stats_results_interaction){

  stats_results_gaba <- stats_results_gaba[[2]] %>%
    mutate(test = 'gaba_solution') %>%
    select(test, everything()) %>%
    filter(effect == 'solution')

  stats_results_glut <- stats_results_glut[[2]] %>%
    mutate(test = 'glut_solution') %>%
    select(test, everything()) %>%
    filter(effect == 'solution')

  stats_results_interaction <- stats_results_interaction[[2]] %>%
    mutate(test = 'interaction') %>%
    select(test, everything()) %>%
    filter(effect == 'region*solution')

  stats_corrected <- stats_results_gaba %>%
    bind_rows(stats_results_glut) %>%
    bind_rows(stats_results_interaction)

  stats_corrected <- stats_corrected %>%
    select(test, stat, effect, ss, df_num, ss_error, df_den, f, p_value_raw = p_value) %>%
    mutate(p_value = p_value_raw * 3) %>%
    quick_sig()

  if (!is.na(dir_output)) {
    write_csv(stats_corrected, str_c(dir_output, prefix, '.csv'))
    print(str_c('saved_file: ', dir_output, prefix, '.csv'))
  }

  return(stats_corrected)
}
