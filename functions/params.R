# params.R
# Shared parameters, lookup tables, and color palettes used across all analysis scripts.
# Source this file at the top of every figure script after loading libraries.

# ── Region lookup ──────────────────────────────────────────────────────────────
# Maps internal region codes to publication-ready labels.
# Order here sets the factor level order in all plots.
key_regions <- tibble(
  region       = c('lha_gaba', 'lha_glut', 'lha_ratio',
                   'nac',      'nac_corr', 'nacsh_med', 'nac_shelllat', 'nacsh_lat',
                   'dms', 'dls', 'ts'),
  region_label = c('LHA:GABA', 'LHA:Glut', 'LHA:Ratio',
                   'NAcCR',    'NAcCC',    'NAcShM',    'NAcShL',       'NAcShL',
                   'DMS', 'DLS', 'TS')
)

levels_region <- unique(key_regions$region_label)

# region_ids: internal codes used when iterating over preprocessed data files
region_ids <- c(
  'nac', 'nac_corr', 'nac_shelllat', 'nacsh_lat', 'nacsh_med',
  'dms', 'dls', 'ts',
  'lha_gaba', 'lha_glut', 'lha_ratio'
)

# ── Solution lookup ────────────────────────────────────────────────────────────
key_solutions <- tibble(
  solution       = c('sucrose00', 'sucrose05', 'sucrose10', 'sucrose20', 'sucrose30',
                     'nacl000',   'nacl025',   'nacl050',   'nacl100',   'nacl150'),
  solution_label = c('0',         '5',         '10',        '20',        '30',
                     '0.00',      '0.25',      '0.50',      '1.00',      '1.50'),
  solution_set   = c('sucrose',   'sucrose',   'sucrose',   'sucrose',   'sucrose',
                     'nacl',      'nacl',      'nacl',      'nacl',      'nacl')
)

# Plotting x-position and facet assignment for solution axis
join_solution_x <- tibble(
  solution = c('sucrose00', 'sucrose05', 'sucrose10', 'sucrose20', 'sucrose30',
               'nacl000',   'nacl025',   'nacl050',   'nacl100',   'nacl150'),
  plt_x    = c(1, 2, 3, 4, 5, 5, 4, 3, 2, 1),
  plt_f    = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
)

# ── Set-ID lookup ──────────────────────────────────────────────────────────────
# Maps internal set_id codes to publication labels.
key_set_id <- tibble(
  set_id =       c('wrsuc', 'frsuc', 'wrnacl', 'frmultisacc', 'wrwater'),
  set_id_label = c('WR:Suc','FR:Suc','WR:NaCl', 'FR:Sacc',    'WR:Water')
)


# ── Color palettes ─────────────────────────────────────────────────────────────
# All color vectors are defined once here and imported by figure scripts.
# Requires viridisLite to be loaded before sourcing this file.

color_values_nacl    <- viridisLite::inferno(5, begin = 0, end = 0.8)
color_values_suc     <- viridisLite::mako(5,    begin = 0, end = 0.8)
color_values_misc01  <- c('black', 'grey', '#BCD634', '#34BCD6')
color_values_misc02  <- c('black', '#D634BC', '#BCD634', '#34BCD6')

# Regions (DA-recording cohort, 6-region subset)
color_values_regions_da7 <- viridisLite::turbo(7, begin = 0, end = 0.8)
color_values_regions_da  <- color_values_regions_da7[c(1, 2, 4, 5, 6, 7)]
color_values_lha <- c('#72BF45', '#CD202A', '#7B8137')

# Full region palette (all 9 regions)
color_values_regions <- c(color_values_lha, color_values_regions_da)
  
# Experiment-set palette
color_values_experiment_set  <- c('#7edb00', '#007edb', '#db007e')

# Group-combined palette (control / LHA-GABA / LHA-Glut)
color_values_groupcombined <- c('darkgrey', '#CC2029', '#70BF46')

# ── Formatting helpers ─────────────────────────────────────────────────────────

format_solutions <- function(df) {
  df %>%
    ungroup() %>%
    left_join(key_solutions, by = "solution") %>%
    mutate(solution_label = factor(solution_label, levels = key_solutions$solution_label)) %>%
    select(-solution) %>%
    rename(solution = solution_label) %>%
    select(solution, everything())
}

format_group_combined <- function(df) {
  df %>%
    mutate(group_combined = case_when(
      group_combined == 'control'     ~ 'Control',
      group_combined == 'vgat_stim'   ~ 'LHA GABA',
      group_combined == 'vglut2_stim' ~ 'LHA Glut'
    ))
}
