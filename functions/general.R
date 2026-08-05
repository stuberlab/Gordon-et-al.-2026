
read_session <- function(dir_sessions, blockname, suffix){

  fn <- str_c(dir_sessions, blockname, '/', blockname, suffix)


  if(!(suffix %>% str_detect('.csv')) & !(suffix %>% str_detect('.feather'))){
    print('file type not supported, check suffix')
    print('')
  }

  if(!file.exists(fn)){
    print('file does not exist')
    print(str_c('fn: ', fn))
    return(0)
  }

  if(fn %>% str_detect('.csv')){
    df <- read_csv(fn, show_col_types = FALSE)
    print(str_c('read file: ', fn))
    return(df)
  }

  if(fn %>% str_detect('.feather')){
    df <- read_feather(fn)
    print(str_c('read file: ', fn))
    return(df)
  }
}

get_region_labels <- function(df){

  key_regions <- tibble(
    region =       c('lhagaba',  'lha_gaba', 'lhaglut',  'lha_glut','lharatio', 'lha_ratio', 'nac',   'naccorr','nac_corr','naccorc','nacshm', 'nacsh_med','nacshl', 'nacsh_lat','nac_shelllat','dms', 'dls', 'ts'),
    region_label = c('LHA:GABA', 'LHA:GABA', 'LHA:Glut', 'LHA:Glut','LHA:Ratio','LHA:Ratio', 'NAcCR', 'NAcCR',  'NAcCC',   'NAcCC',  'NAcShM', 'NAcShM',   'NAcShL', 'NAcShL',   'NAcShL',  'DMS', 'DLS', 'TS')
  )

  df %>%
    left_join(key_regions, by = "region") %>%
    select(-region) %>%
    rename(region = region_label) %>%
    mutate(region = factor(region, levels = key_regions$region_label %>% unique()))
}

get_set_id <- function(log_data){
  # derive set_id from procedure / deprivation

  log_data %>%
    mutate(set_id = ifelse(procedure == 'multi_nacl' & deprivation == 'wr', 'wrnacl', NA))%>%
    mutate(set_id = ifelse(procedure == 'multi_s' & deprivation == 'wr', 'wrsuc', set_id))%>%
    mutate(set_id = ifelse(procedure == 'multi_sucrose' & deprivation == 'wr', 'wrsuc', set_id))%>%
    mutate(set_id = ifelse(procedure == 'multi_s' & deprivation == 'fr', 'frsuc', set_id))%>%
    mutate(set_id = ifelse(procedure == 'multi_sucrose' & deprivation == 'fr', 'frsuc', set_id))%>%
    mutate(set_id = ifelse(procedure == 'multi_misc' & deprivation == 'wr', 'wrmisc01', set_id))%>%
    mutate(set_id = ifelse(procedure == 'multi_misc02' & deprivation == 'wr', 'wrmisc02', set_id)) %>%
    mutate(set_id = ifelse(procedure == 'multi_water', 'wrwater', set_id)) %>%
    mutate(set_id = ifelse(procedure == 'multi_sucrose_satiation_1000', 'wrwatersat', set_id)) %>%
    mutate(set_id = ifelse(procedure == 'multi_sacc', 'frmultisacc', set_id))
}

get_solution_value <- function(df){
  df <- df %>%
    rowwise() %>%
    mutate(solution_value = ifelse(solution %>% str_detect('sucrose'), solution %>% str_remove('sucrose') %>% as.double(), NA)) %>%
    mutate(solution_value = ifelse(solution %>% str_detect('nacl'), solution %>% str_remove('nacl') %>% as.double() / 100, solution_value)) %>%
    mutate(solution_value = ifelse(solution %>% str_detect('water'), 0, solution_value)) %>%
    ungroup()

  # scaled solution value (-1 to 1)
  df <- df %>%
    mutate(solution_value_scaled_m1_p1 = solution_value / ((1/2) * max(solution_value)) - 1) %>%
    mutate(solution_value_scaled_m1_p1 = ifelse(solution %>% str_detect('nacl'), -solution_value_scaled_m1_p1, solution_value_scaled_m1_p1)) %>%
    mutate(solution_value_scaled_m1_p1 = ifelse(solution %>% str_detect('water'), -solution_value_scaled_m1_p1, solution_value_scaled_m1_p1))  # for water/nacl multispout

  # scaled solution value (0 to 1)
  df <- df %>%
    mutate(solution_value_scaled_0_p1 = solution_value / max(solution_value))%>%
    mutate(solution_value_scaled_0_p1 = ifelse(solution %>% str_detect('nacl'), -solution_value_scaled_0_p1, solution_value_scaled_0_p1))

  return(df)
}

filter_placements <- function(df, var_filt){

  log_placements_edited <- log_placements %>%
    filter(target == 'LHA') %>%
    mutate(target = 'LHA:GABA') %>%
    bind_rows(
      log_placements %>%
        filter(target == 'LHA') %>%
        mutate(target = 'LHA:Glut')) %>%
    bind_rows(
      log_placements %>%
        filter(target == 'LHA') %>%
        mutate(target = 'LHA:Ratio')) %>%
    bind_rows(
      log_placements %>%
        filter(target != 'LHA')
    )

  df %>%
    left_join(
      log_placements_edited %>%
        select(subject, region = target, !!as.name(var_filt), hit_ap, hit_ml, hit_dv),
      by = c('region', 'subject')
    ) %>%
    filter(!!as.name(var_filt) != 1) %>%
    select(-!!as.name(var_filt))


}

filter_subjects <- function(df, log_analysis_subjects, var_inclusion){
  df %>%
    left_join(log_analysis_subjects %>% select(subject, !!as.name(var_inclusion))) %>%
    filter(!!as.name(var_inclusion) == 1)
}

filter_qc_metrics <- function(df_streams, combined_qc_metrics, removal_type){

  # Step 1 (all removal types): drop any blockname x region row where
  # exclude_poor_signal == 1. This covers both STR and LHA regions — any fiber
  # flagged as poor signal for that session is removed here.
  # LHA:Ratio rows are NA in exclude_poor_signal (no direct QC entry) and are
  # preserved by the is.na() guard; they are handled by removal_type below.
  df_streams <- df_streams %>%
    left_join(combined_qc_metrics %>% select(blockname, region, exclude_poor_signal), by = c('blockname', 'region')) %>%
    filter(exclude_poor_signal == 0 | is.na(exclude_poor_signal))

  # Identify sessions where LHA:GABA or LHA:Glut failed QC. LHA:Ratio is a
  # derived signal and cannot be trusted if either component was excluded.
  bns_lha_ratio_to_remove <- combined_qc_metrics %>%
    filter(region %>% str_detect('lha')) %>%
    group_by(blockname) %>%
    filter(sum(exclude_poor_signal) > 0) %>%
    select(blockname) %>%
    unique() %>%
    pull(blockname)

  if(removal_type == 1){
    # Drop all LHA signals (including LHA:Ratio) for sessions where any LHA
    # component failed QC. STR signals from those sessions are kept.
    df_streams <- df_streams %>%
      filter(!((blockname %in% bns_lha_ratio_to_remove) & (region %>% str_detect('lha'))))
  }

  if(removal_type == 2){
    # Drop the entire session if any LHA component failed QC.
    df_streams <- df_streams %>%
      filter(!(blockname %in% bns_lha_ratio_to_remove))
  }

  df_streams <- df_streams %>%
    select(-exclude_poor_signal)

  return(df_streams)
}

join_placements <- function(df){
  log_placements_edited <- log_placements %>%
    filter(target == 'LHA') %>%
    mutate(target = 'LHA:GABA') %>%
    bind_rows(
      log_placements %>%
        filter(target == 'LHA') %>%
        mutate(target = 'LHA:Glut')) %>%
    bind_rows(
      log_placements %>%
        filter(target == 'LHA') %>%
        mutate(target = 'LHA:Ratio')) %>%
    bind_rows(
      log_placements %>%
        filter(target != 'LHA')
    )

  df %>%
    left_join(log_placements_edited %>% select(subject, region = target, hit_ap, hit_ml, hit_dv),
              by = join_by(subject, region))
}

format_regions <- function(df, var_region, key_regions){
  df %>%
    ungroup() %>%
    rename(region = !!as.name(var_region)) %>%
    left_join(key_regions, by = join_by(region)) %>%
    mutate(region_label = factor(region_label, levels = key_regions$region_label %>% unique())) %>%
    select(-region) %>%
    rename(!!var_region := region_label) %>%
    select(!!var_region, everything())
}

format_set_id <- function(df, key_set_id){
  df %>%
    ungroup() %>%
    left_join(key_set_id, by = join_by(set_id)) %>%
    mutate(set_id_label = factor(set_id_label, levels = key_set_id$set_id_label %>% unique())) %>%
    select(-set_id) %>%
    rename(set_id = set_id_label) %>%
    select(set_id, everything())
}

key_regions <- tibble(
  region =       c('lha_gaba', 'lha_glut', 'lha_ratio', 'nac',   'nac_corr', 'nacsh_med', 'nac_shelllat', 'nacsh_lat', 'dms', 'dls', 'ts'),
  region_label = c('LHA:GABA', 'LHA:Glut', 'LHA:Ratio', 'NAcCR', 'NAcCC',    'NAcShM',    'NAcShL',       'NAcShL',    'DMS', 'DLS', 'TS')
)

key_solutions <- tibble(
  solution =       c('sucrose00', 'sucrose05', 'sucrose10', 'sucrose20', 'sucrose30', 'nacl000', 'nacl025', 'nacl050', 'nacl100', 'nacl150'),
  solution_label = c('0',         '5',         '10',        '20',        '30',        '0.00',    '0.25',    '0.50',    '1.00',    '1.50'),
  solution_set   = c('sucrose',   'sucrose',   'sucrose',   'sucrose',   'sucrose',   'nacl',    'nacl',    'nacl',    'nacl',    'nacl')
)

format_solutions <- function(df, key_solutions){
  df %>%
    ungroup() %>%
    left_join(key_solutions, by = join_by(solution)) %>%
    mutate(solution_label = factor(solution_label, levels = key_solutions$solution_label %>% unique())) %>%
    select(-solution) %>%
    rename(solution = solution_label) %>%
    select(solution, everything())
}

key_set_id <- tibble(
  set_id =       c('wrsuc', 'frsuc', 'wrnacl'),
  set_id_label = c('WR:Suc', 'FR:Suc', 'WR:NaCl')
)
