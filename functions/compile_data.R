locally_save_multispout_data_behavior <- function(import_key, dir_localdata_sessions, overwrite = 0){
  # import_key (dataframe):
  #  - experiment (char)
  #  - blockname (char)
  #  - dir_arduino_processed (char)
  
  print("locally saving...")
  
  for(n_blockname in seq(1, nrow(import_key))){
    
    # save import_key vars for loop
    loop_experiment <- import_key$experiment[n_blockname]
    loop_blockname <- import_key$blockname[n_blockname]
    loop_dir_arduino_processed <- import_key$dir_arduino_processed[n_blockname]
    loop_dir_arduino_extracted <- import_key$dir_arduino_extracted[n_blockname]
    loop_procedure <- import_key$procedure[n_blockname]
    
    # generate folder for session
    dir_export <- str_c(dir_localdata_sessions,loop_blockname)
    
    
    # Check if the folder exists
    file_exists <- 1
    
    if (!dir.exists(dir_export)) {
      # Create the folder if it does not exist
      dir.create(dir_export, recursive = TRUE)
      file_exists <- 0
    }
    
    if(!file_exists | overwrite == 1){
      print(str_c("importing- ", loop_blockname))
      
      # import arduino data, trim, and save locally
      if(str_detect(loop_procedure, 'fa_')){ # if session is for free-access
        
        # import data
        data_beh_event <- read.csv(str_c(loop_dir_arduino_extracted, '/', loop_blockname, '_event.csv')) 
        
        # trim data
        data_beh_event <- data_beh_event %>%
          select(blockname, event_id_char, event_ts) %>%
          filter(event_id_char %in% c('start_session', 'lick', 'end_session'))
        
        # save data
        data_beh_event %>%
          write_csv(str_c(dir_export, '/', loop_blockname, '_event.csv'))
      }
      
      
      if(str_detect(loop_procedure, 'multi_')){ # if session is for multi-spout
        
        # import data
        data_beh_event <- read.csv(str_c(loop_dir_arduino_extracted, '/', loop_blockname, '_event.csv')) 
        data_beh_trial <- read.csv(str_c(loop_dir_arduino_processed, '/', loop_blockname, '_data_trial.csv')) 
        data_beh_trial_binned <- read.csv(str_c(loop_dir_arduino_processed, '/', loop_blockname, '_data_trial_binned.csv')) 
        data_beh_trial_summary <- read.csv(str_c(loop_dir_arduino_processed, '/', loop_blockname, '_data_trial_summary.csv')) 
        data_beh_spout_summary <- read.csv(str_c(loop_dir_arduino_processed, '/', loop_blockname, '_data_spout_summary.csv')) 
        data_beh_session_binned <- read.csv(str_c(loop_dir_arduino_processed, '/', loop_blockname, '_data_session_binned.csv')) 
        
        # trim data to near minimum number of variables
        data_beh_event <- data_beh_event %>%
          select(blockname, event_id_char, event_ts)
        
        data_beh_trial <- data_beh_trial %>%
          select(blockname, trial_num, solution, spout, event_ts, event_ts_rel)
        
        data_beh_trial_binned <- data_beh_trial_binned %>%
          select(blockname, trial_num, solution, spout, time_bin, count_binned)
        
        data_beh_trial_summary <- data_beh_trial_summary %>%
          select(-experiment, -cohort, -date, -solution_type, -solution_value)
        
        data_beh_spout_summary <- data_beh_spout_summary %>%
          select(-solution_type, -solution_value, -date, -cohort, -experiment)
        
        # save data
        data_beh_event %>%
          write_csv(str_c(dir_export, '/', loop_blockname, '_events.csv'))
        
        data_beh_trial %>%
          write_csv(str_c(dir_export, '/', loop_blockname, '_data_trial.csv'))
        
        data_beh_trial_binned %>%
          write_csv(str_c(dir_export, '/', loop_blockname, '_data_trial_binned.csv'))
        
        data_beh_trial_summary %>%
          write_csv(str_c(dir_export, '/', loop_blockname, '_data_trial_summary.csv'))
        
        data_beh_spout_summary %>%
          write_csv(str_c(dir_export, '/', loop_blockname, '_data_spout_summary.csv'))
        
        data_beh_session_binned %>%
          write_csv(str_c(dir_export, '/', loop_blockname, '_data_session_binned.csv'))
        
      } 
    }
  }
}

locally_save_multispout_data_fp_peth <- function(import_key, dir_localdata_sessions, dir_tdt, dir_npm, overwrite = 0){
  # import_key (dataframe):
  #  - experiment (char)
  #  - blockname (char)
  #  - dir_arduino_processed (char)
  
  print("locally saving fp data...")
  
  
  for(n_blockname in seq(1, nrow(import_key))){
    
    # save import_key vars for loop
    loop_experiment <- import_key$experiment[n_blockname]
    loop_blockname <- import_key$blockname[n_blockname]
    loop_fp_system <- import_key$imaging[n_blockname]
    loop_procedure <- import_key$procedure[n_blockname]
    
    # generate folder for session
    dir_export <- str_c(dir_localdata_sessions, loop_blockname)
    
    # Check if data is already in output folder
    file_exists <- 0
    fn_peth <- str_c(dir_export, '/', loop_blockname, '_streams_peth.feather')
    fn_event <- str_c(dir_export, '/', loop_blockname, '_fp_events.csv')
    
    if(file.exists(fn_peth) | file.exists(fn_event)){
      file_exists <- 1
    }
    
    # set import directory
    if(loop_fp_system == 'tdt'){dir_fp <- dir_tdt} 
    if(loop_fp_system == 'npm'){dir_fp <- dir_npm} 
    
    
    if(!file_exists | overwrite == 1){
      print(str_c("importing- ", loop_blockname))
      
      
      fn_streams <- str_c(dir_fp, 'data_processed/', loop_blockname, '_streams_peth.feather')
      
      if(!file.exists(fn_streams)){
        print(str_c('  MISSING STREAMS FILE /  skipped: ', fn_streams))
        next
      }
      
      # import data
      data_streams_peth <- read_feather(fn_streams) %>% ungroup()
      data_fp_events <- read_feather(str_c(dir_fp, 'data_processed/', loop_blockname, '_events.feather'))  %>% ungroup()
      
      # trim data to near minimum number of variables
      data_streams_peth <- data_streams_peth %>% 
        select(any_of(c("blockname", "fiber_id", "branch_id", "signal_wavelength", "event_id_char", "event_ts", "event_number", "time_rel", "time", "signal", "delta_signal_poly")))
      
      if(str_detect(loop_procedure, 'multi')){
        data_streams_peth %>% 
          filter(event_id_char %in% c('spout_extended', 'access_period')) %>%
          mutate(event_id_char = 'access_period')
      }
      
      data_fp_events <- data_fp_events %>%
        select(any_of(c("blockname", "event_id_char", "event_number", "event_ts"))) %>%
        filter(!is.na(event_id_char)) %>%
        filter(!event_id_char %in% c('tick_clock_out') ) %>%
        filter(!event_id_char %>% str_detect('sol_'))
      
      # save data
      data_streams_peth %>%
        write_feather(str_c(dir_export, '/', loop_blockname, '_streams_peth.feather'))
      
      data_fp_events %>%
        write_csv(str_c(dir_export, '/', loop_blockname, '_fp_events.csv'))
    }
  }
}


locally_save_multispout_data_fp_baseline <- function(import_key, dir_localdata_sessions, dir_tdt, dir_npm, blockname_baselines, overwrite = 0){
  # import_key (dataframe):
  #  - experiment (char)
  #  - blockname (char)
  #
  # blockname_baselines
  #  - blocknames of sessions with independent baseline recordings

  print("locally saving fp baseline data...")

  for(n_blockname in seq(1, nrow(import_key))){

    loop_blockname <- import_key$blockname[n_blockname]
    loop_fp_system <- import_key$imaging[n_blockname]
    loop_procedure  <- import_key$procedure[n_blockname]

    dir_export <- str_c(dir_localdata_sessions, loop_blockname)

    file_exists <- 0
    if(file.exists(str_c(dir_export, '/', loop_blockname, '_streams_baseline.feather'))){
      file_exists <- 1
    }

    if(file.exists(str_c(dir_export, '/', loop_blockname, '_fp_events.csv'))){
      fp_events <- read.csv(str_c(dir_export, '/', loop_blockname, '_fp_events.csv'))
    } else {
      print(str_c("warning! ", loop_blockname, ' session folder does not contain _fp_events.csv'))
    }

    if(loop_fp_system == 'tdt'){dir_fp <- dir_tdt}
    if(loop_fp_system == 'npm'){dir_fp <- dir_npm}

    if(!file_exists | overwrite == 1){

      if(!loop_blockname %in% blockname_baselines){
        data_streams_session <- read_feather(str_c(dir_fp, 'data_processed/', loop_blockname, '_streams_session.feather')) %>% ungroup()

        ts_first_access <- fp_events %>%
          ungroup() %>%
          filter(event_id_char %>% str_detect('lick') | event_id_char %>% str_detect('access') | event_id_char %>% str_detect('trial_start')) %>%
          filter(event_ts == min(event_ts)) %>%
          pull(event_ts) %>%
          unique()

      } else {
        data_streams_session <- read_feather(str_c(dir_fp, 'data_processed/', loop_blockname, '_baseline_streams_session.feather')) %>% ungroup()

        ts_first_access <- data_streams_session %>% pull(time) %>% max()
      }

      data_streams_session <- data_streams_session %>%
        select(any_of(c("blockname", "fiber_id", "branch_id", "signal_wavelength", "time", "signal", "delta_signal_poly")))

      # filter to time prior to first task event
      data_streams_session <- data_streams_session %>%
        filter(time < ts_first_access)

      print(str_c(loop_blockname, ' - ', loop_fp_system, ' - baseline end: ', ts_first_access))

      data_streams_session %>%
        write_feather(str_c(dir_export, '/', loop_blockname, '_streams_baseline.feather'))
    }
  }
}


multispout_preprocess_fp_peth <- function(import_key, dir_localdata_sessions, log_tdt, log_npm, overwrite = 0){
  
  # region lookup for tdt sessions; branch_id cast to integer then character
  # to strip leading zeros and match values stored in peth feathers ('1' not '01')
  tdt_branch <- log_tdt %>%
    select(blockname, fiber_id01, fiber_id02) %>%
    gather('branch_id', 'region', starts_with('fiber')) %>%
    mutate(branch_id = str_sub(branch_id, 9,10) %>% as.integer() %>% as.character())

  for(n_blockname in seq(1, nrow(import_key))){
    loop_blockname <- import_key$blockname[n_blockname]
    loop_system <- import_key$imaging[n_blockname]

    fn_out <- str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_streams_peth_preprocessed.feather')

    if(file.exists(fn_out) & overwrite == 0){
      print(str_c(loop_blockname, ' - skipped'))
      next
    }

    print(str_c('preprocessing fp - ', loop_blockname))

    streams_peth <- read_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_streams_peth.feather'))

    # filter out first event for sessions with TDT recording of arduino session start
    streams_peth <- streams_peth %>%
      mutate(event_number_count = max(event_number)) %>%
      mutate(event_number = ifelse(event_number_count == 101, event_number - 1, event_number)) %>%
      filter(event_number != 0 & event_number <= 100) %>%
      select(-event_number_count)

    # join in brain region ids
    if("fiber_id" %in% names(streams_peth)) {
      streams_peth <- streams_peth %>% rename(branch_id = fiber_id)
    }
    streams_peth <- streams_peth %>% mutate(branch_id = as.character(branch_id))

    if(loop_system == 'npm'){
      streams_peth <- streams_peth %>%
        left_join(log_npm %>% mutate(branch_id = as.character(branch_id)) %>% select(blockname, branch_id, region), by = c('blockname', 'branch_id'))
    }

    if(loop_system == 'tdt'){
      streams_peth <- streams_peth %>%
        left_join(tdt_branch, by = c('blockname', 'branch_id'))
    }
    
    # aaw cohort: lha only — assign regions by wavelength (no log entry needed)
    streams_peth <- streams_peth %>%
      mutate(region = ifelse(blockname %>% str_detect('aaw') & signal_wavelength == 465, 'lha_gaba', region)) %>%
      mutate(region = ifelse(blockname %>% str_detect('aaw') & signal_wavelength == 560, 'lha_glut', region))
    
    # acd cohort: single-color nac recording — discard red channel
    if(str_detect(loop_blockname, 'acd')){
      streams_peth <- streams_peth %>%
        filter(signal_wavelength == 470)
    }

    # acr cohort: lha dual-color + striatum — assign lha regions by wavelength,
    # discard red channel for striatal fibers (single-color)

    streams_peth <- streams_peth %>%
      mutate(region = ifelse(
        blockname %>% str_detect('acr') & 
          region %>% str_detect('lha') & 
          signal_wavelength == 470, 
        'lha_gaba', region)) %>%
      mutate(region = ifelse(
        blockname %>% str_detect('acr') & 
          region %>% str_detect('lha') & 
          signal_wavelength == 560, 
        'lha_glut', region)) %>%
      filter(
        region %>% str_detect('lha') |
          (!region %>% str_detect('lha') & signal_wavelength %in% c(465, 470))
      )
    
    # compute z-score
    streams_peth <- streams_peth %>%
      filter(time_rel >= -3, time_rel < 0) %>%
      group_by(blockname, region) %>%
      summarise(delta_signal_poly_mean = delta_signal_poly %>% mean(),
                delta_signal_poly_sd   = delta_signal_poly %>% sd(), 
                .groups = 'drop') %>%
      left_join(streams_peth, by = c('blockname', 'region')) %>%
      mutate(zscore = (delta_signal_poly - delta_signal_poly_mean) / delta_signal_poly_sd) %>%
      select(-delta_signal_poly_mean, -delta_signal_poly_sd)
    
    # compute lha_ratio: z-scored gaba/glut ratio signal
    if (all(c("lha_gaba", "lha_glut") %in% streams_peth$region)){
      streams_peth_ratio <- streams_peth  %>%
        select(blockname, branch_id, region, event_id_char, event_ts, event_number, time, time_rel, zscore)

      streams_peth_ratio <- streams_peth_ratio  %>%
        filter(region %in% c('lha_gaba', 'lha_glut')) %>%
        spread(region, zscore) %>%
        mutate(zscore  = (100 + lha_gaba) / (100 + lha_glut)) %>%
        select(-lha_gaba, -lha_glut) %>%
        mutate(region = 'lha_ratio')
      
      streams_peth_ratio <- streams_peth_ratio %>%
        filter(time_rel >= -3, time_rel < 0) %>%
        group_by(blockname, region) %>%
        summarise(zscore_mean = zscore %>% mean(),
                  zscore_sd   = zscore %>% sd(), 
                  .groups = 'drop') %>%
        left_join(streams_peth_ratio, by = c('blockname', 'region')) %>%
        mutate(zscore = (zscore - zscore_mean) / zscore_sd) %>%
        select(-zscore_mean, -zscore_sd)
      
      streams_peth <- streams_peth %>%
        bind_rows(streams_peth_ratio)
    }
    
    # compute baseline subtraction for peth
    streams_peth <- streams_peth %>% 
      filter(time_rel >= -3, time_rel < 0) %>%
      group_by(blockname, event_id_char, branch_id, region, event_number) %>%
      summarise(zscore_bl = mean(zscore), .groups = 'drop') %>%
      left_join(streams_peth,., by = c("blockname", "region", "branch_id", "event_id_char", "event_number")) %>%
      mutate(zscore_blsub = zscore - zscore_bl) %>%
      select(-zscore_bl)
    
    streams_peth %>% write_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_streams_peth_preprocessed.feather'))
  }
}


multispout_preprocess_fp_baseline <- function(import_key, dir_localdata_sessions, log_tdt, log_npm){

  # region lookup for tdt sessions; branch_id cast to integer then character
  # to strip leading zeros and match values stored in baseline feathers
  tdt_branch <- log_tdt %>%
    select(blockname, fiber_id01, fiber_id02) %>%
    gather('branch_id', 'region', starts_with('fiber')) %>%
    mutate(branch_id = str_sub(branch_id, 9, 10) %>% as.integer() %>% as.character())

  for(n_blockname in seq(1, nrow(import_key))){
    loop_blockname <- import_key$blockname[n_blockname]
    loop_system <- import_key$imaging[n_blockname]

    print(str_c('preprocessing fp baseline - ', loop_blockname))

    streams_bl <- read_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_streams_baseline.feather'))

    # join in brain region ids
    if("fiber_id" %in% names(streams_bl)) {
      streams_bl <- streams_bl %>% rename(branch_id = fiber_id)
    }
    streams_bl <- streams_bl %>% mutate(branch_id = as.character(branch_id))

    if(loop_system == 'npm'){
      streams_bl <- streams_bl %>%
        left_join(log_npm %>% mutate(branch_id = as.character(branch_id)) %>% select(blockname, branch_id, region), by = c('blockname', 'branch_id'))
    }

    if(loop_system == 'tdt'){
      streams_bl <- streams_bl %>%
        left_join(tdt_branch, by = c('blockname', 'branch_id'))
    }

    # aaw cohort: lha only — assign regions by wavelength
    streams_bl <- streams_bl %>%
      mutate(region = ifelse(blockname %>% str_detect('aaw') & signal_wavelength == 465, 'lha_gaba', region)) %>%
      mutate(region = ifelse(blockname %>% str_detect('aaw') & signal_wavelength == 560, 'lha_glut', region))

    # acd cohort: single-color nac recording — discard red channel
    if(str_detect(loop_blockname, 'acd')){
      streams_bl <- streams_bl %>%
        filter(signal_wavelength == 470)
    }

    # acr cohort: lha dual-color + striatum — assign lha regions by wavelength
    if(str_detect(loop_blockname, 'acr')){
      streams_bl <- streams_bl %>%
        mutate(region = ifelse(
          blockname %>% str_detect('acr') &
            region %>% str_detect('lha') &
            signal_wavelength == 470,
          'lha_gaba', region)) %>%
        mutate(region = ifelse(
          blockname %>% str_detect('acr') &
            region %>% str_detect('lha') &
            signal_wavelength == 560,
          'lha_glut', region)) %>%
        filter(
          region %>% str_detect('lha') |
            (!region %>% str_detect('lha') & signal_wavelength %in% c(465, 470))
        )
    }

    # compute z-score
    streams_bl <- streams_bl %>%
      group_by(region) %>%
      mutate(zscore = (delta_signal_poly - mean(delta_signal_poly)) / sd(delta_signal_poly))

    # compute lha_ratio: z-scored gaba/glut ratio signal
    if (all(c("lha_gaba", "lha_glut") %in% streams_bl$region)){
      streams_bl_ratio <- streams_bl %>%
        select(blockname, branch_id, region, time, zscore) %>%
        filter(region %in% c('lha_gaba', 'lha_glut')) %>%
        spread(region, zscore) %>%
        mutate(zscore = (100 + lha_gaba) / (100 + lha_glut)) %>%
        select(-lha_gaba, -lha_glut) %>%
        mutate(region = 'lha_ratio') %>%
        group_by(region) %>%
        mutate(zscore = (zscore - mean(zscore)) / sd(zscore))

      streams_bl <- streams_bl %>%
        bind_rows(streams_bl_ratio)
    }

    streams_bl %>% write_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_streams_baseline_preprocessed.feather'))
  }
}


get_signal_quality_control <- function(dir_localdata_sessions, overwrite){
  print('calculating quality metrics from baseline recordings')
  
  dir_sessions <- list.dirs(dir_localdata_sessions, recursive = F)
  
  for(dir_session in dir_sessions){
    loop_blockname <- sub(".*/", "", dir_session) 
    
    fn_qc <- str_c(loop_blockname, '_quality_metrics.csv')
    
    fns <- list.files(dir_session, recursive = F)
    
    if(sum(fn_qc %in% fns) == 1 & overwrite == 0){
      print(str_c(loop_blockname, '- skipped'))
      
    }else{
      print(str_c(loop_blockname))
      
      fn <- str_c(dir_session, '/', loop_blockname, '_streams_baseline_preprocessed.feather')
      
      if(!file.exists(fn)){
        print(str_c(loop_blockname, '- file missing: ', fn))
        next
      }
      
      streams_qc <- read_feather(fn) %>%
        filter(region != 'lha_ratio')
      
      
      # count the number of samples at each raw signal value
      filt_sample_frac <- streams_qc %>%
        group_by(blockname, region, branch_id, signal) %>%
        summarise(sample_count = n(), .groups = 'drop') 
      
      # determine the maximum proportion of samples detected at a given signal value (accounts for floor / ceil)
      filt_sample_frac <- filt_sample_frac %>%
        arrange(blockname, region, branch_id, signal) %>%
        group_by(blockname, region, branch_id) %>%
        mutate(
          sample_rank = row_number(), 
          sample_count_total = sum(sample_count)
        ) %>%
        mutate(sample_frac = sample_count / sample_count_total) %>%
        filter(sample_frac == max(sample_frac)) %>%
        select(blockname, region, branch_id, sample_frac) %>%
        unique() 
      
      filt_sample_frac %>%
        mutate(exclude_poor_signal = ifelse(sample_frac > 0.05,1,0)) %>%
        write_csv(str_c(dir_session, '/', fn_qc))
      
    }
  } 
}


bin_access_time <- function(streams_peth, bin_tms, bin_ids){
  streams_peth %>%
    mutate(time_bin = cut(time_rel, bin_tms, label = bin_ids)) %>%
    filter(!is.na(time_bin))
}


summarise_sessions <- function(import_key, dir_localdata_sessions, overwrite = 0){
  
  for(n_blockname in seq(1, nrow(import_key))){
    loop_blockname <- import_key$blockname[n_blockname]
    
    print(str_c('summarise session - ', loop_blockname))
    
    # read in stream data and trial information
    streams_peth <- read_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_streams_peth_preprocessed.feather'))
    beh_trial_summary <- read.csv(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_data_trial_summary.csv')) 
    
    # join trial info
    streams_peth <- streams_peth %>%
      rename(trial_num = event_number) %>%
      left_join(beh_trial_summary, by = c('blockname', 'trial_num'))
    
    peth01 <- streams_peth %>%
      bin_access_time(c(-3,0,3), c('pre', 'access'))
    
    peth02 <- streams_peth %>%
      bin_access_time(c(-3,0,0.2,1.5,3), c('pre', 'anticipation', 'valuation', 'sustained'))
    
    summary_peth01_trial <- peth01 %>%
      group_by_at(vars(-time, -time_rel, -signal, -zscore, -delta_signal_poly, -zscore_blsub)) %>%
      summarise(zscore_mean = zscore_blsub %>% mean(), .groups = 'drop')
    
    summary_peth01_trial %>% write_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_summary_peth01_trial.feather'))
    
    
    summary_peth02_trial <- peth02 %>%
      group_by_at(vars(-time, -time_rel, -signal, -zscore, -delta_signal_poly, -zscore_blsub)) %>%
      summarise(zscore_mean = zscore_blsub %>% mean(), .groups = 'drop')
    
    summary_peth02_trial %>% write_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_summary_peth02_trial.feather'))
    
    # trace averages
    summary_trace_solution <- streams_peth %>%
      filter(trial_lick == 1) %>%
      group_by(blockname, region, time_rel, solution) %>%
      mutate(trial_count = n_distinct(trial_num)) %>%
      summarise(
        trial_count = mean(trial_count), 
        zscore_sd = zscore_blsub %>% sd(),
        zscore_mean = zscore_blsub %>% mean(),
        .groups = 'drop'
      )
    
    summary_trace_solution %>% write_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_summary_trace_solution.feather'))
    
    summary_trace_spout <- streams_peth %>%
      filter(trial_lick == 1) %>%
      group_by(blockname, region, time_rel, spout) %>%
      mutate(trial_count = n_distinct(trial_num)) %>%
      summarise(
        trial_count = mean(trial_count), 
        zscore_sd = zscore_blsub %>% sd(),
        zscore_mean = zscore_blsub %>% mean(),
        .groups = 'drop'
      )
    
    summary_trace_spout %>% write_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_summary_trace_spout.feather'))
    
    summary_trace_triallick <- streams_peth %>%
      group_by(blockname, region, time_rel, trial_lick) %>%
      mutate(trial_count = n_distinct(trial_num)) %>%
      summarise(
        trial_count = mean(trial_count), 
        zscore_sd = zscore_blsub %>% sd(),
        zscore_mean = zscore_blsub %>% mean(), 
        .groups = 'drop'
      )
    
    summary_trace_triallick %>% write_feather(str_c(dir_localdata_sessions, loop_blockname, '/', loop_blockname, '_summary_trace_triallick.feather'))
    
  }
}








import_arduino <- function(manual_blocknames, dir_arduino_raw, dir_arduino_raw_local, overwrite = 0){
  if (!dir.exists(dir_arduino_raw_local)) {
    dir.create(dir_arduino_raw_local, recursive = TRUE)
  }

  for(loop_blockname in manual_blocknames){
    fn_input <- str_c(dir_arduino_raw, '/', loop_blockname, '.csv')
    fn_output <- str_c(dir_arduino_raw_local, '/', loop_blockname, '.csv')
    
    if(file.exists(fn_input)){
      if(!file.exists(fn_output) | overwrite == 1){
        print(str_c(loop_blockname, ' - importing'))
        df <- read.csv(fn_input, header = FALSE)
        
        # arduino files store the header as the first data row, not as colnames
        tr <- df$V1[1]
        df <- df %>% tail(nrow(df)-1)
        colnames(df) <- tr
        
        df %>%
          write_csv(str_c(dir_arduino_raw_local, '/', loop_blockname, '.csv'))
      } 
      
      
    } else {
      print(str_c(loop_blockname, ' - MISSING in RAW'))
      
    }
  }
}





