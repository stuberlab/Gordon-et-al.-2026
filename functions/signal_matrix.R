generate_signal_matrix_batch <- function(dir_session, fns){
  # generates signal matrix and null matrix (from iti period) for fns in dir_session
  # produces 2 output files for each region/sensor (zscore and zscore_blsub)
  
  print('generating null matricies')
  
  for(fn in fns){
    print(str_c('signal - ', fn))
    # create directory if it does not exist
    dir_glm_signal <- str_c(dir_session, fn, '/glm/signals/')
    
    if(!dir.exists(dir_glm_signal)){
      dir.create(dir_glm_signal, recursive = T)
    }
    
    dir_glm_nulls <- str_c(dir_session, fn, '/glm/nulls/')
    if(!dir.exists(dir_glm_nulls)){
      dir.create(dir_glm_nulls, recursive = T)
    }
    
    # filter data to access period 
    df_signal <- read_feather(str_c('./data/sessions/', fn, '/', fn, '_streams_peth_preprocessed.feather')) %>%
      filter(event_id_char %in% c('spout_extended', 'access_period')) %>%
      mutate(event_id_char = 'access_period')
    
    df_signal_filt <- df_signal %>%
      filter(time_rel >= -3, time_rel < 6) %>%
      select(blockname, time, time_rel, region, zscore, zscore_blsub)
    
    # NULL: filter data to period prior to next access period
    # join in next trial start time
    df_signal_null <- df_signal %>%
      filter(time_rel == 0) %>%
      filter(region == df_signal$region[1]) %>%
      select(event_number, time_trial_start = time) %>%
      mutate(time_trial_start_next = lead(time_trial_start)) %>%
      left_join(df_signal,., by = join_by(event_number))
    
    # set last next trial start to the end of the session
    df_signal_null <- df_signal_null %>%
      mutate(time_trial_start_next = ifelse(is.na(time_trial_start_next), max(time), time_trial_start_next))
    
    # set next trial start to end of peth for trials with iti exceeding the necessary duration
    trials_extended_iti <- df_signal_null %>%
      select(event_number, event_ts, time, time_trial_start_next) %>%
      unique() %>%
      group_by(event_number) %>%
      summarise(count = sum(time>time_trial_start_next - 9)) %>%
      filter(count < 180)
    
    if(nrow(trials_extended_iti) > 0){
      
      trials_extended_iti_edits <- df_signal_null %>%
        filter(event_number %in% trials_extended_iti$event_number) %>%
        group_by(event_number) %>%
        filter(time == max(time)) %>%
        select(event_number, time) %>%
        unique()
      
      for(en in trials_extended_iti_edits$event_number){
        time_trial_start_next_replacement <- trials_extended_iti_edits$time[trials_extended_iti_edits$event_number == en]
        
        df_signal_null <- df_signal_null %>%
          mutate(time_trial_start_next = ifelse(event_number == en, time_trial_start_next_replacement, time_trial_start_next))
        
      }
      
    }
    
    # filter signal matrix to last 9s prior to next trial
    df_signal_null <- df_signal_null %>%
      group_by(event_number, region) %>%
      filter(time >= time_trial_start_next - 9, time < time_trial_start_next + 1/100) %>%
      mutate(count = n()) %>%
      mutate(filt_extra = ifelse(count > 180 & time_rel == max(time_rel), 1,0)) %>%
      filter(filt_extra == 0) %>%
      select(-count, -filt_extra) %>%
      ungroup()
    
    # shuffle event number and sort dataframe
    df_signal_null <- df_signal_null %>%
      select(event_number) %>%
      unique() %>%
      mutate(event_number_shuffle = sample(event_number)) %>%
      left_join(df_signal_null, ., by = join_by(event_number)) %>%
      select(-event_number) %>%
      rename(event_number = event_number_shuffle) %>%
      arrange(region, event_number, time_rel)
    
    signals <- df_signal_filt %>% 
      select(region) %>%
      unique() %>%
      pull(region)
    
    for(signal in signals){
      # write z-score blsub signal
      df_signal_filt %>%
        filter(region == signal) %>%
        select(zscore_blsub) %>%
        write_csv(str_c(dir_glm_signal, signal, '_zscoreblsub.csv'), col_names = F)

      # write z-score blsub null
      df_signal_null %>%
        select(region, zscore_blsub) %>%
        filter(region == signal) %>%
        select(zscore_blsub) %>%
        write_csv(str_c(dir_glm_nulls, signal, '_zscoreblsub.csv'), col_names = F)

    }
  }
}

generate_nulls_v02 <- function(blockname_region_filtered, dir_sessions, dir_null, region_ids){
  # function combines zscoreblsub null matricies for each region defined in region_ids
  # requires that all signal matricies have the same number of rows

  for(region in region_ids){

    region_label <- tibble(
      region = region) %>%
      get_region_labels() %>%
      mutate(region = region %>% as.character())

    if(region_label %>% str_detect('LHA')){
      fns <- blockname_region_filtered %>%
        filter(region %>% str_detect('LHA')) %>%
        pull(blockname)
    } else {
      fns <- blockname_region_filtered %>%
        filter(region == region_label$region) %>%
        pull(blockname)
    }

    # Pattern to match in the filenames
    filename_pattern <- str_c(region, '_zscoreblsub.csv')

    # List all files that match the pattern in the nulls directories
    file_paths <- list.files(path = dir_sessions, pattern = filename_pattern, full.names = TRUE, recursive = TRUE)

    # Filter to glm/nulls/ subdirectories only
    signal_files <- grep("/glm/nulls/", file_paths, value = TRUE)

    # Filter based on blocknames in fns
    filtered_files <- unlist(lapply(fns, function(fn) {
      grep(fn, signal_files, value = TRUE)
    }))

    print(str_c('combining: ', length(filtered_files), ' files- ', region))

    if (length(filtered_files) == 0) {
      warning(str_c('skipping: no null files found for region=', region))
      next
    }

    # combine files
    data_list <- combine_signals(filtered_files)

    df_combined      <- data_list[[1]]
    df_info_combined <- data_list[[2]]

    # save combined files
    df_combined      %>% write_csv(str_c(dir_null, region, '_zscoreblsub_null_data.csv'),       col_names = F)
    df_info_combined %>% write_csv(str_c(dir_null, region, '_zscoreblsub_null_blocknames.csv'))

  }
}
