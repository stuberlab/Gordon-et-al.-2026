# Title     : f_general_functions.R
# Objective : collect functions for functions that are used across multiple applications
# Created by: Adam Gordon-Fennell (agg2248@uw.edu)
#             Garret Stuber Lab, University of Washington
# Created on: 1/31/2022

# general --------------------------------------------------------------------------------------------------------------
# dir_diff (used in tidy_med, fp_preprocessing)
dir_diff <- function(dir_input, dir_output, list_suffix, print_message = 0, ignore_suffixes = NA){
  # return the files located in dir_input that are not already located in dir_output
  #
  # list_suffix provides strings that contain file name suffix + extension to be removed off of file name

  list_dir_input <- list.files(dir_input)
  list_dir_output <- list.files(dir_output)

  # filter out files in dir_output that contain suffix to ignore
  if(sum(!is.na(ignore_suffixes)) >= 1){
    for(ignore_suffix in ignore_suffixes){
      list_dir_output <- list_dir_output[!str_detect(list_dir_output, ignore_suffix)]
    }
  }

  for(suffix in list_suffix){
    list_dir_input <- list_dir_input %>%
      str_remove(suffix)

    list_dir_output <- list_dir_output %>%
      str_remove(suffix)
  }

  list_dir_input <- list_dir_input %>% unique()
  list_dir_output <- list_dir_output %>% unique()

  # if there are any files in the output directory that are not in the input directory, give a warning
  if(length(setdiff(list_dir_output, list_dir_input))){
    if(print_message){
      print("WARNING: THE FOLLOWING FILES ARE LOCATED IN THE dir_output THAT ARE NOT FOUND IN dir_input")
    }

    for(fn_missing in setdiff(list_dir_output, list_dir_input)){
      if(print_message){
        print(fn_missing)
      }
    }
    print("")
  }

  # create a list of files contained in dir_input but not dir_output
  fns <- setdiff(list_dir_input, list_dir_output)

  if(print_message){
    print(str_c("number of files not found in dir_output: ", length(fns)))
    print("")
  }

  return(fns)
}


# generic combine / append function
combine_files <- function(dir_input_tidy, fns, output_fn, suffix, append_combined){
  # combines multiple files into a single combined file. If the combined file already exists, new files will be appended
  # to the existing combined file
  #
  # dir_input_tidy: directory of input tidy data
  # fns: list of files within dir_input_tidy that we will combine
  # suffix: suffix of input files and output file (e.g. "_events.csv")
  # append_combined: 1: append to existing combined dataframe, 0: genreate from scratch and overwrite existing dataframe

  skip <- 0
   if(append_combined){
      if(file.exists(output_fn)){
        if(str_detect(suffix, 'csv')){fns_combined <- read_csv(output_fn, col_types = cols())}
        if(str_detect(suffix, 'feather')){fns_combined <- read_feather(output_fn)}

        fns_combined <- fns_combined %>% pull(med_file_name) %>% unique()

        fns_diff <- setdiff(fns %>% str_remove(suffix), fns_combined)

        if(length(fns_diff) > 0){
          fns_diff <- fns_diff %>%
            str_c(suffix)
        }

        if(length(fns_diff) == 0){
          print(str_c("all raw files already found in combined dataframe ", output_fn))
          skip <- 1
        } else {
          fns <- fns_diff
        }
      } else {
        fns_diff <- vector('character')
      }
    } else {
      print("append overwrite... creating new output dataframe")
      fns_diff <- vector('character')
    }

    if(length(fns) == 0){
      print(str_c("WARNING NO FILES FOUND WITH fn_string = ", fn_string, " & suffix = ", suffix))
      next
    }

  if(skip != 1){
      print(str_c("combining ", length(fns), " files with suffix ", suffix))

      first_file <- 1

      for(fn in fns){
        if(str_detect(fn, 'csv')){df_loop <- read_csv(str_c(dir_input_tidy, '/', fn), col_types = cols())}
        if(str_detect(fn, 'feather')){df_loop <- read_feather(str_c(dir_input_tidy, '/', fn))}
        if(!str_detect(fn, 'csv') & !str_detect(fn, 'feather')){
          print(str_c("WARNING INCOMPATABLIE FILE TYPE FOR FILE ", fn))
          next
        }

        if(first_file == 1){
          df_out <- df_loop
          first_file <- 0
        } else {
          df_out <- df_loop %>% bind_rows(df_out, .)
        }
      }

      print(str_c("writing: ", output_fn))

      if(str_detect(suffix, 'csv')){
        if(append_combined & length(fns_diff) > 0){
          print(output_fn)
          print(read_csv(output_fn))
          print(df_out)
          df_out %>% bind_rows(read_csv(output_fn, col_types = cols()),.) %>% write_csv(output_fn)
        } else {
          df_out %>% write_csv(output_fn)
        }
      }

      if(str_detect(suffix, 'feather')){
        if(append_combined & length(fns_diff) > 0){
          df_out %>% bind_rows(read_feather(output_fn, col_types = cols()),.) %>% write_feather(output_fn)
        } else {
          df_out %>% write_feather(output_fn)
        }
      }

      print("")
    }
  }


coerce_character <- function(t1, t2){
  # compare the datatypes of 2 dataframes and then convert numeric data to character if there is a disagreement
  #
  t1_dtype <- t1 %>%
    head() %>%
    collect() %>%
    lapply(class) %>%
    unlist() %>%
    as_tibble() %>%
    filter(!value %in% c('difftime', 'POSIXt')) %>% # filter out double data type for dates
    rename(t1_dtype = value) %>%
    mutate(variable = colnames(t1))


  t2_dtype <- t2 %>%
    head() %>%
    collect() %>%
    lapply(class) %>%
    unlist() %>%
    as_tibble() %>%
    filter(!value %in% c('difftime', 'POSIXt')) %>% # filter out double data type for dates
    rename(t2_dtype = value) %>%
    mutate(variable = colnames(t2))

  diff_t1_t2 <-
    suppressMessages(left_join(t1_dtype, t2_dtype)) %>%
    filter(t1_dtype != t2_dtype)

  if(nrow(diff_t1_t2 > 0)){
    for(n_var in 1:nrow(diff_t1_t2)){
      var_name <- diff_t1_t2$variable[n_var]
      if(diff_t1_t2$t1_dtype[n_var] == 'character'){
        t2 <- t2 %>% mutate_at(vars(var_name), as.character)
      }

      if(diff_t1_t2$t2_dtype[n_var] == 'character'){
        t1 <- t1 %>% mutate_at(vars(var_name), as.character)
      }
    }
  }

  return(list(t1, t2))
}



combined_import <- function(import_directory, filter_strings = NA, prefixes = NA, suffixes, verbose = 0){
  # combine a set of csv or feather files into a single dataframe
  #
  # inputs:
  # - import_directory: directory of files
  # - filter_strings (OPTIONAL): vector of strings to filter files in import_directory with
  # - prefix (OPTIONAL): vector of strings to filter files in import_directory with
  # - suffixes: vector of strings of suffixes you wish to combine
  #
  # output:
  # - list of dataframes that combine files with specified suffixes

    if(missing(verbose)) {
        verbose <- 0
    }

  if(verbose){
    print('combined import...')
    print('')
  }

  if(length(filter_strings) == 0){
    print('combined_import cancled: filter_strings has length of 0')
    return(0)
  }

  num_suffix <- 1

  for(suffix in suffixes){
    fns <- list.files(import_directory)

    if(verbose){
      print(str_c('number of files in ', import_directory, ': ', length(fns)))
      print('')
    }

    # filter fns to files with  strings included in filter_strings
    if(sum(!is.na(filter_strings)) > 0){
      for(filter_string in filter_strings){
        fns_loop <- fns[str_detect(fns, filter_string)]

        if(filter_string == filter_strings[1]){
          fns_combined <- fns_loop
        } else{
          fns_combined <- c(fns_combined, fns_loop)
        }
      }
    fns <- fns_combined
    }

    # filter fns to files with  strings included in prefixes
    if(sum(!is.na(prefixes)) > 0){
      for(prefix in prefixes){
        fns_loop <- fns[str_detect(fns, prefix)]

        if(prefix == prefixes[1]){
          fns_combined <- fns_loop
        } else{
          fns_combined <- c(fns_combined, fns_loop)
        }
      }
    fns <- fns_combined
    }

    # filter fns to files with suffix
    fns <- fns[str_detect(fns, suffix)]

    if(length(fns) == 0){
      print(str_c('No file names matching criteria found in directory ', import_directory))
      print('')
      print('check suffixes and extension')
      return(0)
    }

    if(verbose){
      print(str_c('combining ', length(fns), ' files'))
      print('')
    }

    # read in and combine files in fitler_list
    if(str_detect(suffix, 'feather')){
      for(fn in fns){
        if(verbose){print(fn)}

        data_loop <- read_feather(str_c(import_directory, '/', fn)) %>%
          mutate(fn = fn) %>%
          select(fn, everything())

        if(fn == fns[1]){
          data_combined <- data_loop
        } else {
          data_combined_list <- coerce_character(data_combined, data_loop)

          data_combined <- data_combined_list[[1]]
          data_loop     <- data_combined_list[[2]]

          data_combined <- data_loop %>% bind_rows(data_combined,.)
        }
      }
    }

    if(str_detect(suffix, 'csv')){
      for(fn in fns){
        if(verbose){print(fn)}

        data_loop <- read_csv(str_c(import_directory, '/', fn), col_types = cols()) %>%
          mutate(fn = fn) %>%
          select(fn, everything())

        if(fn == fns[1]){
          data_combined <- data_loop
        } else {
          data_combined_list <- coerce_character(data_combined, data_loop)

          data_combined <- data_combined_list[[1]]
          data_loop     <- data_combined_list[[2]]

          data_combined <- data_loop %>% bind_rows(data_combined,.)
        }
      }
    }

    if(num_suffix == 1){
      output_list <- list(data_combined)
      } else {
        output_list[[num_suffix]] <- data_combined
        }

    num_suffix <- num_suffix + 1
  }

  return(output_list)
}



format_dir <- function(x){
  if(str_sub(x, nchar(x), nchar(x)) != '/'){
    x <- x %>% str_c('/')
  }
  return(x)
}



# Get events -----------------------------------------------------------------------------------------------------------
get_event_bouts <- function(events, event_id_char_of_interest,  filt_tm, filt_n, filt_dir, id_char){
  # extract either onset or offsets of bouts of a chosen event based on the number of events prior/post (filt_n) within
  # a timeframe prior/post (filt_dir)
  #
  # inputs:
  #  - events (df): dataset from a single session that includes event_id_char (the id of the event) and event_ts (the
  #    time of the event
  #  - event_id_char_of_ineterest (char vector): vector of strings that contains the events that you want to use
  #  - filt_tm (double vector): time for window prior and post event that will be used
  #      - Units of this variable must match units for event_ts in events
  #  - filt_n (integer vector): number of events in the window prior and post that will be used
  #  - filt_dir(character vector): logical used to apply to time window ('>', '<', or '==')
  #  - id_char (string): string to replace event_id_char in output dataframe
  #
  # directions:
  #  - filt_tm, filt_n, and filt_dir each contain 2 values that apply to the window prior to and post each event
  #  - events will be filtered down to events that match the specified conditions
  #
  # example:
  #  - licking bout onsets
  #  - condition: no licks in 1s prior to first lick, at least 1 lick in 1s post lick
  #  input: events_get_event_bouts(events, c('lick'), c(1,1), c(0, 0), c('==', '>'), 'lick_onset')

  # filter to data that match the event of interest
  events_bout_onset <- events %>%
    filter(event_id_char %in% event_id_char_of_interest) %>%
    mutate(n_event_prior = 0,
           n_event_post  = 0) %>%
    arrange(event_ts)

  event_tss <- events_bout_onset %>%
    pull(event_ts)

  # for each time stamp, retrieve the number of events preceding and following
  for (event in 1:nrow(events_bout_onset)){
    events_bout_onset$n_event_prior[event] <-
      sum(
        event_tss > events_bout_onset$event_ts[event] - filt_tm[1] &
        event_tss < events_bout_onset$event_ts[event]
        )

    events_bout_onset$n_event_post[event] <-
      sum(
        event_tss > events_bout_onset$event_ts[event] &
        event_tss < events_bout_onset$event_ts[event] + filt_tm[2]
        )
  }

  # filter based on events prior
  if(filt_dir[1] == '<'){
    events_bout_onset <- events_bout_onset %>%
    filter(n_event_prior < filt_n[1])
  }

  if(filt_dir[1] == '>'){
    events_bout_onset <- events_bout_onset %>%
    filter(n_event_prior > filt_n[1])
  }

  if(filt_dir[1] == '=='){
    events_bout_onset <- events_bout_onset %>%
    filter(n_event_prior == filt_n[1])
  }

  # filter based on events post
  if(filt_dir[2] == '<'){
    events_bout_onset <- events_bout_onset %>%
    filter(n_event_post < filt_n[2])
  }

  if(filt_dir[2] == '>'){
    events_bout_onset <- events_bout_onset %>%
    filter(n_event_post > filt_n[2])
  }

  if(filt_dir[2] == '=='){
    events_bout_onset <- events_bout_onset %>%
    filter(n_event_post == filt_n[2])
  }

  # rename event_id_char so it can be combined with input dataset
  events_bout_onset <- events_bout_onset %>%
    mutate(event_id_char = id_char) %>%
    select(-n_event_prior, -n_event_post)

  return(events_bout_onset)
}

# time series functions ------------------------------------------------------------------------------------------------



get_peri_event <- function(df_trials, df_events, window) {
  # df_trials: one row per trial, with columns:
  #   - blockname     (chr): session identifier
  #   - trial_id      (chr): trial type label (e.g. 'trial_start')
  #   - trial_ts      (dbl): timestamp of the trial reference event
  #
  # df_events: one row per event, with columns:
  #   - blockname     (chr): session identifier, used to join with df_trials
  #   - event_id_char (chr): event type label (e.g. 'lick')
  #   - event_ts      (dbl): timestamp of the event
  #
  # window: numeric vector of length 2, c(start, end), in same units as timestamps

  df_trials %>%
    group_by(blockname, trial_id) %>%
    arrange(trial_ts, .by_group = TRUE) %>%
    mutate(trial_number = row_number()) %>%
    ungroup() %>%
    left_join(df_events, by = "blockname", relationship = "many-to-many") %>%
    mutate(time_rel = event_ts - trial_ts) %>%
    filter(time_rel >= window[1], time_rel <= window[2]) %>%
    select(blockname, trial_id, trial_number, trial_ts, event_id_char, event_ts, time_rel)
}

get_trial_lick_summary <- function(df_perievent, df_trials, window) {
  # df_perievent: output of get_peri_event()
  # df_trials:    original trials df, used to preserve trials with 0 licks
  # window:       numeric vector c(start, end) to apply to time_rel

  df_perievent %>%
    filter(time_rel >= window[1] & time_rel <= window[2]) %>%
    group_by(blockname, trial_id, trial_number, trial_ts) %>%
    summarise(
      lick_count   = n(),
      lick_latency = min(time_rel, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    right_join(
      df_trials %>%
        group_by(blockname, trial_id) %>%
        arrange(trial_ts, .by_group = TRUE) %>%
        mutate(trial_number = row_number()) %>%
        ungroup(),
      by = c("blockname", "trial_id", "trial_number", "trial_ts")
    ) %>%
    mutate(
      lick_count   = replace_na(lick_count, 0),
      lick_latency = na_if(lick_latency, Inf)
    ) %>%
    arrange(blockname, trial_id, trial_number)
}

get_peaks_threshold <- function(streams, var_time, var_dv, fs, filt_window, var_stream_id, peak_threshold){
  # return peak times from streams
  #
  # inputs
  #  streams (dataframe): time series data
  #  var_time(string): variable name of time within streams
  #  var_dv (string): variable name of signal within streams
  #  fs (double): sampling rate (Hz)
  #  filt_window(double): time window to filter double catches (set to 0 to return all peak times)
  #  var_stream_id(vector of strings): grouping variable names (e.g. c('blockname', 'channel_id'), or c('etl_plane', 'cell_number'))
  #  peak_threshold(double): threshold for peak detection (units match units of var_dv)

  streams <- streams  %>%
    rename(time = !!as.name(var_time))

  # return variable that denotes if sample is above peak_threshold
  streams <- streams %>%
    arrange_(c(var_stream_id, 'time')) %>%
    group_by(.dots = lapply(c(var_stream_id), as.symbol)) %>%
    mutate(above_threshold_window = ifelse(!!as.name(var_dv) >= peak_threshold,  1, 0)) %>%
    ungroup()

  # return peak onset times
  temp_peaks <- streams %>%
    group_by(.dots = lapply(c(var_stream_id), as.symbol)) %>%
    filter(above_threshold_window == 1) %>% # filter to samples above threshold
    mutate(time_step = time - lag(time)) %>% # compute time step
    mutate(onset_above_threshold_window  = ifelse(time_step > 1/fs + 1/(fs*10) | is.na(time_step), 1, NA)) %>% # filter to samples are not sequential
    filter(!is.na(onset_above_threshold_window)) %>%
    select(-time_step) %>%
    mutate(peak_num = row_number()) %>%
    ungroup()

  temp_peaks <- suppressMessages(left_join(streams,temp_peaks))


  # return peak points within periods above peak_threshold
  temp_peaks <- temp_peaks %>%
    group_by(.dots = lapply(c(var_stream_id), as.symbol)) %>%
    fill(peak_num) %>%
    group_by(.dots = lapply(c(var_stream_id, 'peak_num'), as.symbol)) %>%
    mutate(peak = ifelse(!!as.name(var_dv) == max(!!as.name(var_dv)), 1, 0)) %>% #
    ungroup()


  # filter peak times
  temp_peaks_filt <- temp_peaks %>%
    filter(peak == 1) %>%
    rename(event_ts = time) %>%
    mutate(event_id_char = 'peak') %>%
    group_by(.dots = lapply(c(var_stream_id), as.symbol)) %>%
    do(get_event_bouts(.,
                       event_id_char_of_interest = c('peak'),
                       filt_tm = c(filt_window, filt_window),
                       filt_n = c(0, 9999),
                       filt_dir = c("==", "<"),
                       id_char = 'filtered_peak'
                       )
       )%>%
      rename(time = event_ts) %>%
      select(var_stream_id, time, event_id_char) %>%
      left_join(temp_peaks,., by = c(var_stream_id, 'time')) %>%
    filter(event_id_char == 'filtered_peak') %>%
    group_by(.dots = lapply(c(var_stream_id), as.symbol)) %>%
    mutate(peak_num = row_number()) %>%
    rename(event_time = time) %>%
    select(-above_threshold_window, -onset_above_threshold_window, -peak, -event_id_char)

  return(temp_peaks_filt)
}

