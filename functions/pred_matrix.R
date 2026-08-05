
get_lagged_cols <- function(data, column_name, max_lag, lag_direction, fill_zeros) {
  # This function creates new columns in a data frame to represent lagged (historical) or lead (future) values
  # of a specified column. 
  #
  # Parameters:
  #   - data: A data frame containing the original data.
  #   - column_name: The name of the column for which lagged or lead columns should be created.
  #   - max_lag: The maximum number of lagged or lead periods to create.
  #   - lag_direction: A numeric value indicating the direction of the lag/lead.
  #                    Use 1 for lagged columns (past values) and -1 for lead columns (future values).
  #   - fill_zeros: A binary value (0 or 1) indicating whether to fill NA values with zeros.
  #                 Use 1 to fill NA values with zeros, and 0 to leave them as NA.
  #
  # Returns:
  #   - A data frame with the original data and the new lagged or lead columns added.
  #
  # Example:
  #   df <- data.frame(time = seq(as.Date('2020-01-01'), as.Date('2020-01-10'), by = "day"),
  #                    value = rnorm(10))
  #   df_lagged <- get_lagged_cols(df, "value", 2, 1, 1)
  #   df_lead <- get_lagged_cols(df, "value", 2, -1, 1)
  
  # prepare data
  data <- data %>%
    mutate(!!column_name := !!sym(column_name) %>% as.double())
  
  # Loop to create lagged or lead columns based on the specified direction
  for (lag in 1:max_lag) {
    # Generate the new column name based on lag or lead and the current lag value
    lagged_column_name <- paste0(column_name, ifelse(lag_direction == 1, "_lag", "_lead"), sprintf("%02d", lag))
    
    # Create the lagged or lead column using the appropriate `dplyr` function
    data <- data %>% 
      mutate(!!lagged_column_name := if(lag_direction == 1) lag(!!sym(column_name), lag) else lead(!!sym(column_name), lag))
    
    # Optionally replace NA values in the new column with zeros
    if (fill_zeros == 1) {
      data <- data %>%
        mutate(!!lagged_column_name := if_else(is.na(!!sym(lagged_column_name)), 0, !!sym(lagged_column_name)))
    }
  }
  
  return(data)
}

reorder_laglead <- function(df) {
  # Reorder Data Frame Columns Based on Lag and Lead Suffixes
  # This function reorders the columns of a data frame based on their lag and lead suffixes.
  # Columns without lag or lead suffixes are placed at the beginning, followed by lagged columns in ascending order,
  # and lead columns in descending order.
  # Parameters:
  #   - df: A data frame whose columns need to be reordered based on lag and lead suffixes.
  # Returns:
  #   - A data frame with columns reordered according to their lag and lead suffixes.
  
  # Create a tibble with original column names
  
  df_colnames <- tibble(
    col_org = df %>% colnames()
  )
  
  # Assign sorting values to columns based on lag or lead suffix and reorder them
  reorder_colnames <- df_colnames %>%
    # Extract integer part from suffixes, suppress warnings for non-integer conversions
    mutate(sort_value = suppressWarnings(as.integer(str_sub(col_org, -2)))) %>%
    # Replace NA (non-suffix columns) with 0 to prioritize them in sorting
    mutate(sort_value = ifelse(is.na(sort_value), 0, sort_value)) %>%
    # Negate sort value for lead columns to sort them separately from lag columns
    mutate(sort_value = ifelse(col_org %>% str_detect('_lead'), -sort_value, sort_value)) %>%
    # Arrange column names based on calculated sort values
    arrange(sort_value) %>%
    # Extract sorted column names
    pull(col_org)
  
  # Reorder columns in the data frame based on sorted column names
  df_reordered <- df %>%
    select(all_of(reorder_colnames))
  
  return(df_reordered)
}


get_multispout_value_history <- function(df, var_name, max_n) {
  # Function to create rolling mean columns based on var_name for various window sizes
  
  
  # Ensure the dataframe is ordered by 'trial_num' to correctly compute rolling means
  df <- df %>% arrange(trial_num)
  
  # Loop through each window size from 1 to 'max_n' to create rolling mean columns
  for (n in max_n){#1:max_n) {
    # Define the column name for the rolling mean, formatted with leading zeros for consistency
    roll_mean_col_name <- sprintf("_history%02d", n)
    roll_mean_col_name <- roll_mean_col_name %>% str_c(var_name, .)
    
    # Compute the rolling mean for 'solution_value' with a window size of 'n'
    # 'rollapply' is used from the 'zoo' package (ensure it's installed and loaded)
    # 'align = right' ensures that the mean is calculated for the window ending at the current row
    # 'partial = TRUE' allows for the computation of means in windows smaller than 'n' at the start of the data
    df[[roll_mean_col_name]] <- rollapply(df %>% pull(var_name), width = n, FUN = mean, 
                                          align = 'right', fill = NA, partial = TRUE)
    
    # Lag the computed rolling means by 1 to exclude the current trial's value from its own mean
    # This step shifts the rolling mean values down by one row
    df[[roll_mean_col_name]] <- lag(df[[roll_mean_col_name]], n = 1)
    
    # Replace any NA values resulting from the lag operation with 0
    # This is particularly relevant for the first 'n' rows where the lag would introduce NAs
    df <- df %>%
      mutate(!!as.name(roll_mean_col_name) := ifelse(
        is.na(!!as.name(roll_mean_col_name)), 
        0, 
        !!as.name(roll_mean_col_name)
      ))
  }
  
  return(df)
}

convolve_licks <- function(lick_vector, kernel_length, sd){
  # Function to convolve a vector of lick events with a half-normal distribution kernel
  
  
  # Generate a sequence from 0 to 'kernel_length' and create a half-normal distribution kernel
  # based on a standard normal distribution, truncated at 0 (hence, half-normal)
  kernel <- dnorm(0:kernel_length, mean = 0, sd = sd)
  
  # Normalize the kernel so that its sum equals 1, ensuring that the convolution
  # does not change the overall scale of the lick vector
  kernel <- kernel / sum(kernel)
  
  # The padding size is set to 'kernel_length' to prevent edge effects during convolution
  pad_size <- kernel_length
  
  # Convolve the lick vector with the reversed kernel using 'type = filter'
  # This convolution type is linear and does not wrap around at the edges, which is
  # often desired for time series analysis to avoid artificial introduction of values from the end of the vector to the beginning
  result <- convolve(lick_vector, rev(kernel), type = 'filter')
  
  # Ensure that the convolution result matches the length of the input 'lick_vector'
  # In some cases, the convolution operation might alter the length of the output
  if (length(result) != length(lick_vector)) {
    # If the result's length is different, pad it with zeros on the right to match
    # This step is taken to maintain the alignment of the convolution result with the original time series
    result <- c(rep(0, pad_size), result)
  }
  
  # Truncate or pad the result to ensure its length is exactly the same as the input 'lick_vector'
  # This is crucial for maintaining consistent vector lengths for further analysis
  result <- result[1:length(lick_vector)]
  
  return(result)
}

get_prediction_matrix_multispout <- function(df_event, df_trial, blockname_loop){
  # returns prediction matrix for a single multi-spout session
  
  # filter out possible onset/offsets
  df_event <- df_event %>%
    filter(!event_id_char %>% str_detect('_onset'))%>%
    filter(!event_id_char %>% str_detect('_offset'))
  
  # adjust events ids to be consistent 
  df_joined <- df_event %>%
    mutate(event_id_char = ifelse(event_id_char == 'access_period', 'spout_extended', event_id_char)) %>%
    mutate(event_id_char = ifelse(event_id_char %>% str_detect('lick'), 'lick', event_id_char)) %>%
    mutate(trial_num = ifelse(event_id_char == 'spout_extended', event_number, NA)) %>%
    left_join(df_trial %>% select(blockname, trial_num, spout, solution), by = c("blockname", "trial_num")) %>% 
    fill(trial_num, spout, solution)
  
  # add in derived events
  df_joined <- df_joined %>%
    filter(event_id_char == 'spout_extended') %>%
    mutate(event_ts = event_ts + 3) %>%
    mutate(event_id_char = 'spout_retracted') %>%
    bind_rows(df_joined) %>%
    arrange(blockname, event_ts)
  
  # add in derived events
  df_joined <- df_joined %>%
    filter(event_id_char == 'spout_retracted') %>%
    mutate(event_ts = event_ts + 0.7) %>%
    mutate(event_id_char = 'radial_positioned') %>%
    bind_rows(df_joined) %>%
    arrange(blockname, event_ts)
  
  # compute  time rel
  df_joined <- df_joined %>%
    filter(event_id_char == 'spout_extended') %>%
    select(blockname, trial_num, trial_start_ts = event_ts) %>%
    left_join(df_joined,., by = c("blockname", "trial_num")) %>%
    mutate(event_ts_rel = event_ts - trial_start_ts) 
  
  df <- df_joined
  
  sampling_rate <- 20 #hz
  
  tm_max <- df %>%
    pull(event_ts_rel) %>%
    max()
  
  tm_bins <- seq(-3, 6 - 1/sampling_rate, 1/sampling_rate)
  
  tm_bins <-  data.frame(tm_bin = tm_bins)
  
  # gather events
  df <- df %>%
    select(blockname, trial_num, event_ts_rel, event_ts, event_id_char, spout, solution) %>%
    gather('set', 'event_id_char', event_id_char:solution) %>%
    filter(!is.na(event_id_char)) %>%
    mutate(event_id_char = ifelse(event_id_char %>% str_detect('lick'), 'lick', event_id_char)) %>%
    arrange(blockname, event_ts) %>%
    select(-set)
  
  # bin event_ts_rel
  df <- df %>%
    select(blockname, trial_num, event_id_char, event_ts_rel) %>%
    mutate(tm_bin = cut(event_ts_rel, tm_bins$tm_bin, label = tm_bins$tm_bin[1:nrow(tm_bins)-1])) %>%
    filter(!is.na(tm_bin))
  
  df <- df %>%
    mutate(tm_bin = tm_bin %>% as.character() %>% as.double() %>% as.character())
  
  df <- df %>%
    group_by(blockname, trial_num, tm_bin, event_id_char) %>%
    summarise(count = n(), .groups = 'drop') %>%
    spread(key = event_id_char, value = count, fill = 0)
  
  df_complete <- df %>%
    select(blockname, trial_num) %>%
    unique()
  
  suppressWarnings( # supress many-to-many warning
    df_complete <- df %>%
      filter(blockname == blockname_loop) %>%
      select(blockname, trial_num) %>%
      unique() %>%
      left_join(tm_bins %>% mutate(blockname = blockname_loop), by = join_by(blockname)) %>%
      mutate(tm_bin = as.character(tm_bin))
  )
  
  df_complete <- df_complete %>%
    left_join(df, by = c("blockname", "trial_num", "tm_bin")) 
  
  df_complete[is.na(df_complete)] <- 0
  
  # edit df_complete
  df_complete <- df_complete %>%
    mutate(spout_extended = ifelse(tm_bin == 0, 1, 0)) 
  
  # derive access period
  df_complete <- df_complete %>%
    mutate(access = spout_extended - spout_retracted) %>%
    mutate(access = ifelse(access == 0, NA, access)) %>%
    fill(access) %>%
    mutate(access = ifelse(access == -1, 0, access)) %>%
    mutate(access = ifelse(is.na(access), 0, access)) 
  
  # compute cummulative_lick
  df_complete <- df_complete %>%
    mutate(cummulative_lick = cumsum(lick))
  
  # compute tm_absolute
  df_complete <- df_complete %>%
    left_join(
      df_joined %>% filter(blockname == blockname_loop) %>%
        select(trial_num, trial_start_ts) %>%
        unique(),
      by = "trial_num"
    ) %>%
    mutate(tm_absolute = as.numeric(tm_bin) + trial_start_ts) %>%
    select(-trial_start_ts)
  
  
  # pull out first lick
  df_complete <- df_complete %>%
    select(blockname, trial_num, tm_bin, lick) %>%
    filter(lick == 1) %>%
    group_by(blockname, trial_num) %>%
    filter(tm_bin == min(tm_bin)) %>% # filter to earliest time
    rename(licking_initiation = lick) %>%
    left_join(df_complete,., by = c("blockname", "trial_num", "tm_bin")) %>%
    mutate(licking_initiation = ifelse(is.na(licking_initiation), 0, licking_initiation))
  
  
  # pull out last lick
  df_complete <- df_complete %>% 
    select(blockname, trial_num, tm_bin, lick) %>%
    filter(lick == 1) %>%
    group_by(blockname, trial_num) %>%
    filter(tm_bin == max(tm_bin)) %>% # filter to latest time
    rename(licking_termination = lick)  %>%
    left_join(df_complete,., by = c("blockname", "trial_num", "tm_bin")) %>%
    mutate(licking_termination = ifelse(is.na(licking_termination), 0, licking_termination))
  
  # pull out last lick prior to half trial
  df_complete <- df_complete %>%
    select(blockname, trial_num, tm_bin, lick) %>%
    filter(lick == 1) %>%
    group_by(blockname, trial_num) %>%
    filter(tm_bin == max(tm_bin)) %>% # filter to latest time
    filter(as.double(tm_bin) < 1.5) %>% # filter to time less than 1.5s
    rename(licking_termination_early = lick)  %>%
    left_join(df_complete,., by = c("blockname", "trial_num", "tm_bin")) %>%
    mutate(licking_termination_early = ifelse(is.na(licking_termination_early), 0, licking_termination_early))
  
  # edit the tm_bin to be 2 decimal places
  df_complete <- df_complete %>%
    mutate(tm_bin = tm_bin %>% as.double() %>% round(2) %>% as.character()) %>%
    mutate(tm_bin = sprintf("%.2f", as.double(tm_bin)))
  
  # solution value
  df_complete <- df_trial %>%
    select(blockname, trial_num, solution) %>%
    rowwise() %>%
    mutate(solution_value = ifelse(solution %>% str_detect('sucrose'), solution %>% str_remove('sucrose') %>% as.double(), NA)) %>%
    mutate(solution_value = ifelse(solution %>% str_detect('nacl'), solution %>% str_remove('nacl') %>% as.double() / 100, solution_value)) %>%
    mutate(solution_value = ifelse(solution %>% str_detect('water'), 0, solution_value)) %>%
    select(-solution) %>%
    mutate(access = 1) %>%
    get_lagged_cols('solution_value', 5, 1, fill_zeros = 0) %>%
    left_join(df_complete,., by = c('blockname', 'trial_num', 'access')) %>%
    mutate(solution_value = ifelse(is.na(solution_value), 0, solution_value))
  
  # spout id
  df_complete <- df_trial %>%
    select(blockname, trial_num, spout) %>%
    mutate(spout_id = spout %>% str_remove('spout') %>% as.integer()) %>%
    select(-spout) %>%
    mutate(access = 1) %>%
    left_join(df_complete,., by = c('blockname', 'trial_num', 'access')) %>%
    mutate(spout_id = ifelse(is.na(spout_id), 0, spout_id))
  
  # reward history
  df_trial_solutions <- df_complete %>%
    filter(access == 1) %>%
    select(trial_num, access, solution_value) %>%
    unique() 
  
  df_solution_value_history <- get_multispout_value_history(df_trial_solutions, 'solution_value', max_n = 5) %>%
    select(-solution_value, -access)
  
  df_complete <- df_complete %>%
    left_join(df_solution_value_history, by = c("trial_num"))
  
  
  # lick and spout / trial
  df_complete <- df_complete %>%
    left_join(df_trial %>% select(blockname, trial_num, spout, solution), by = c("blockname", "trial_num")) %>% 
    select(blockname, trial_num, solution, spout, everything())
  
  return(df_complete)
}


batch_get_prediction_matrix_multispout <- function(fns){
  # pre-processes data and applies get_prediction_matrix_multispout to all fns
  
  # correction for extra tdt event occurs here 
  
  print('returning prediction matrix for multi-spout sessions...')
  
  for(fn in fns){
    
    
    df_event <- read.csv(str_c('./data/sessions/', fn,'/', fn, '_fp_events.csv')) %>% arrange(event_ts)
    df_trial <- read.csv(str_c('./data/sessions/', fn,'/', fn, '_data_trial_summary.csv'))
    
    
    # edit number of trials for sessions with start ttl event
    n_trials <- df_event %>%
      filter(event_id_char == 'access_period' | event_id_char == 'spout_extended') %>%
      nrow()
    
    # add in event_number for sessions without
    if (!any(names(df_event) == "event_number")) {
      df_event <- df_event %>% 
        group_by(event_id_char) %>%
        mutate(event_number = row_number()) %>%
        ungroup() 
    } 
    
    if(n_trials == 101){
      df_event <- df_event %>%
        filter(event_ts > min(event_ts + 1)) %>%
        mutate(event_number = event_number - 1)
    }
    
    if(n_trials > 101){
      print('WARNING IMPROPER NUMBER OF EVENTS')
    }
    
    n_trials_edit <- df_event %>%
      filter(event_id_char == 'access_period' | event_id_char == 'spout_extended') %>%
      nrow()
    
    print(str_c(fn, '- n trial: ', n_trials, ' / ', n_trials_edit))
    
    pred_matrix <- get_prediction_matrix_multispout(df_event, df_trial, fn)
    
    pred_matrix %>%
      write_csv(str_c('./data/sessions/', fn,'/', fn, '_data_prediction_matrix_full.csv'))
    
    
    # repeat with shuffled spout / solution ids
    df_trial$solution <- df_trial$solution[sample(nrow(df_trial))]
    df_trial$spout <- df_trial$spout[sample(nrow(df_trial))]
    
    pred_matrix_shuffled <- get_prediction_matrix_multispout(df_event, df_trial, fn)
    
    pred_matrix_shuffled %>%
      write_csv(str_c('./data/sessions/', fn,'/', fn, '_data_prediction_matrixshuffle_full.csv'))
    
  }
}


batch_get_trial_diagonal_matricies <- function(dir_sessions, fns, n_samples_per_trial, toggle_solution_shuffle){
  # pre-processes data and applies get_prediction_matrix_multispout to all fns
  
  # correction for extra tdt event occurs here 
  
  print('returning diagonal prediction matrix for multi-spout sessions...')
  
  n_file <- 1
  
  for(fn in fns){
    print(str_c(' ~ ', fn, ' ', n_file, ' / ', length(fns)))
    
    dir_output <- str_c(dir_sessions, fn,'/')
    
    if(toggle_solution_shuffle){
      fn_stem <- 'diagonal_shuffle_'
    } else {
      fn_stem <- 'diagonal_true_'
    }
    
    
    df_trial <- read.csv(str_c(dir_sessions, fn,'/', fn, '_data_trial_summary.csv'))
    
    if(toggle_solution_shuffle){
      df_trial$solution <- df_trial$solution %>% sample(replace = F)
    }
    
    # get solution values and dummy
    df_trial <- df_trial %>%
      get_solution_value()  %>%
      mutate(dummy_trial = 1)
    
    # # solution concentration raw
    # get_diagonal_from_trial_ids(
    #   df_trial_summary = df_trial,
    #   var_id = 'solution_value',
    #   n_samples_per_trial = n_samples_per_trial) %>% 
    #   write.table(str_c(dir_output, 'glm/', fn_stem, 'solution_conc.csv'), sep = ',', row.names = F, col.names = F)
    
    # # solution concentration scaled from -1:1
    # get_diagonal_from_trial_ids(
    #   df_trial_summary = df_trial,
    #   var_id = 'solution_value_scaled_m1_p1',
    #   n_samples_per_trial = n_samples_per_trial) %>% 
    #   write.table(str_c(dir_output, 'glm/', fn_stem, 'solution_conc_scaled_m1_p1.csv'), sep = ',', row.names = F, col.names = F)
    
    # solution concentration scaled from 0:1
    get_diagonal_from_trial_ids(
      df_trial_summary = df_trial,
      var_id = 'solution_value_scaled_0_p1',
      n_samples_per_trial = n_samples_per_trial) %>%
      write.table(str_c(dir_output, 'glm/', fn_stem, 'solution_conc_scaled_0_p1.csv'), sep = ',', row.names = F, col.names = F)
    
    # # trial diagonal
    get_diagonal_from_trial_ids(
      df_trial_summary = df_trial,
      var_id = 'dummy_trial',
      n_samples_per_trial = n_samples_per_trial) %>%
      write.table(str_c(dir_output, 'glm/', fn_stem, 'trial.csv'), sep = ',', row.names = F, col.names = F)
    
    # # trial lick
    # get_diagonal_from_trial_ids(
    #   df_trial_summary = df_trial,
    #   var_id = 'trial_lick',
    #   n_samples_per_trial = n_samples_per_trial) %>% 
    #   write.table(str_c(dir_output, 'glm/', fn_stem, 'trial_lick.csv'), sep = ',', row.names = F, col.names = F) 
    
    
    # # history for solution_conc
    # df_history <- df_trial %>%
    #   get_solution_value() %>%
    #   select(trial_num, solution_value) %>%
    #   rename(solution_conc = solution_value) %>% 
    #   get_multispout_value_history('solution_conc', 3) %>%
    #   select(-trial_num, -solution_conc) 
    # 
    # 
    # for(var_history in colnames(df_history)){
    #   
    #   get_diagonal_from_trial_ids(
    #     df_trial_summary = df_history,
    #     var_id = var_history,
    #     n_samples_per_trial = n_samples_per_trial) %>% 
    #     write.table(str_c(dir_output, 'glm/', fn_stem, var_history, '.csv'), sep = ',', row.names = F, col.names = F)
    # }
    
    
    # # history for solution_conc_scaled_m1_p1
    # df_history <- df_trial %>%
    #   get_solution_value() %>%
    #   select(trial_num, solution_value_scaled_m1_p1) %>%
    #   rename(solution_conc_scaled_m1_p1 = solution_value_scaled_m1_p1) %>% 
    #   get_multispout_value_history('solution_conc_scaled_m1_p1', 3) %>%
    #   select(-trial_num, -solution_conc_scaled_m1_p1) 
    # 
    # 
    # for(var_history in colnames(df_history)){
    #   
    #   get_diagonal_from_trial_ids(
    #     df_trial_summary = df_history,
    #     var_id = var_history,
    #     n_samples_per_trial = n_samples_per_trial) %>% 
    #     write.table(str_c(dir_output, 'glm/', fn_stem, var_history, '.csv'), sep = ',', row.names = F, col.names = F)
    # }
    
    
    # history for solution_conc_scaled_0_p1
    df_history <- df_trial %>%
      get_solution_value() %>%
      select(trial_num, solution_value_scaled_0_p1) %>%
      rename(solution_conc_scaled_0_p1 = solution_value_scaled_0_p1) %>%
      get_multispout_value_history('solution_conc_scaled_0_p1', 3) %>%
      select(-trial_num, -solution_conc_scaled_0_p1)
    
    
    for(var_history in colnames(df_history)){
      
      get_diagonal_from_trial_ids(
        df_trial_summary = df_history,
        var_id = var_history,
        n_samples_per_trial = n_samples_per_trial) %>%
        write.table(str_c(dir_output, 'glm/', fn_stem, var_history, '.csv'), sep = ',', row.names = F, col.names = F)
    }
    
    n_file <- n_file + 1
  }
}



finalize_prediction_matrix_multispout <- function(dir_session, fn){
  # seperates full prediction matrix into independnt csv files for each predictor
  
  dir_glm <- str_c(dir_session, fn, '/glm/')
  
  if(!dir.exists(dir_glm)){
    dir.create(dir_glm, recursive = T)
  }
  
  pm_full <- read.csv(str_c('./data/sessions/', fn, '/', fn, '_data_prediction_matrix_full.csv'))
  pm_full_shuffle <- read.csv(str_c('./data/sessions/', fn, '/', fn, '_data_prediction_matrixshuffle_full.csv'))
  
  # # time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pm_full %>%
  #   select(tm_absolute) %>%
  #   write_csv(str_c(dir_glm, 'tm_absolute.csv'), col_names = F)
  
  # licking ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  lick_lagged_cols <- 5
  
  licks_raw <- pm_full %>%
    mutate(lick = ifelse(lick > 1, 1, lick)) %>%
    pull(lick) 
  
  #kernel_length <- 25  # Adjust based on desired decay length
  #sd <- 2.5*20  # Adjust standard deviation for rate of decay
  licks_convolved <- convolve_licks(licks_raw, 2.5*20,round(2.5*20))
  
  pm_licks <- tibble(licks_convolved = licks_convolved) %>%
    get_lagged_cols("licks_convolved", lick_lagged_cols, 1, fill_zeros = 1)  %>%
    get_lagged_cols("licks_convolved", lick_lagged_cols, -1, fill_zeros = 1) %>%
    reorder_laglead() 
  
  pm_licks %>%
    write_csv(str_c(dir_glm, 'lick_kernal.csv'), col_names = F)
  
  # # lick initiation ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # licks_raw <- pm_full %>%
  #   pull(licking_initiation) 
  # 
  # #kernel_length <- 25  # Adjust based on desired decay length
  # #sd <- 2.5*20  # Adjust standard deviation for rate of decay
  # licks_convolved <- convolve_licks(licks_raw, 2.5*20, 3) 
  # 
  # pm_licks <- tibble(licks_convolved = licks_convolved) %>%
  #   get_lagged_cols("licks_convolved", lick_lagged_cols, 1, fill_zeros = 1)  %>%
  #   get_lagged_cols("licks_convolved", lick_lagged_cols, -1, fill_zeros = 1) %>%
  #   reorder_laglead() 
  
  # pm_licks %>%
  #   write_csv(str_c(dir_glm, 'lickinitiation_kernal.csv'), col_names = F)
  # 
  # # lick termination ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # licks_raw <- pm_full %>%
  #   pull(licking_termination) 
  # 
  # licks_convolved <- convolve_licks(licks_raw, 2.5*20, 3)
  # 
  # pm_licks <- tibble(licks_convolved = licks_convolved) %>%
  #   get_lagged_cols("licks_convolved", lick_lagged_cols, 1, fill_zeros = 1)  %>%
  #   get_lagged_cols("licks_convolved", lick_lagged_cols, -1, fill_zeros = 1) %>%
  #   reorder_laglead() 
  # 
  # pm_licks %>%
  #   write_csv(str_c(dir_glm, 'licktermination_kernal.csv'), col_names = F)
  # 
  # # lick termination early ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # licks_raw <- pm_full %>%
  #   pull(licking_termination_early) 
  # 
  # licks_convolved <- convolve_licks(licks_raw, 2.5*20, 3)
  # 
  # pm_licks <- tibble(licks_convolved = licks_convolved) %>%
  #   get_lagged_cols("licks_convolved", lick_lagged_cols, 1, fill_zeros = 1)  %>%
  #   get_lagged_cols("licks_convolved", lick_lagged_cols, -1, fill_zeros = 1) %>%
  #   reorder_laglead() 
  # 
  # pm_licks %>%
  #   write_csv(str_c(dir_glm, 'lickterminationearly_kernal.csv'), col_names = F)
  # 
  # # solution kernals ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pm_solution <- pm_full %>%
  #   select((starts_with('sucrose')|starts_with('nacl'))) 
  # 
  # 
  # solution_vars <- pm_solution %>% colnames()
  # 
  # for(var in solution_vars){
  #   
  #   licks_raw <- pm_solution %>%
  #     mutate(!!as.name(var) := ifelse(!!as.name(var) > 1, 1, !!as.name(var))) %>%
  #     pull(var)
  #   
  #   licks_convolved <- convolve_licks(licks_raw, 2.5*20,round(2.5*20)) # matches licking kernal
  #   
  #   pm_licks <- tibble(licks_convolved = licks_convolved) %>%
  #     get_lagged_cols("licks_convolved", lick_lagged_cols, 1, fill_zeros = 1)  %>%
  #     get_lagged_cols("licks_convolved", lick_lagged_cols, -1, fill_zeros = 1) %>%
  #     reorder_laglead() 
  #   
  #   pm_licks %>%
  #     write_csv(str_c(dir_glm, 'solution_', var, '_kernal.csv'), col_names = F)
  # }
  # 
  # # solution kernals shuffled ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pm_solution_shuffle <- pm_full_shuffle %>%
  #   select((starts_with('sucrose')|starts_with('nacl'))) 
  # 
  # 
  # solution_vars <- pm_solution_shuffle %>% colnames()
  # 
  # shuffle_n <- 1
  # 
  # for(var in solution_vars){
  #   
  #   licks_raw <- pm_solution_shuffle %>%
  #     mutate(!!as.name(var) := ifelse(!!as.name(var) > 1, 1, !!as.name(var))) %>%
  #     pull(var)
  #   
  #   licks_convolved <- convolve_licks(licks_raw, 2.5*20,round(2.5*20)) # matches licking kernal
  #   
  #   pm_licks <- tibble(licks_convolved = licks_convolved) %>%
  #     get_lagged_cols("licks_convolved", lick_lagged_cols, 1, fill_zeros = 1)  %>%
  #     get_lagged_cols("licks_convolved", lick_lagged_cols, -1, fill_zeros = 1) %>%
  #     reorder_laglead() 
  #   
  #   pm_licks %>%
  #     write_csv(str_c(dir_glm, 'solutionshuffle_', shuffle_n, '_kernal.csv'), col_names = F)
  #   
  #   shuffle_n <- shuffle_n + 1
  # }
  
  # ============================================================================================================================================
  
  # # trial solution kernals ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # bins_access_post_access <- 6 / (1/20)
  # 
  # # generate mask that will filter the trial kernals to time after the first lick
  # lick_mask <- pm_full %>%
  #   mutate(lick = ifelse(lick == 0, NA, lick)) %>%
  #   group_by(trial_num) %>%
  #   fill(lick) %>%
  #   mutate(lick = ifelse(is.na(lick), 0, lick )) %>%
  #   pull(lick)
  # 
  # solution_trial_kernal_all <- pm_full %>%
  #   select(spout_extended) %>%
  #   get_lagged_cols("spout_extended", bins_access_post_access-1, 1, fill_zeros = 1) #%>%
  #   #sweep(1, lick_mask, `*`)
  # 
  # solution_trial_kernal_all %>%
  #   write_csv(str_c(dir_glm, 'trial_full_kernal.csv'), col_names = F)
  #   
  # 
  # solution_ids <- pm_full$solution %>% unique() %>% sort()
  # 
  # for(sol in solution_ids){
  #   solution_mask <- pm_full$solution == sol
  #   
  #   solution_trial_kernal_single <- solution_trial_kernal_all %>%
  #     sweep(1, solution_mask, `*`)
  #   
  #   solution_trial_kernal_single %>%
  #     write_csv(str_c(dir_glm, 'trial_solution_', sol, '_kernal.csv'), col_names = F)
  # }
  # 
  # # trial solution kernals shuffled ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # # generate mask that will filter the trial kernals to time after the first lick
  # lick_mask <- pm_full_shuffle %>%
  #   mutate(lick = ifelse(lick == 0, NA, lick)) %>%
  #   group_by(trial_num) %>%
  #   fill(lick) %>%
  #   mutate(lick = ifelse(is.na(lick), 0, lick )) %>%
  #   pull(lick)
  # 
  # solution_trial_kernal_all <- pm_full_shuffle %>%
  #   select(spout_extended) %>%
  #   get_lagged_cols("spout_extended", bins_access_post_access-1, 1, fill_zeros = 1) #%>%
  #   #sweep(1, lick_mask, `*`)
  # 
  # solution_ids <- pm_full_shuffle$solution %>% unique() %>% sort()
  # 
  # shuffle_n <- 1
  # 
  # for(sol in solution_ids){
  #   solution_mask <- pm_full_shuffle$solution == sol
  #   
  #   solution_trial_kernal_single <- solution_trial_kernal_all %>%
  #     sweep(1, solution_mask, `*`)
  #   
  #   solution_trial_kernal_single %>%
  #     write_csv(str_c(dir_glm, 'trial_solutionshuffle_', shuffle_n, '_kernal.csv'), col_names = F)
  #   
  #   shuffle_n <- shuffle_n + 1
  # }
  
  # ============================================================================================================================================
  
  # # solution concentration continuous ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pm_full %>%
  #   select(solution_value) %>%
  #   write_csv(str_c(dir_glm, 'solution_conc_continuous.csv'), col_names = F)
  # 
  # 
  # 
  # # solution id descrete ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pm_full %>%
  #   select(solution) %>%
  #   mutate(solution = solution %>% factor() %>% as.numeric()) %>%
  #   write_csv(str_c(dir_glm, 'solution_id_descrete.csv'), col_names = F)
  # 
  # pm_full_shuffle %>%
  #   select(solution) %>%
  #   mutate(solution = solution %>% factor() %>% as.numeric()) %>%
  #   write_csv(str_c(dir_glm, 'solutionshuffle_id_descrete.csv'), col_names = F)
  #   
  # 
  # # cummulative_lick ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # pm_full %>%
  #   select(cummulative_lick) %>%
  #   write_csv(str_c(dir_glm, 'lick_count_cummulative.csv'), col_names = F)
  # 
  # pm_full_shuffle %>%
  #   select(cummulative_lick) %>%
  #   write_csv(str_c(dir_glm, 'lick_count_cummulative.csv'), col_names = F)
  # 
  # # access / spout extended  / spout retracted kernals ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # ## define number of bins for each component of access period
  # bins_extension <- 0.2 / (1/20)
  # bins_retraction <- 0.2 / (1/20)
  # bins_access <- 3 / (1/20)
  # 
  # bins_access_full <- bins_access + bins_retraction #3s access + 200ms retraction
  # 
  # 
  # pm_kernal_access_full <- pm_full %>%
  #   select(spout_extended) %>%
  #   get_lagged_cols("spout_extended", bins_access_full-1, 1, fill_zeros = 1) 
  # 
  # ## save each subset as seperate csv files
  # pm_kernal_access_full[,1:bins_extension] %>%
  #   write_csv(str_c(dir_glm, 'trial_kernal_extension.csv'), col_names = F)
  # 
  # pm_kernal_access_full[,(bins_extension+1):bins_access] %>%
  #   write_csv(str_c(dir_glm, 'trial_kernal_access.csv'), col_names = F)
  # 
  # pm_kernal_access_full[,(bins_access+1):bins_access_full]  %>%
  #   write_csv(str_c(dir_glm, 'trial_kernal_retraction.csv'), col_names = F)
}




finalize_prediction_matrix_multispout_batch <- function(fns, dir_session){
  # applies finalize_prediction_matrix_multispout to fns listed in dir_session
  
  print('finalizng prediction matrix...')
  for(fn in fns){
    print(str_c('processing - ', fn))
    finalize_prediction_matrix_multispout(dir_session, fn)
  }
}


