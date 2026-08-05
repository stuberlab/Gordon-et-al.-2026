combine_signals <- function(fns){
  # returns a list with 
  #  combined signals
  #  information for signals
  
  n_file <- 1
  
  for(fn in fns){
    
    df <- read.csv(fn, header = F)
    
    pattern <- ".*/(\\d{4}_\\d{2}_\\d{2}_[a-z]{3}\\d{2})/.*"
    blockname <- sub(pattern, "\\1", fn)
    
    df_info <- tibble(
      blockname = blockname,
      sample_count = nrow(df)
    )
    
    if(fn == fns[1]){
      df_combined <- df
      df_info_combined <- df_info
    } else {
      n_file <- n_file + 1
      
      max_length <- max(nrow(df), nrow(df_combined))
      
      # ensure both dataframes have the same number of rows, padding with NA if necessary
      df1_padded <- df_combined %>% 
        add_row(V1 = rep(NA, max_length - nrow(df_combined)))
      
      df2_padded <- df %>% 
        add_row(V1 = rep(NA, max_length - nrow(df)))
      
      df2_padded <- df2_padded %>%
        rename(!!as.name(str_c('V', n_file)) := 'V1')
      
      # bind the columns together
      df_combined <- bind_cols(df1_padded, df2_padded)
      
      df_info_combined <- df_info %>% bind_rows(df_info_combined,.)
      
    }
  }
  
  return(list(df_combined, df_info_combined))
}


generate_nulls <- function(fns, dir_sessions, dir_null, region_ids, signal_ids){
  # function combines all signal matricies for each signal defined in signal_ids and each region defined in region_ids
  # 
  # requires that all signal matricies have the same number of rows
  
  for(region in region_ids){
    for(signal in signal_ids){
      
      # Pattern to match in the filenames
      filename_pattern <- str_c(region, '_', signal, '.csv')
      
      # List all files that match the pattern in the signals directories
      file_paths <- list.files(path = dir_sessions, pattern = filename_pattern, full.names = TRUE, recursive = TRUE)
      
      # Filter out files that are not in 'signals' directories
      signal_files <- grep("/glm/signals/", file_paths, value = TRUE)
      
      # Filter signal_files based on blocknames in fns
      filtered_files <- unlist(lapply(fns, function(fn) {
        grep(fn, signal_files, value = TRUE)
      }))
      
      print(str_c('combining: ',  length(filtered_files), ' files- ', region, ' x ', signal))
      
      # combine files
      data_list <- combine_signals(filtered_files)
      
      df_combined <- data_list[[1]]
      df_info_combined <- data_list[[2]]
      
      # save combined files
      df_combined %>% write_csv(str_c(dir_null, region, '_', signal, '_null_data.csv'), col_names = F)
      df_info_combined %>% write_csv(str_c(dir_null, region, '_', signal, '_null_blocknames.csv'))
      
    }
  }
}

pad_signal_matrix <- function(dir_null, n_cols){
  # pads null distribution to n_cols using randomly selected signals within matrix
  
  file_paths <- list.files(path = dir_null, pattern = 'null_data.csv', full.names = TRUE, recursive = TRUE)
  
  for(fn in file_paths){
    df <- read.csv(fn, header = F)
    
    n_sessions <- ncol(df)
    
    if(n_sessions < n_cols){
      print(str_c('appending - ', fn))
      
      new_cols_needed <- n_cols - n_sessions
      
      # Randomly sample column names from the existing dataframe
      # Note: If new_cols_needed is greater than the number of columns in df,
      # we allow repeated sampling by setting replace = TRUE
      sampled_col_names <- sample(names(df), new_cols_needed, replace = TRUE)
      
      # Use lapply to create a new list of sampled columns
      # Here we're just copying the columns, but you could apply any transformation if needed
      sampled_cols <- lapply(sampled_col_names, function(col_name) df[[col_name]])
      
      # Convert the list of sampled columns into a dataframe
      new_df <- as.data.frame(sampled_cols)
      
      # Rename the new columns to ensure unique names
      names(new_df) <- paste0("Sampled_", sampled_col_names, "_", seq_len(ncol(new_df)))
      
      # Append the new columns to the original dataframe
      df <- cbind(df, new_df)
      
      df %>% write_csv(fn, col_names = F)
    }
  }
}