get_diagonal_from_trial_ids <- function(df_trial_summary, var_id, n_samples_per_trial) {
  # Function to convert trial data into a diagonal matrix
  # This function takes a dataframe with trial summary (1 observation / trial), a variable ID, and the number of samples per trial,
  # and returns a matrix where each trial's values are represented as diagonal matrices, stacked row-wise.
  
  
  # Initialize an empty matrix to store the results
  matrix_session <- matrix(nrow = 0, ncol = n_samples_per_trial)
  
  # Loop through each unique trial ID
  for(value in df_trial_summary %>% pull(!!as.name(var_id))) {
    
    # Create a diagonal matrix with the extracted value and specified number of samples
    matrix_trial <- diag(1, n_samples_per_trial, n_samples_per_trial) * value
    
    # Append the diagonal matrix to the session matrix
    matrix_session <- rbind(matrix_session, matrix_trial)
  }
  
  # Return the resulting matrix
  return(matrix_session)
}
