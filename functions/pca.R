
tidy_pca_summary <- function(pca_results) {
  # Extract standard deviation of each principal component
  std_devs <- pca_results$sdev
  
  # Compute the proportion of variance explained by each principal component
  prop_var <- std_devs^2 / sum(std_devs^2)
  
  # Compute the cumulative proportion of variance explained
  cum_var <- cumsum(prop_var)
  
  # Create a data frame with the tidy PCA summary
  tidy_summary <- tibble(
    pc = paste0(seq_along(std_devs) %>% as.numeric()),
    sd = std_devs,
    prop_var = prop_var,
    prop_var_cummulative = cum_var
  )
  
  return(tidy_summary)
}


set_pc_origin3d <- function(df){
  df %>%
    filter(time_rel == 0) %>% 
    rename(
      pc1_origin = pc1, 
      pc2_origin = pc2, 
      pc3_origin = pc3
    ) %>%
    select(-time_rel) %>%
    left_join(df,.) %>%
    mutate(
      pc1_adj = pc1 - pc1_origin,
      pc2_adj = pc2 - pc2_origin,
      pc3_adj = pc3 - pc3_origin
    ) %>%
    select(
      -pc1_origin, 
      -pc2_origin, 
      -pc3_origin
    )
  
} 