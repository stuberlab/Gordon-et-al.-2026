plt_str <- function(df, var_color, var_color_name, vars_facet, color_limits, plt_plane, plt_dim_base){
  range_ap <- c(-3, 3)
  range_ml <- c(0, 4)
  range_dv <- c(-6, -1)
  
  var_color <- rlang::enquo(var_color)
  facet_formula <- as.formula(paste(vars_facet[1], '~', vars_facet[2]))
  
  if(plt_plane == 'sagittal'){
    plt <- df %>%
      ggplot(aes(hit_ap, hit_dv, fill = !!var_color)) +
      coord_cartesian(xlim = range_ap, ylim = range_dv, expand = F) +
      xlab('Anterior-posterior (mm)')+
      ylab('Dorsal-ventral (mm)') 
  }
  
  if(plt_plane == 'coronal'){
    plt <- df %>%
      ggplot(aes(hit_ml, hit_dv, fill = !!var_color)) +
      coord_cartesian(xlim = range_ml, ylim = range_dv, expand = F) +
      xlab('Medial-lateral (mm)')+
      ylab('Dorsal-ventral (mm)')
  }
  
  if(plt_plane == 'horizontal'){
    plt <- df %>%
      ggplot(aes(hit_ap, hit_ml, fill = !!var_color)) +
      coord_cartesian(xlim =  range_ap, ylim = range_ml, expand = F) +
      xlab('Anterior-posterior (mm)')+
      ylab('Medial-lateral (mm)') 
  }
  
  plt <- plt +
    geom_jitter(width = 0.2, height = 0.2, size = 1, shape = 21, color = 'white', stroke = 1/8) # adjust fill
  
  if(sum(is.na(color_limits)) == 0){
    plt <- plt +
      scale_fill_viridis_c(
        limits = color_limits,
        oob = scales::squish,
        name = var_color_name, 
        breaks = color_limits)
  }
  
  plt <- plt+
    theme_ag01(6) +
    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.2,"cm"),
      legend.title.position = "right",
      panel.grid.major = element_line(color = 'lightgrey', size = 0.5), 
      legend.title = element_text(angle = 270, hjust = 0.5),
      axis.title = element_text(color = "black", size = 6),
      axis.text.x = element_text(color = "black", size = 6, hjust = 0.5),
      axis.text.y = element_text(color = "black", size = 6, hjust = 1, vjust = 0.3, margin = margin(r = 1)),
      
    ) 
  
  
  plt <- plt +
    facet_grid(facet_formula)
  
  dim_ap <- range_ap[2] - range_ap[1]
  dim_ml <- range_ml[2] - range_ml[1]
  dim_dv <- range_dv[2] - range_dv[1]
  
  
  aspect_sagital <- dim_ap / dim_dv
  aspect_coronal <- dim_ml / dim_dv
  aspect_horizontal <- dim_ap / dim_ml
  
  if(plt_plane == 'sagittal'){
    plt <- plt +
      force_panelsizes(rows = unit(plt_dim_base, 'cm'), cols = unit(plt_dim_base*aspect_sagital, 'cm'))
  } 
  
  if(plt_plane == 'horizontal'){
    plt <- plt +
      force_panelsizes(rows = unit(plt_dim_base, 'cm'), cols = unit(plt_dim_base*aspect_horizontal, 'cm'))
  } 
  
  if(plt_plane == 'coronal'){
    plt <- plt +
      force_panelsizes(rows = unit(plt_dim_base, 'cm'), cols = unit(plt_dim_base*aspect_coronal, 'cm'))
  }
  
  return(plt)
  
}



plt_str_3d <- function(df, var_color, color_limits, dir_output, fn_stem, toggle_save){
  x_range <- c(-3, 3)
  y_range <- c(0, 4)
  z_range <- c(-5, -2)
  
  font.pref.title <- list(size=30, family="Arial, sans-serif", color="black")
  font.pref.tick <- list(size=18, family="Arial, sans-serif", color="black")
  
  i <- -2.5 # x/y eye position
  ii <- 0.5 # z eye position
  
  # Convert the column name to a symbol
  var_color_sym <- sym(var_color)
  
  # Squish the values of var_color within the specified color_limits
  df <- df %>%
    mutate(!!var_color_sym := ifelse(!!var_color_sym > color_limits[2], color_limits[2], !!var_color_sym)) %>%
    mutate(!!var_color_sym := ifelse(!!var_color_sym < color_limits[1], color_limits[1], !!var_color_sym))
  
  # Create the plot
  plt <- plot_ly(df,
                 x = ~hit_ap, 
                 y = ~hit_ml, 
                 z = ~hit_dv, 
                 color = ~.data[[var_color]],  # Use the symbol for the color mapping
                 type = 'scatter3d', 
                 mode = 'markers',  # Plot points only
                 marker = list(
                   size = 10,
                   colorscale = 'Viridis',  
                   cmin = color_limits[1],   # Set the minimum value for color scaling
                   cmax = color_limits[2]    # Set the maximum value for color scaling
                 )
  ) %>%
    layout(scene = list(
      xaxis = list(
        title = 'AP (mm)', 
        range = x_range, 
        tickvals = seq(x_range[1], x_range[2], by = 1),  # Specify x-axis ticks
        ticktext = seq(x_range[1], x_range[2], by = 1),  # Custom labels
        nticks = sum(abs(x_range)), 
        titlefont = font.pref.title,
        tickfont = font.pref.tick,
        zeroline = FALSE,  # Remove emphasis on zero line
        showline = TRUE,   # Keep the axis line visible
        showgrid = TRUE    # Optionally keep grid lines
      ),
      yaxis = list(
        title = 'ML (mm)', 
        range = y_range, 
        tickvals = seq(y_range[1], y_range[2]-1, by = 1),  # Specify y-axis ticks
        ticktext = seq(y_range[1], y_range[2]-1, by = 1),  # Custom labels
        nticks = sum(abs(y_range)), 
        titlefont = font.pref.title,
        tickfont = font.pref.tick,
        zeroline = FALSE,  # Remove emphasis on zero line
        showline = TRUE,  
        showgrid = TRUE   
      ),
      zaxis = list(
        title = 'DV (mm)', 
        range = z_range, 
        tickvals = seq(z_range[1]+1, z_range[2], by = 1),  # Specify z-axis ticks
        ticktext = seq(z_range[1]+1, z_range[2], by = 1),  # Custom labels
        nticks = 6, 
        titlefont = font.pref.title,
        tickfont = font.pref.tick,
        zeroline = FALSE,  # Remove emphasis on zero line
        showline = TRUE,  
        showgrid = TRUE   
      ), 
      aspectmode = 'manual',
      aspectratio = list(
        x = (x_range[2] - x_range[1])/4.5, 
        y = (y_range[2] - y_range[1])/4.5, 
        z = (z_range[2] - z_range[1])/4.5
      ),
      camera = list(
        eye = list(x = cos(i)*3, y = sin(i)*3, z = ii*3), 
        center = list(x = 0, y = 0, z = -0.1)
      )
    )
    )
  
  if(toggle_save){
    save_image(plt,
               width = 900,
               height = 800,
               scale = 1,
               str_c(dir_output, fn_stem, ".pdf"))
  } else {
    plt
  }
}

plt_corr_generic <- function(df, ylim_all, xlim_ap, xlim_ml, xlim_dv, var_dep, var_dep_label, plt_dims){
  require(ggpubr)
  require(patchwork)
  
  plt_data <- df %>%
    gather('dim', 'mm', starts_with('hit')) %>%
    
    mutate(dim = case_when(
      dim == 'hit_ap' ~ 'AP', #'Anterior-Posterior', 
      dim == 'hit_ml' ~ 'ML', #'Medial-Lateral',
      dim == 'hit_dv' ~ 'DV') #'Dorsal-Ventral'
    ) %>%
    mutate(dim = dim %>% factor(levels = c('AP', 'ML', 'DV'))) # c('Anterior-Posterior', 'Medial-Lateral', 'Dorsal-Ventral')))
  
  f_plt <- function(plt_data, ylim_all, xlim_ap, xlim_ml, xlim_dv, var_dep, var_dep_label, plt_dims, plt_dim){
    
    plt <- plt_data %>%
      filter(dim == plt_dim) %>%
      ggplot(aes(mm, !!as.name(var_dep))) +
      geom_smooth(method = 'lm', size = 0.3, alpha = 1/5, color = 'black') +
      geom_jitter(width = 0.1, shape = 21, size = 2, color = 'white', fill = 'black', stroke = 1/8) +
      facet_grid(.~dim, scales = 'free') +
      theme_ag01(6) +
      #force_panelsizes(rows = unit(plt_dims[1], 'cm'), cols = unit(plt_dims[2], 'cm'))
      xlab('MM Relative to bregma') +
      ylab(var_dep_label) +
      theme(strip.text.y = element_blank()) +
      theme(panel.spacing.y = unit(0.75, "lines")) 
    
    if(plt_dim == 'AP'){
      plt <- plt +
        coord_cartesian(ylim = ylim_all, xlim = c(min(xlim_ap), max(xlim_ap)), expand = F, clip = 'off') +
        stat_cor(method = "pearson", label.y = ylim_all[2], label.x = 0, size = 2, hjust = 0.5) +
        scale_x_continuous(breaks = xlim_ap) +
        theme(axis.title.x = element_blank())
    } 
    
    if(plt_dim == 'ML'){
      plt <- plt +
        coord_cartesian(ylim = ylim_all, xlim =  c(min(xlim_ml), max(xlim_ml)), expand = F, clip = 'off') +
        stat_cor(method = "pearson", label.y = ylim_all[2], label.x = 2, size = 2, hjust = 0.5) +
        scale_x_continuous(breaks = xlim_ml) 
      
      plt <- plt %>%
        remove_y_all
    } 
    
    if(plt_dim == 'DV'){
      plt <- plt +
        coord_cartesian(ylim = ylim_all, xlim = c(min(xlim_dv), max(xlim_dv)), expand = F, clip = 'off') +
        stat_cor(method = "pearson", label.y = ylim_all[2], label.x = -4, size = 2, hjust = 0.5) +
        scale_x_continuous(breaks = xlim_dv) +
        theme(axis.title.x = element_blank())
      
      plt <- plt %>%
        remove_y_all
    } 
    
    return(plt)
  }
  
  plt_list <- list()
  
  
  for(plt_dim in c('AP', 'ML', 'DV')){
    plt <- f_plt(plt_data, ylim_all, xlim_ap, xlim_ml, xlim_dv, var_dep, var_dep_label, plt_dims, plt_dim)
    plt_list[[plt_dim]] <- plt
    
  }
  
  # compute stats
  stats_output <- plt_data %>%
    tidy_cor('mm', var_dep, 'dim')
  
  
  plt_list[['stats_output']] <- stats_output
  
  
  return(plt_list)
  
}


plt_str_d <- function(df, var_color, var_color_name, vars_facet, plt_plane, plt_dim_base, color_values){
  range_ap <- c(-3, 3)
  range_ml <- c(0, 4)
  range_dv <- c(-6, -1)
  
  var_color <- rlang::enquo(var_color)
  facet_formula <- as.formula(paste(vars_facet[1], '~', vars_facet[2]))
  
  if(plt_plane == 'sagittal'){
    plt <- df %>%
      ggplot(aes(hit_ap, hit_dv, fill = !!var_color)) +
      coord_cartesian(xlim = range_ap, ylim = range_dv, expand = F) +
      xlab('Anterior-posterior (mm)')+
      ylab('Dorsal-ventral (mm)') 
  }
  
  if(plt_plane == 'coronal'){
    plt <- df %>%
      ggplot(aes(hit_ml, hit_dv, fill = !!var_color)) +
      coord_cartesian(xlim = range_ml, ylim = range_dv, expand = F) +
      xlab('Medial-lateral (mm)')+
      ylab('Dorsal-ventral (mm)')
  }
  
  if(plt_plane == 'horizontal'){
    plt <- df %>%
      ggplot(aes(hit_ap, hit_ml, fill = !!var_color)) +
      coord_cartesian(xlim =  range_ap, ylim = range_ml, expand = F) +
      xlab('Anterior-posterior (mm)')+
      ylab('Medial-lateral (mm)') 
  }
  
  plt <- plt +
    geom_jitter(width = 0.2, height = 0.2, size = 2, shape = 21, color = 'white', stroke = 1/8) # adjust fill
  
  if(sum(is.na(color_values)) == 0){
    plt <- plt +
      scale_fill_manual(
        values = color_values)
  }
  
  plt <- plt+
    theme_ag01(6) +
    theme(
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.2,"cm"),
      legend.title.position = "right",
      panel.grid.major = element_line(color = 'lightgrey', size = 0.5), 
      legend.title = element_text(angle = 270, hjust = 0.5),
      axis.title = element_text(color = "black", size = 6),
      axis.text.x = element_text(color = "black", size = 6, hjust = 0.5),
      axis.text.y = element_text(color = "black", size = 6, hjust = 1, vjust = 0.3, margin = margin(r = 1)),
      
    ) 
  
  
  plt <- plt +
    facet_grid(facet_formula)
  
  dim_ap <- range_ap[2] - range_ap[1]
  dim_ml <- range_ml[2] - range_ml[1]
  dim_dv <- range_dv[2] - range_dv[1]
  
  
  aspect_sagital <- dim_ap / dim_dv
  aspect_coronal <- dim_ml / dim_dv
  aspect_horizontal <- dim_ap / dim_ml
  
  if(plt_plane == 'sagittal'){
    plt <- plt +
      force_panelsizes(rows = unit(plt_dim_base, 'cm'), cols = unit(plt_dim_base*aspect_sagital, 'cm'))
  } 
  
  if(plt_plane == 'horizontal'){
    plt <- plt +
      force_panelsizes(rows = unit(plt_dim_base, 'cm'), cols = unit(plt_dim_base*aspect_horizontal, 'cm'))
  } 
  
  if(plt_plane == 'coronal'){
    plt <- plt +
      force_panelsizes(rows = unit(plt_dim_base, 'cm'), cols = unit(plt_dim_base*aspect_coronal, 'cm'))
  }
  
  return(plt)
  
}


calculate_pairwise_distances <- function(data) {
  # Select the x, y, z coordinate columns and add a row identifier
  coords <- data %>% select(region, hit_ap, hit_ml, hit_dv)
  
  # Calculate pairwise Euclidean distances
  distance_matrix <- as.matrix(dist(coords %>% select(hit_ap, hit_ml, hit_dv), method = "euclidean"))
  
  # Convert the distance matrix to a tidy dataframe
  tidy_distances <- as.data.frame(as.table(distance_matrix)) %>%
    rename(region1 = Var1, region2 = Var2, distance = Freq) %>%
    mutate(
      region1 = coords$region[as.integer(region1)],
      region2 = coords$region[as.integer(region2)]
    ) %>%
    filter(region1 != region2)  # Remove self-distances if necessary
  
  return(tidy_distances)
}


filter_matrix_unique_combinations <- function(df){                       
  df %>%                                                                 
    rowwise() %>%                                                        # process each row individually
    mutate(pair = paste(sort(c(var_x, var_y)), collapse = "_")) %>%      # create a new column 'pair' with var_x and var_y sorted and joined
    ungroup() %>%                                                        # remove rowwise grouping
    distinct(subject, pair, .keep_all = TRUE)                            # keep distinct rows by subject and pair, retain all other columns
}