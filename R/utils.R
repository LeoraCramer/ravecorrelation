# Put utility functions here, use `@export` to make function visible


######################################################################
####################       Baseline       ############################
######################################################################

# Baseline the data
#     Explain what this does
basic_baseline <- function(power, baseline_window, analysis_method,
                           along_dim = 3, unit_dims = (1:4)[-along_dim]){
  
  baseline_window_index = which(power$dimnames$Time %within% baseline_window)
  
  # Baseline the data based on UI requested baseline arguments
  bl <- dipsaus::baseline_array(power$get_data(),
                                along_dim = along_dim,
                                baseline_indexpoints = baseline_window_index,
                                unit_dims = unit_dims,
                                method = analysis_method)
  
  # Convert to type ECoGTensor
  return(
    ECoGTensor$new(bl, dim = dim(power), dimnames = dimnames(power),
                   varnames = power$varnames, hybrid = FALSE)
  )
}

######################################################################
###################     Power Over Time     ##########################
######################################################################

# Get the mean power over time and standard error cloud for each condition group
#     Explain what this does
group_power_function <- function(trial, cond_group, baselined_subset, type, error_cloud) {
  
  power_list <- lapply(cond_group, function(each_group) {
    
    # Subset the data based on trials corresponding to each condition group
    baselined_subset <- baselined_subset$subset(
      # Subset by trials
      Trial = Trial %in%
        # The trial numbers corresponding to the conditions user selects for each group
        trial$Trial[trial$Condition %in% each_group$group_conditions])
    
    # Collapse (aggregate)
    #     dim 1: trial
    #     dim 2: frequency
    #     dim 3: time
    #     dim 4: electrode
    
    if (type == 'between') {
      # Collapse over electrodes and frequencies
      #     Keep dims 1 & 3 for trials and time
      each_group_data <- baselined_subset$collapse(keep = c(1,3))
      
      # Considering adding option to show or hide st dev error cloud
      # Are there other error types? Should this be a graphical UI option?
      each_sd <- if (error_cloud=="Show") {
        apply(each_group_data, 2, sd)
      } else {
        rep(0, dim(baselined_subset)[3])
      }
      
      # Structure the data as 2-column matrix with mean power over time and standard deviation of trials
      each_group_data <- cbind(mean = rowMeans(t(each_group_data)),
                               sd = each_sd)
      
    } else if (type == 'within') {
      # Collapse over electrodes and frequencies
      #     Keep dims 3 & 4 for time and electrode
      each_group_data <- baselined_subset$collapse(keep = 3:4) %>%
        # Set column names (electrodes)
        set_colnames(baselined_subset$dimnames$Electrode)
    }
    
    # Graphic attributes of data
    attr(each_group_data, 'xlab') <- "Time (s)"
    attr(each_group_data, 'ylab') <- "Power"
    
    # Return data, x-axis (time), and y-range as list
    list(data = each_group_data,
         # Time on the x--axis
         x = dimnames(baselined_subset)$Time,
         # Range of power on y-axis plus minus space for the error clouds
         range = range(plus_minus(each_group_data)),
         # Name each group by the group name given in the UI
         name = each_group$group_name,
         # Switch on/off graph:
         # if there's a missing group selection, has_trials to F
         has_trials = TRUE)
  })
  
  # Name each group's power over time list by the UI group name
  names(power_list) <- unlist(lapply(cond_group, function(cg) { cg$group_name }))
  return(power_list)
}

# See if I can make this work...
experimental_function <- function(trial, cond_group, baselined_subset,
                                  error_cloud, set_y_range) {
  
  power_list <- lapply(cond_group, function(each_group) {
    
    # Keep comments:
    
    # Need explanation here
    each_group_data <- baselined_subset$subset(
      Trial = Trial %in%
        trial$Trial[trial$Condition %in% each_group$group_conditions])$collapse(
          keep = 3:4) %>%
      set_colnames(baselined_subset$dimnames$Electrode)
    
    # Loop through each electrode and find the stdev of trials for error cloud
    #     Collapse over frequencies (dim 2)
    #     Then apply with margin = c(2,3) to keep time (dim 2) and electrodes (dim 3)
    if (error_cloud == "Show") {
      each_sd <- apply(baselined_subset$collapse(keep = c(1,3,4)), c(2,3), sd)
    } else {
      # Or if we don't want an error cloud:
      # Need to come back to this...
      each_sd <- matrix(rep(0, dim(baselined_subset)[3]*2), ncol = 2)
    }
    
    # I'm not sure how else to combine column pairs, but this is what I did
    # cbind the mean in the 1st column and the sd in the 2nd column for each electrode
    each_group_data <- set_names(
      lapply(1:dim(each_group_data)[2], function(i) {
        each_col <- cbind(mean = each_group_data[,i], sd = each_sd[,i])
        attr(each_col, 'xlab') <- "Time (s)"
        attr(each_col, 'ylab') <- "Power"
        each_col <- list(data = each_col,
                         x = dimnames(baselined_subset)$Time,
                         range = range(plus_minus(each_col)),
                         name = paste("Electrode ", baselined_subset$dimnames$Electrode),
                         has_trials = TRUE)
        return(each_col)
      }), baselined_subset$dimnames$Electrode)
    
    return(each_group_data)
  })
  
  # If power, the y-range, is the same for all graphs, then reset the range to
  # the extrema of the dataset, across all electrodes and all conditions
  if (set_y_range == 'Range of all groups') {
    
    set_y_range_val <- range(sapply(power_list, function(each_group) {
      sapply(each_group, function(each) {
        each$range
      })
    }))
    
    power_list <- lapply(power_list, function(each_group) {
      lapply(each_group, function(each) {
        each$range <- set_y_range_val
        return(each)
      })
    })
  }
  
  names(power_list) <- unlist(lapply(cond_group, function(cg) { cg$group_name }))
  return(power_list)
}

######################################################################
###################       Smoothing       ############################
######################################################################

# Smooth using the loess method
#     Explain what this does
loess_smooth_power <- function(power_list, span_val = 0.1) {
  
  # If smoothing data in the mean power list
  if (colnames(power_list[[1]]$data)[2] == "sd") {
    # For each group in the power over time lists
    power_list <- lapply(power_list, function(power_over_time) {
      # Predict with loess over the mean only (column 1), not the sd (column 2)
      power_over_time$data[,1] <- predict(loess(power_over_time$data[,1] ~ power_over_time$x, span = span_val))
      return(power_over_time)
    })
  } else{ # If smoothing power over time per electrode
    # For each group in the power over time lists
    power_list <- lapply(power_list, function(power_over_time) {
      # Apply predict with loess over each column
      power_over_time$data <- apply(power_over_time$data, 2, function(electrode) {
        predict(loess(electrode ~ power_over_time$x, span = span_val))
      })
      return(power_over_time)
    })
  }
  return(power_list)
}

# Smooth using the spline method
#     Explain what this does
spline_smooth_power <- function(power_list, span_val = 0.1) {
  
  # If smoothing data in the mean power list
  if (colnames(power_list[[1]]$data)[2] == "sd") {
    # For each group in the power over time lists
    power_list <- lapply(power_list, function(power_over_time) {
      # Predict with spline over the mean only (column 1), not the sd (column 2)
      power_over_time$data[,1] <- predict(smooth.spline(power_over_time$x,
                                                        power_over_time$data[,1],
                                                        spar = span_val))$y
      return(power_over_time)
    })
  } else{ # If smoothing power over time per electrode
    # For each group in the power over time lists
    power_list <- lapply(power_list, function(power_over_time) {
      
      # Apply predict with spline over each column (electrode)
      apply(power_over_time$data, 2, function(electrode) {
        predict(smooth.spline(power_over_time$x, electrode,
                              # Specify spline smoothing parameters:
                              spar = span_val))$y
      })
      
      return(power_over_time)
    })
  }
  return(power_list)
}

######################################################################
###################       Cross Correlation       ####################
######################################################################

# Simple cross correlation function
#     The main difference between this and windowing is
#     that this is across the entire time interval
cross_cor_function <- function(power_list, type) {
  
  # Apply over each condition group's power over time data
  lapply(power_list, function(power_over_time) {
    
    # Set x- and y-axes
    # Placeholder for issue with labeling x- and y-axes
    # If columns are not numeric, convert them to numeric
    xy_axes <- tryCatch({ as.integer(colnames(power_over_time$data))
    }, warning = function(w) { seq_along(colnames(power_over_time$data)) })
    xy_lab <- ifelse(type == 'within', 'Electrode', 'Condition Group')
    
    # Run and store the Pearson cross correlation
    results <- list(data = cor(power_over_time$data),
                    x = xy_axes,
                    y = xy_axes,
                    main = "Cross Correlation Heatmap",
                    # Switch on/off graph:
                    # if there's a missing group selection, has_trials to F
                    has_trials = TRUE,
                    # Set z range
                    # Pearson correlation values standardized from -1 to +1
                    range = c(-1,1))
    
    # Graphic attributes of cor results
    attr(results$data, 'xlab') = xy_lab
    attr(results$data, 'ylab') = xy_lab
    attr(results$data, 'zlab') = 'Pearson Correlation'
    
    return(results)
  })
}

######################################################################
##############       Windowed Correlations       #####################
######################################################################

# Windowing Correlation Function
#     Explain what this does
windowing <- function(power_list, e1, sliding_window_size) {
  
  # Apply over each condition group
  lapply(power_list, function(power_over_time) {
    
    # Return NULL if only one electrode selected
    if (dim(power_over_time$data)[2] < 2) {
      return(NULL)
    } else {
      
      # Half of the window size
      half_window <- floor(sliding_window_size/2)
      # Set window start and window end based on half-window size
      wind_st <- half_window + 1
      wind_end <- length(power_over_time$x) - half_window
      
      # Correlate all other electrodes against the chosen electrode over each window
      cor_results <- t(sapply(wind_st:wind_end, function(center) {
        # Correlate the windowed subset
        cor(power_over_time$data[(center-half_window):(center+half_window), ])[e1,-e1] 
      }))
      
      # Transpose and set column names if there is only one comparison electrode
      if (dim(power_over_time$data)[2] < 3) {
        # Ensures that the single-column result will be in the same format as above
        cor_results <- set_colnames(t(cor_results), colnames(power_over_time$data)[-e1])
      }
      
      # x-axis, Time, Center of window
      x <- power_over_time$x[wind_st:wind_end]
      
      # Indices of maxima and absolute maxima
      max_row <- apply(cor_results, 2, which.max)
      abs_max_row <- apply(abs(cor_results), 2, which.max)
      
      # Return the correlation results
      results <- list(
        # Store data as item in results
        data = cor_results,
        # x-axis: Time axis of the window center
        x = x,
        # y-axis: Comparison electrodes
        y = as.integer(colnames(cor_results)),
        # Switch on/off graph:
        # if there's a missing group selection, has_trials to F
        has_trials = TRUE,
        # Set z range
        # Pearson correlation values standardized from -1 to +1
        range = c(-1,1),
        # Indices of the maxima and absolute maxima
        max_row = max_row,
        abs_max_row = abs_max_row,
        # Time corresponding to the max and abs max window center
        max_time = sapply(max_row, function(mrow) x[mrow]),
        abs_max_time = sapply(abs_max_row, function(mrow) x[mrow]),
        # Unsure if this would get used, but a main plot title
        main = paste0("Windowed\nPower Correlation Heatmap\nVersus Electrode ",
                      colnames(power_over_time$data)[e1])
      )
      
      # Graphic attributes of cor results
      attr(results$data, 'xlab') = 'Center of Window (s)'
      attr(results$data, 'ylab') = 'Comparison Electrode'
      attr(results$data, 'zlab') = 'Pearson Correlation'
      
      return (results)
    }
  })
}

######################################################################
##############          Time Lag Correlations        #################
######################################################################

# Time Lag Correlation
#     Explain what this does
time_lag <- function(power_list, e1, analysis_window) {
  
  # Apply over each condition group
  lapply(power_list, function(power_over_time) {
    
    # Return NULL if only one electrode selected
    if (dim(power_over_time$data)[2] < 2) {
      return(NULL)
    } else {
      
      # Row indices of the analysis window
      analysis_index <- power_over_time$x %within% analysis_window
      
      # Determine the span of rows to compare against the analysis window:
      window_span <- 1:(1 + length(power_over_time$x) - sum(analysis_index))
      
      # Run each other electrode time lagged against fixed first electrode
      #     Moves comparison electrode from the left-most lag to the right-most
      #     first iteration is rows (nrow(power_over_time)-sum(analysis_index)):nrow(power_over_time)
      #     final iteration is rows 1:sum(analysis_index)
      
      if (dim(power_over_time$data)[2] >= 3) {
        
        # For multiple comparison electrodes
        cor_results <- set_colnames(t(sapply(window_span, function(timerow) {
          cor(power_over_time$data[timerow:(timerow+sum(analysis_index)-1),-e1],
              power_over_time$data[analysis_index, e1])
        })), colnames(power_over_time$data)[-e1])
        
      } else {
        # For the case where there is just one comparison electrode
        #     Ensures that the single-column result will be in the same format as above
        cor_results <- set_colnames(as.matrix(sapply(window_span, function(timerow) {
          cor(power_over_time$data[timerow:(timerow + sum(analysis_index)-1),-e1],
              power_over_time$data[analysis_index, e1])
        })), colnames(power_over_time$data)[-e1])
      }
      
      # Lag time axis
      x <- power_over_time$x[window_span] - power_over_time$x[which(analysis_index)[1]]
      # Returns the indices of the maxima and absolute maxima
      max_row <- apply(cor_results, 2, which.max)
      abs_max_row <- apply(abs(cor_results), 2, which.max)
      
      # Return the correlation results
      results <- list(
        # Store data as item in results
        data = cor_results,
        # Correlation plot axes
        x = x,
        y = as.integer(colnames(cor_results)),
        # Switch on/off graph:
        # if there's a missing group selection, has_trials to F
        has_trials = TRUE,
        # Set z range
        # Pearson correlation values standardized from -1 to +1
        range = c(-1,1),
        # Row indices
        max_row = max_row,
        abs_max_row = abs_max_row,
        # Returns the lag time to max or abs max correlation
        max_time = sapply(max_row, function(electrode) {x[electrode]}),
        abs_max_time = sapply(abs_max_row, function(electrode) {x[electrode]}),
        main = paste0("Time Lag\nPower Correlation Heatmap\nVersus Electrode ",
                      colnames(power_over_time$data)[e1])
      )
      
      # Graphic attributes of cor results
      attr(results$data, 'xlab') = 'Time Lag (s)'
      attr(results$data, 'ylab') = 'Comparison Electrode'
      attr(results$data, 'zlab') = 'Pearson Correlation'
      
      return (results)
    }
  })
}


######################################################################
###################          Peak Maxima          ####################
######################################################################

# Peak maximum of signals
peaks <- function(power_list, e1) {
  
  # Apply over each condition group
  lapply(power_list, function(power_over_time) {
    
    # Return NULL if only one electrode selected
    if (dim(power_over_time$data)[2] < 2) {
      return(NULL)
    } else {
      
      # Peak max
      # Apply over each comparison electrode
      max_row <- apply(as.matrix(power_over_time$data), 2, which.max)[-e1]
      # Lag time axis, centered around the peak of the primary electrode
      x <- power_over_time$x - power_over_time$x[which.max(power_over_time$data[,e1])]
      
      # Return results in a list
      list(
        max_row = max_row,
        # Lag time to peak
        #     The time lag needed to align all each signal by the peak max,
        #     where the primary electrode remains stationary at axis time 0
        max_time = sapply(max_row, function(electrode) {x[electrode]}),
        x = x
      )
    }
  })
}

######################################################################
###################          Align by Max         ####################
######################################################################

# Align_data Function
# Align data along the time axis by lag
align_data <- function(power_by_list, results, max_type) {
  
  # Return NULL if user did not select multiple electrodes for comparison
  if (length(power_by_list[[1]]) < 2) {
    return(NULL)
  } else {
    
    # Save the electrode names to add back in after alignment
    cond_group_names <- names(power_by_list)
    
    # Loop through each condition group
    power_by_list <- lapply(1:length(power_by_list), function(group_i) {
      
      # Max row is determined by max type (max vs absolute max)
      max_row <- (if (max_type == 'Absolute Max Correlation')
        results[[group_i]]$abs_max_row else results[[group_i]]$max_row)
      
      # Save the electrode names to add back in after alignment
      electrode_names <- names(power_by_list[[group_i]])
      
      # Apply over each electrode list item
      # power_by_list[["i" condition group]][[electrode index]]
      power_by_list[[group_i]] <- lapply(1:length(power_by_list[[group_i]]), function(electrode_i) {
        
          # The number of rows of the lag
          align <- tryCatch({
            max_row[[electrode_names[electrode_i]]] -
              # Which row corresponds to 0 lag of the results time axis
              which(results[[group_i]]$x == 0)
          }, error = function(e) {
            # If column corresponds to the primary electrode, assign 0 lag
            0
          })
          
          # Save the attributes to add back in after alignment
          data_attr <- attributes(power_by_list[[group_i]][[electrode_i]]$data)

          # Aligned data object
          power_by_list[[group_i]][[electrode_i]]$data <- 
            # If positive lag
            if (align > 0) {
              # Apply identical shift over both columns (mean and sd)
              apply(power_by_list[[group_i]][[electrode_i]]$data, 2, function(data_i) {
                
                # Move the data up by that number of rows
                c(data_i[(align+1):length(data_i), drop = FALSE],
                  # Fill NAs in the tail rows
                  matrix(rep(NA, align)))
              })
              
              # If negative lag
            } else if (align < 0) {
              
              # Same as above, apply over both columns
              apply(power_by_list[[group_i]][[electrode_i]]$data, 2, function(data_i) {
                
                # Fill NAs in the header rows
                c(matrix(rep(NA, abs(align))),
                  # Move the data down by that number of rows
                  data_i[1:(length(data_i) + align)])
              })
              
              # If align=0, ie if there is 0 lag
            } else {
              # Return the data unaltered
              power_by_list[[group_i]][[electrode_i]]$data
            }
          
          # Retain original data attributes
          # Save the attributes to add back in after alignment
          attributes(power_by_list[[group_i]][[electrode_i]]$data) <- data_attr
          
          # Return the whole list item
          return(power_by_list[[group_i]][[electrode_i]])
      })
      
      # Add electrode names back in
      names(power_by_list[[group_i]]) <- electrode_names
      
      # Return the aligned data per condition group
      return(power_by_list[[group_i]])
      
      })
    
    # Add condition group names back in
    names(power_by_list) <- cond_group_names
    
    # Return the aligned data list of lists
    return(power_by_list)
  }
}
