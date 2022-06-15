# Plots used to render outputs

# Plot power data
#     Reusable function called by other functions below
plot_data <- function(power_over_time) {
  
  # Call RAVE built-in plotting function
  ravebuiltins:::time_series_plot(
    plot_data = power_over_time,
    PANEL.FIRST = ravebuiltins:::time_series_decorator(),
    PANEL.LAST = function(...) {
      ravebuiltins:::legend_decorator(power_over_time, include = "name")
    }
  )
}

# Plot power data - between groups
#      Plot the mean of each group's power in a single graph
plot_power_between_groups <- function(result, ...) {
  
  # Prepare results
  power_over_time <- result$get_value('power_list')
  
  # Give user an error message if data is still loading
  shiny::validate(shiny::need(!is.null(power_over_time), message = "Loading"))
  
  # Call function to plot the data
  plot_data(power_over_time)
  
}

# Plot power data - within groups
#     Plot each condition group's electrodes in side-by-side plots
plot_power_per_electrode_within_groups <- function(result, ...) {
  
  # Prepare results
  power_over_time <- result$get_value('experimental_list')

  # Give user an error message if data is still loading
  # Error when a condition group is selected but empty
  shiny::validate(shiny::need(!is.null(power_over_time), message = "Loading"))
  
  # Create the layout for the side-by-side plots
  # Placeholder, unsure how the layout would be ideally done
  layout(mat = matrix(1:length(power_over_time), nrow = 1))
  
  # Call function to plot each group's set of data
  for (i in 1:length(power_over_time)) {
    plot_data(power_over_time[[i]])
  }
}

# Not sure this is a thing. What would all the groups be aligned by????
# Plot power data aligned by lag - between groups
aligned_plot_data_between_groups <- function(result, ...) {
}

# Plot power data aligned by lag - within groups
#     Plot each condition group's electrodes in side-by-side plots
aligned_plot_per_electrode_within_groups <- function(result, ...) {
  
  # Prepare results
  power_over_time <- result$get_value('aligned_data')
  max_type <- result$get_value('max_type')
  analysis_window <- result$get_value('analysis_window')
  e1 <- result$get_value('primary_electrode_index')
  
  # Give user an error message if data is still loading
  # Need to fix error when a condition group is selected but empty
  shiny::validate(shiny::need(!is.null(power_over_time), message = "Loading"))
  
  # Create the layout for the side-by-side plots
  # Placeholder, unsure how the layout would be ideally done
  layout(mat = matrix(1:length(power_over_time), nrow = 1))
  
  # Call function to plot each group's set of data
  for (i in 1:length(power_over_time)) {
    plot_data(power_over_time[[i]])
    
    # Vertical lines at peaks
    if (max_type == 'Peak Max') {
      abline(lty = 2,
        v = power_over_time[[i]][[1]]$x[which.max(power_over_time[[i]][[1]]$data[,1])])

    } else {
      # Vertical lines indicating analysis window start and end
      abline(v = analysis_window, lty = 2, lwd = 2)
    }
  }
}

######################################################################
#################      Cross Correlation      ########################
######################################################################

plot_cross_cor <- function(result,...) {
  
  # Prepare results
  cross_cor <- result$get_value('cross_cor')
  
  # Give user an error message if data is still loading
  shiny::validate(shiny::need(!is.null(cross_cor), message = "Loading"))
  
  # Call built-in plotting function
  ravebuiltins:::draw_many_heat_maps(
    # Plot the cross correlation
    cross_cor,
    # Default graphics
    PANEL.LAST = ravebuiltins:::spectrogram_heatmap_decorator()
  )
}

plot_cross_cor_between_groups <- function(result,...) {
  
  # Prepare results
  cross_cor <- result$get_value('between_cross_cor')
  
  # Give user an error message if data is still loading
  shiny::validate(shiny::need(!is.null(cross_cor), message = "Loading"))
  
  # Call built-in plotting function
  ravebuiltins:::draw_many_heat_maps(
    # Plot the cross correlation
    cross_cor,
    # Default graphics
    PANEL.LAST = ravebuiltins:::spectrogram_heatmap_decorator()
  )
}

######################################################################
####################      Correlation      ###########################
######################################################################

# Plot correlation heatmap using image
plot_cor <- function(cor_results) {
  
  # Call built-in plotting function
  ravebuiltins:::draw_many_heat_maps(
    # Plot the cross correlation
      #Do plots get titles?? main = cor_results$main
      # Vertical Line at 0?? abline(v = 0, lty = 2)
      # Correlation maxima as points?? might need a different y, though.. points(x = cor_results$x[cor_results$max_row], y = cor_results$y, pch = 19, cex = 1)
    cor_results,
    # Default graphics
    PANEL.LAST = ravebuiltins:::spectrogram_heatmap_decorator()
  )
  
}

# Time lag correlation plot call
plot_time_lag_cor <- function(result,...) {
  
  # Prepare results
  cor_results <- result$get_value('time_lag_cor_results')
  
  # Give user a message if data is still loading
  shiny::validate(shiny::need(!is.null(cor_results), message = "Loading"))
  # Give user an error message is computation cannot be done because more than 1 electrode is needed for correlation analysis
  # message = "Select more than one electrode for analysis"))
  
  plot_cor(cor_results)
  
}

# Windowed correlation plot call
plot_window_cor <- function(result,...) {
  
  # Prepare results
  cor_results <- result$get_value('window_cor_results')
  
  # Give user a message if data is still loading
  shiny::validate(shiny::need(!is.null(cor_results), message = "Loading"))
  # Give user an error message is computation cannot be done because more than 1 electrode is needed for correlation analysis
  # message = "Select more than one electrode for analysis"))
  
  plot_cor(cor_results)
  
}

######################################################################
##################         Analysis Window         ###################
######################################################################

# Plot analysis window for the time_lag function
plot_analysis_window <- function(result,...) {
  
  # Prepare results
  power_over_time <- result$get_value('experimental_list')
  analysis_window <- result$get_value('analysis_window')

  # Give user a message if data is still loading
  shiny::validate(shiny::need(!is.null(power_over_time[[1]][[1]]$data), message = "Loading"))
  # Give user an error message is computation cannot be done because more than 1 electrode is needed for correlation analysis
  # message = "Select more than one electrode for analysis"))
  
  # Create the layout for the side-by-side plots
  # Placeholder, unsure how the layout would be ideally done
  layout(mat = matrix(1:length(power_over_time), nrow = 1))
  
  # Call function to plot each group's set of data
  for (i in 1:length(power_over_time)) {
    plot_data(power_over_time[[i]])
    
    # Vertical lines indicating analysis window start and end
    abline(v = analysis_window, lty = 2, lwd = 2)
  }
}

######################################################################
####################         Window Span         ####################
######################################################################

# Plot a visual of the sliding window span for the windowing function
# Goal is to make this an animation
plot_sliding_window <- function(result,...) {
  
  # Prepare results
  power_over_time <- result$get_value('experimental_list')
  sliding_window_size <- result$get_value('sliding_window_size')
  e1 <- result$get_value('primary_electrode_index')

  # Give user a message if data is still loading
  shiny::validate(shiny::need(!is.null(power_over_time[[1]][[1]]$data), message = "Loading"))
  # Give user an error message is computation cannot be done because more than 1 electrode is needed for correlation analysis
  # message = "Select more than one electrode for analysis"))
  
  # Create the layout for the side-by-side plots
  # Placeholder, unsure how the layout would be ideally done
  layout(mat = matrix(1:length(power_over_time), nrow = 1))
  
  # Call function to plot each group's set of data
  for (i in 1:length(power_over_time)) {
    plot_data(power_over_time[[i]])
    
    # Representative window
    # Center around the peak of the primary electrode
    center <- which.max(power_over_time[[i]][[e1]]$data[,1])
    half_window <- floor(sliding_window_size/2)
    # Find the times (x-values) corresponding to the representative window
    sliding_window <- power_over_time[[i]][[e1]]$x[c(
      center - half_window, center + half_window)]
    
    # Vertical lines indicating analysis window start and end
    abline(v = sliding_window, lty = 2, lwd = 2)
  }
}

######################################################################
################          Tables and Values          #################
######################################################################

# Window Correlation Output
# Sliding Window Maxima
print_window_cor_maxima <- function(result, ...){
  cor_res <- result$get_value('window_cor_results')
  cor_res <- do.call(cbind, lapply(cor_res, function(res) res$max_time))
  return(cor_res)
}

# Aligned Data Output
# Time corresponding to lag
print_alignment_times <- function(result, ...){
  
  # Prepare data
  max_type <- result$get_value('max_type')
  results <- result$get_value('results')
  
  # Pull alignment times for each group based on the max type
  alignment_times <- (if (max_type == 'Absolute Max Correlation')
    do.call(cbind, lapply(results, function(res) res$abs_max_time)) else
      do.call(cbind, lapply(results, function(res) res$max_time)))
  
  return(alignment_times)
}
