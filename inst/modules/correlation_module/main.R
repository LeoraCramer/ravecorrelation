# Main algorithm - rave_executes
require(ravecorrelation)

# Initialize inputs
dev_ravecorrelation(expose_functions = TRUE)
mount_demo_subject()
init_module('correlation_module', debug = TRUE)

# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------
######' @auto=TRUE

# User-selected data
requested_electrodes = dipsaus::parse_svec(text_electrode)

# Pull data from the selected recording
time_points = preload_info$time_points
rec_rate = diff(time_points[1:2])
frequencies = preload_info$frequencies
trial = module_tools$get_meta('trials')

# Load the power
p <- module_tools$get_power()

# Event: requested_baseline or requested_method change
# Calculate baseline-corrected data across time(along_dim = 3), per frequency, per trial, per electrode; (unit_dims = 1,2,4)
baselined <- basic_baseline(p, requested_baseline, requested_method)

# Event: requested_frequencies or requested_electrodes change
# Subset the data
baselined_subset <- baselined$subset(
  Frequency = Frequency %within% requested_frequencies,
  Electrode = Electrode %in% requested_electrodes)

# MOCK DATA
# cond_group <- list(
#   list(group_name='AUD', group_conditions=c('known_a', 'last_a', 'drive_a', 'meant_a')),
#   list(group_name='AV', group_conditions=c('known_av', 'last_av', 'drive_av', 'meant_av')),
#   list(group_name='VIS', group_conditions=c('known_v', 'last_v', 'drive_v', 'meant_v'))
# )

# Event: cond_group or error_cloud
# Mean power over time across trials and frequencies per group
#      Function type = 'within'
power_by_electrode_list <- group_power_function(trial, cond_group, baselined_subset, type = 'within', error_cloud)
# Mean power over time across electrodes, trials, and frequencies per group
#      Function type = 'between'
power_list <- group_power_function(trial, cond_group, baselined_subset, type = 'between', error_cloud)
# Essentially the same as the 'within' function but in a format that I can use to plot, with error cloud per electrode over trials
#      Indices would be: experimental_list[[index of cond_group]][[index of electrode]]
experimental_list <- experimental_function(trial, cond_group, baselined_subset,
                                           error_cloud, set_y_range)

# Not sure how to fix line 86 in utils.R
# this isn't right for the "electrode" type, but will work for the group "means" type
# So this is a temporary fix for this list:
power_by_electrode_list <- lapply(power_by_electrode_list, function(p_list) {
  p_list$range <- range(p_list$data)
  return(p_list)
})

# Event: requested_smooth or requested_span
# If a smooth type is selected, call the respective smoothing function
if (requested_smooth != 'No Smoothing') {
  
  # What does this do? Is this a message to the user? Does it get printed in the console?
  dipsaus::cat2('Smoothing POT', level='INFO')
  
  # Switch selects which of the smoothing functions from utils.R to run
  smooth_power <- switch(requested_smooth,
                         "loess" = loess_smooth_power,
                         "spline" = spline_smooth_power)
  
  # Smooth the data using switch function
  power_list <- smooth_power(power_list, requested_span)
  experimental_list <- lapply(experimental_list, function(exp_list) {
    smooth_power(exp_list, requested_span)})
  power_by_electrode_list <- smooth_power(power_by_electrode_list, requested_span)
}

# Cross Correlation
# Function is cross_cor_function(per-group-list, type = 'within')
cross_cor <- cross_cor_function(power_by_electrode_list, type = 'within')
# was trying to figure this out for cross cor of power_list..
# Works fine, but xlab and ylab are not correct. They both say "electrode"
# Function is cross_cor_function(group-comparison-list, type = 'between')
between_cross_cor <- cross_cor_function(list(list(data = do.call(
  cbind, lapply(power_list, function(x) x$data[,1])))), type = 'between')

# Event: primary_electrode change
# Column index of primary electrode
primary_electrode_index <- which(baselined_subset$dimnames$Electrode == primary_electrode)

# Find peak maxima of each electrode
peak <- peaks(power_by_electrode_list, primary_electrode_index)

# Event: sliding_window_size change
# Windowing function
window_cor_results <- windowing(power_by_electrode_list,
                                primary_electrode_index,
                                sliding_window_size)

# Placeholder
# Trying to figure out how to fill out the legend in draw_many_heatmaps
# Electrodes, freq, and time window, and group labels
window_cor_results <- lapply(window_cor_results, function(cor_res) {
  cor_res$electrodes <- requested_electrodes
  cor_res$frequencies <- requested_frequencies
  cor_res$time <- range(time_points)
  cor_res$group_labels <- names(window_cor_results)
  return(cor_res)
})

# Event: analysis_window change
# Time Lag function
time_lag_cor_results <- time_lag(power_by_electrode_list,
                                 primary_electrode_index,
                                 analysis_window)
# Placeholder
time_lag_cor_results <- lapply(time_lag_cor_results, function(cor_res) {
  cor_res$electrodes <- requested_electrodes
  cor_res$frequencies <- requested_frequencies
  cor_res$time <- range(time_points)
  cor_res$group_labels <- names(window_cor_results)
  return(cor_res)
})


# Event: max_type change
# The results to be used to align the data based on the max type
results <- if (max_type == 'Peak Max') peak else time_lag_cor_results

# Align the data by the user-selected max type
#      Maximum time lag cor, abs. max time lag cor, and lag to peak maxima
aligned_data <- align_data(experimental_list, results, max_type)

# Assign to local_data for Shiny customization
local_data$aligned_data <- aligned_data
local_data$window_cor <- window_cor_results

# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# Debug
ravecorrelation::dev_ravecorrelation(expose_functions = TRUE)

# Debug - offline:
# main = ravecorrelation:::debug_module('correlation_module')
mount_demo_subject()
main = rave::get_module('ravecorrelation', 'correlation_module', local=TRUE)
ret = main()
result = ret$results

result$get_value('preload_info')

# Debug - online:
ravecorrelation::dev_ravecorrelation(expose_functions = TRUE)
mount_demo_subject()
view_layout('correlation_module')

# Production - Deploy as RAVE module
# Always Ctrl/cmd+shift+B first if you want to deploy it online
rm(list = ls(all.names = TRUE)); rstudioapi::restartSession()
module = rave::get_module(package = 'ravecorrelation', module_id = 'correlation_module')
rave::init_app(module)
