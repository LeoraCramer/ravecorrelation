input <- getDefaultReactiveInput()
output <- getDefaultReactiveOutput()
session <- getDefaultReactiveDomain()
local_data <- reactiveValues(aligned_data = NULL,
                             window_cor = NULL)

# Events listed in Main.R
    # Event: requested_baseline or requested_method change
    # Event: requested_frequencies or requested_electrodes change
    # Event: cond_group
    # Event: requested_smooth or requested_span
    # Event: primary_electrode change
    # Event: sliding_window_size change
    # Event: analysis_window change
    # Event: max_type change

# Make the primary electrode selector reactive based on the available options
observeEvent(input$text_electrode, {
  
  # Get the list of electrodes
  elec <- dipsaus::parse_svec(input$text_electrode)
  dipsaus::cat2(paste0(elec, collapse = ','), level='WARNING')
  
  # List of UI selected electrodes
  new_choices = preload_info$electrodes[preload_info$electrodes %in% elec]

  # Save the current primary_electrode selection
  new_selected <- input$primary_electrode
  
  # Reset the current primary_electrode selection if not among the UI selected options
  if(!(new_selected %in% new_choices)) {
    new_selected = new_choices[1]
    dipsaus::cat2("Updating choice", level='WARNING')
  }

  # Update the drop-down menu for primary electrode
  updateSelectInput(session=session, inputId = 'primary_electrode',
                    choices = new_choices, selected = new_selected)

})

# This doesn't work. But it also doesn't cause any problems
# Make the smoothing span conditionally available if smoothing is selected
observeEvent(input$requested_smooth, {
  
  # Get the smoothing type UI selected
  if (input$requested_smooth != 'No Smoothing') {
    
    # Show the slider for the requested span
    conditionalPanel(condition = input$requested_smooth != 'No Smoothing',
#    conditionalPanel(condition = "input$requested_smooth != 'No Smoothing'",
                     sliderInput(inputId = 'requested_span',
                                 label = "Smoothing span",
                                 min = 0.01, max = 1,
                                 value = 0.1, step = 0.01))
  }
})

# # Predictive values if user presses a "predict" button
# observeEvent(input$predict_button, {
# 
#   # Do I include this here?? I copied and pasted from above
#   dipsaus::cat2("Updating choice", level='WARNING')
# 
#   # Set all revlevant inputs to predicted values
#   # If I do these, I need some sort of justification
#   
#   # Suggested span
#   # All possible span values based on what is in comp.R
#   span_opts <- seq(from = 0.01, to = 1, by = 0.01)
#   if (input$requested_smooth == 'loess') {
#     # Find span which reduces the residuals in loess
#     # http://r-statistics.co/Loess-Regression-With-R.html
#     
#     # Loop through each condition group
#     sse <- lapply(power_by_list, function(each_group) {
#       # Loop through each electrode column
#       sse <- apply(each_group$data, 2, function(each_col) {
#         # Loop through each possible span value
#         sapply(span_opts, function(span_val) {
#           # Sum of square error
#           suppressWarnings(sum((loess(each_col ~ time_points, span = span_val)$residuals)^2))
#         })
#       })
#       # Return the mean across columns for each group
#       return(rowMeans(sse))
#     })
#     
#     # The span corresponding to the minimum mean error across groups
#     span_val <- span_opts[which.min(Reduce("+", sse)/length(sse))]
#     
#   } else if (input$requested_smooth == 'spline') {
# 
#     # METHOD1
#     # Loop through each condition group
#     mean_gcv <- lapply(power_by_list, function(each_group) {
#       # Loop through each electrode column
#       mean_gcv <- apply(each_group$data, 2, function(each_col) {
#         # Return the minimum GCV automated by the function
#         # Generalized cross-validation statistic
#         smooth.spline(time_points, each_col, spar = )$spar
#       })
#     })
#     span_val <- round(mean(unlist(mean_gcv)), digits=2)
#     
#     # METHOD2
#     # Loop through each condition group
#     cv <- lapply(power_by_list, function(each_group) {
#       # Loop through each electrode column
#       cv <- apply(each_group$data, 2, function(each_col) {
#         # Loop through each possible span value
#         sapply(span_opts, function(span_val) {
#           # Leave-one-out prediction error
#             smooth.spline(time_points, each_col, spar = span_val)$cv.crit
#         })
#       })
#       # Return the mean across columns for each group
#       return(rowMeans(cv))
#     })
#     
#     # The span corresponding to the minimum mean error across groups
#     span_val <- span_opts[which.min(Reduce("+", cv)/length(cv))]
# 
#   } else {
#     # No change, set to current value
#     span_val <- input$requested_span
#   }
#   
#   # Set the requested_span slider to the calculated span_val
#   updateSliderInput(session=session, inputId = 'requested_span',
#                     #Unsure if these are needed:
#                     #                    label = 'Smoothness', min = 0.01, max = 1, step = 0.01,
#                     value = span_val)
# 
# })
