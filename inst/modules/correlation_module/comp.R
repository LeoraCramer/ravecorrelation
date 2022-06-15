# File defining module inputs, outputs

# ----------------------------------- Debug ------------------------------------
require(shiny)
require(rave)
require(ravecorrelation)

env = dev_ravecorrelation(TRUE)

#' Load subject for debugging
#' Make sure this is executed before developing the module to make sure
#' at least one subject is loaded
mount_demo_subject()


# >>>>>>>>>>>> Start ------------- [DO NOT EDIT THIS LINE] ---------------------


#  ----------------------  Initializing Global variables -----------------------
load_scripts(
  "inst/modules/correlation_module/event_listeners.R",
#  "R/output_functions.R",
  "inst/modules/correlation_module/outputs.R",
  asis = TRUE
)


#' define_initialization is executed every time when:
#'   *** a subject is loaded
#'
#' You can use this function to do:
#'   1. Check if data is complete
#'   2. Define some "global" variables
#'
#' Check package vignette to see the explanation

define_initialization({
  # Enter code to handle data when a new subject is loaded
  rave_checks('power referenced')
  
  # # Pull data from the selected recording
  # time_points = preload_info$time_points
  # rec_rate = diff(time_points[1:2])
  # frequencies = preload_info$frequencies
  # trial = module_tools$get_meta('trials')
  # 
  # # Load the power
  # p <- module_tools$get_power()
})




#  ---------------------------------  Inputs -----------------------------------
#' Define inputs
#'
#' Use function `define_input` to define inputs.
#' Make sure rave toolbox (dev_[your_package_name]) is loaded
#'
#' @Usage: define_input(definition, init_args, init_expr)
#'
#' @param definition defines input types, for example:
#'     textInput(inputId = 'text_electrode', label = 'Electrode')
#'   defines a character variable `text_electrode` as one of the inputs
#'
#' Here are some possible types
#'   1. textInput is an input for characters
#'   2. numericInput is for numbers
#'   3. sliderInput is for number or number ranges
#'   4. selectInput is to provide choices
#'   5. checkboxInput is for Yes/No options
#'   6. compoundInput is for multiple group inputs
#'   7. customizedUI is for advanced UI controls
#'
#'   Most of basic UI widgets can be found at:
#'       https://shiny.rstudio.com/gallery/widget-gallery.html
#'
#'   The easiest way to look for usage is using `help` function:
#'   help('textInput'), or ??textInput
#'
#'
#' @param init_args,init_expr define the action when a subject is loaded
#'
#' Use the definition
#'
#'     textInput(inputId = 'text_electrode', label = 'Electrode')
#'
#' as an example. You might want label to update according to electrodes loaded.
#' In this case, you can assign
#'     init_args = 'label'
#' indicating the label of this input will update according to actual data.
#'
#' You may also change multiple arguments. The following code is an extended example
#'
define_input(
  definition = textInput(inputId = 'text_electrode', label = 'Electrode'),

  # label will tell users which electrodes are loaded
  # value will be the first electrode

  init_args = c('label', 'value'),
  init_expr = {

    # check ?rave_prepare for what kind of data are loaded
    loaded_electrodes = preload_info$electrodes

    # Generate text for loaded electrodes
    text = dipsaus::deparse_svec(loaded_electrodes)

    # Update
    label = paste0('Electrode (', text, ')')
    value = loaded_electrodes
  }
)


# User-selected groupings by conditions
define_input_condition_groups(
  inputId = 'cond_group', label = 'Condition Group', initial_groups = 2,
  max_group = 20, min_group = 2, label_color = 'grey40'
  # init_args = NULL, init_expr = NULL
)

define_input(
  sliderInput(inputId = 'requested_frequencies', label = 'Frequency Window',
              min = 0, max = 1, value = 0:1, step = 1, round = TRUE),
  
  init_args = c('min', 'max', 'value'),
  
  init_expr = {
    # The frequency labels are defined in:
    # rstudioapi::navigateToFile(file.path(rave::rave_options('data_dir'), 'demo','DemoSubject','rave', 'meta','frequencies.csv'))
    
    min = min(preload_info$frequencies)
    max = max(preload_info$frequencies)
    value = c(min,max)
  }
)

define_input(
  sliderInput(inputId = 'requested_baseline', label = 'Baseline Window',
              min = 0, max = 1, value = 0:1),
  
  init_args = c('min', 'max', 'value'),
  
  init_expr = {
    min = min(preload_info$time_points)
    max = max(preload_info$time_points)
    value = c(min,0)
  }
)

define_input(
  selectInput(inputId = 'requested_method', label = 'Baseline Method',
              choices = c('percentage', 'sqrt_percentage', 'decibel', 'zscore', 'sqrt_zscore'),
              selected = 'percentage')
)

define_input(
  selectInput(inputId = 'requested_smooth', label = 'Type of Smoothing',
              choices = c('No Smoothing', 'loess', 'spline'), selected = 'No Smoothing')
)

define_input(
    sliderInput(inputId = 'requested_span', label = "Smoothness",
                min = 0.01, max = 1, value = 0.1, step = 0.01))

define_input(
  selectInput(inputId = 'primary_electrode', label = 'Primary Electrode',
              choices = NULL, selected = NULL),

    init_args = c('choices', 'selected'),
  
  init_expr = {
    choices = preload_info$electrodes
    selected = preload_info$electrodes[1]
  }
)

define_input(
  sliderInput(inputId = 'sliding_window_size', label='Sliding Window Size',
              min = 0, max = 1000, value = 1, step = 2),
  
  init_args = c('min', 'max', 'value'),
  
  init_expr = {
    # I arbitrarily set 10 rows as min. Limit would be 3
    min = 11
    # I set half the data as the max
    # Added 1 for nearest odd number
    max = 1 + 2*ceiling(length(preload_info$time_points)/4)
    # Recommended 0.4 sec span, and based on frame rate of data
    # Added 1 for nearest odd number
    value = 1 + 0.4/diff(preload_info$time_points[1:2])
  }
)

define_input(
  sliderInput(inputId = 'analysis_window', label='Analysis Window',
              min = 0, max = 1, value = 0:1, step = 0.01),

  init_args = c('min', 'max', 'value'),

  init_expr = {
    min = preload_info$time_points[2]
    max = preload_info$time_points[length(preload_info$time_points)-1]
    # Default symmetric center around middle and w reasonable span of
    # 0.2 sec on each side, based on frame rate of data? Just an idea
    value = preload_info$time_points[ceiling(length(preload_info$time_points)/2)
            + (0.2/diff(preload_info$time_points[1:2])*c(-1,1))]
  }
)

define_input(
  selectInput(inputId = 'max_type', label = 'Maxima Type for Curve Alignment',
              choices = c('Max Correlation', 'Absolute Max Correlation', 'Peak Max'),
              selected = 'Max Correlation')
)

define_input(
  selectInput(inputId = 'error_cloud', label = 'Show / Hide Standard Deviation Error Cloud',
              choices = c('Show', 'Hide'),
              selected = 'Show')
)

define_input(
  selectInput(inputId = 'set_y_range', label = 'Set y-axis range',
              choices = c('Range of all groups', 'Individual group range'),
              selected = 'Range of all groups')
)


# the input_layout list is used by rave to determine order and grouping of layouts
input_layout <- list(
  'Select electrodes' = list('text_electrode'
  ),
  '[-]Analysis parameters' = list(
    'requested_baseline',
    'requested_method',
    'requested_frequencies',
    'requested_smooth',
    'requested_span'
  ),
  '[-]Correlation Parameters' = list(
    'primary_electrode',
    'sliding_window_size',
    'analysis_window',
    'max_type'
  ),
  '[-]Plot Options' = list(
    'error_cloud',
    'set_y_range'
  ),
  '[-]Select Conditions' = list(
    'cond_group'
  )
)

# End of input
# ----------------------------------  Outputs ----------------------------------
#' Define Outputs
#'
#' Use function `define_output` to define outputs.
#' Make sure rave toolbox (dev_[your_package_name]) is loaded.
#'
#' @Usage: define_output(definition, title, width, order)
#'
#' @param definition defines output types, for example:
#'     verbatimTextOutput('text_result')
#'   defines output type that dumps whatever is printed by function `text_result`
#'
#' Here are some possible types
#'   1. textOutput is an output for characters
#'   2. verbatimTextOutput is for console print
#'   3. plotOutput is for figures
#'   4. tableOutput is to tables
#'   5. customizedUI is for advanced UI controls
#'
#'   There are lots of output types and R packages such as DT, threejsr can provide
#'   very useful output types. Please check vignettes.
#'
#'   The easiest way to look for usage is using `help` function:
#'   help('verbatimTextOutput'), or ??verbatimTextOutput
#'
#'
#' @param title is the title for output
#'
#' @param width an integer from 1 to 12, defines the percentage of output width
#'   12 means 100% width, 6 means 50% and 4 means 33% width.
#'
#' @param order numeric order of outputs. Outputs will be re-ordered by this argument
#'

define_output(
  plotOutput('plot_power_per_electrode_within_groups'),
  title = 'Power Over Time per Electrode for Each Condition Group',
  width = 12,
  order = 1
)

define_output(
  plotOutput('plot_power_between_groups'),
  title = 'Mean Power over time per condition group',
  width = 12,
  order = 2
)

define_output(
  plotOutput('plot_cross_cor'),
  title = 'Cross correlation',
  width = 12,
  order = 4
)

define_output(
  plotOutput('plot_cross_cor_between_groups'),
  title = 'Cross correlation Between Conditions',
  width = 12,
  order = 5
)

define_output(
  plotOutput('plot_sliding_window'),
  # See time_i calculation in output_functions.
  title = 'Sliding Window Span (Representation of Width)',
  width = 12,
  order = 6
)

define_output(
  plotOutput('plot_window_cor'),
  title = 'Windowed Correlation',
  width = 12,
  order = 7
)

# Sliding Window Correlation
define_output(
  definition = verbatimTextOutput('print_window_cor_maxima'),
  title = 'Sliding Window Correlation: Center of Window Time at Maximal Correlation, per Comparison Electrode',
  width = 12,
  order = 8
)

define_output(
  definition = customizedUI('print_window_cor'),
  title = 'Windowed Correlation Data Table',
  width = 12,
  order = 9
)

# Time Lag Correlation & Aligned Data
define_output(
  plotOutput('plot_analysis_window'),
  title = 'Analysis Window for Time Lag',
  width = 12,
  order = 10
)

define_output(
  plotOutput('plot_time_lag_cor'),
  title = 'Time Lag Correlation',
  width = 12,
  order = 11
)

define_output(
  plotOutput('aligned_plot_per_electrode_within_groups'),
  title = 'Power Over Time per Electrode for Each Condition Group, Time-Lagged',
  width = 12,
  order = 12
)

# Lower priority: can zip separate outputs
define_output(
  definition = verbatimTextOutput('print_alignment_times'),
  title = 'Amount of Lag in Time, per Comparison Electrode',
  width = 12,
  order = 13
)

define_output(
  definition = customizedUI('print_aligned_data'),
  title = 'Aligned Data',
  width = 12,
  order = 14
)

# Navbar-style Layout
# Note: Panels need to be in same order as above
output_layout = list(
  # Time Lag Correlation Tabset
  "Time Lag Correlation" = list(
    # Panels in time lag tabset
    "Original Data" = c('..plot_time_lag_cor'),
    "Heatmaps" = c('..aligned_plot_per_electrode_within_groups'),
    "Aligned Curve" = c('..aligned_plot_per_electrode_within_groups'),
    'Data Table / Download' = c('..print_alignment_times',
                          'print_aligned_data')
  ),
  # Windowed Correlation Tabset
  "Windowed Correlation" = list(
    # Panels in windowed tabset
    "Overview" = c('..plot_analysis_window',
                   '..plot_time_lag_cor'),
    "Aligned Curve" = c('..aligned_plot_per_electrode_within_groups'),
    'Numeric Results' = c('..print_alignment_times',
                          'print_aligned_data')
  )
)


# <<<<<<<<<<<< End ----------------- [DO NOT EDIT THIS LINE] -------------------

# -------------------------------- View layout ---------------------------------

# Preview
require(shiny)
require(rave)
require(ravecorrelation)

env = dev_ravecorrelation(TRUE)
mount_demo_subject()
view_layout('correlation_module')

# main =  -----------------------------------------------------------------


