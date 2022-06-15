
# This Outputs.R is located in inst/modules/correlation_module
# These are the files that are not seen in the ravecorrelation project
# Not to be confused with R/output_functions.R

# Time Lagged Data Outputs
# Aligned data
print_aligned_data <- function(){
  DT::dataTableOutput(ns("do_print_aligned_data"))
}


# Trying to download a zip...
# downloadHandler(
#   filename <- function() {
#     paste("output", "tar", sep=".")
#   },
#   
#   content <- function(file) {
#     tar(file, "file/path/")
#   }
# ) 


# Custom Shiny in Rave
output$do_print_aligned_data <- DT::renderDataTable({
  
  # Retrieve from local_data
  aligned_data <- local_data$aligned_data
  # Temporary solution to DT issues. Combine all groups into one matrix for DT outputs
  aligned_data <- cbind.data.frame(
    condition = rep(names(aligned_data), each = length(aligned_data[[1]][[1]]$x)),
    time = rep(aligned_data[[1]][[1]]$x, times = length(aligned_data)),
    do.call(rbind, lapply(aligned_data, function(a_d) {
      do.call(cbind, lapply(a_d, function(ad) ad$data[,1]))
    }))
  )
  
  # Display options
  DT::datatable(aligned_data,
                extensions = c('FixedColumns', 'FixedHeader', 'Scroller', 'Buttons'),
                options = list(
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  # Horizontal and vertical scrolling
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = '200px',
                  # Freeze rownames, which are time, the left-most "column"
                  fixedColumns = list(leftColumns = 1),
                  # Set column widths
#                  autoWidth = TRUE,
                  columnDefs = list(list(width = '50px', targets = "_all")),
                  # Remove sorting
                  ordering = FALSE,
                  # Remove filtering
                  searching = FALSE,
                  # B for button, t for table, r for processing
                  dom = 'Btr'
                  ))
})

# Window Correlation Data Output
print_window_cor <- function(){
  DT::dataTableOutput(ns("do_print_window_cor"))
}

# Custom Shiny in Rave
output$do_print_window_cor <- DT::renderDataTable({
  
  # Retrieve from local_data
  window_cor <- local_data$window_cor
  # Temporary solution to DT issues. Combine all groups into one matrix for DT outputs
  window_cor <- cbind.data.frame(
    condition = rep(names(window_cor), each = length(window_cor[[1]]$x)),
    do.call(rbind, lapply(window_cor, function(wind) {
      cbind(time = wind$x, wind$data)
    })))
  
  # Display options
  DT::datatable(window_cor,
                extensions = c('FixedColumns', 'FixedHeader', 'Scroller', 'Buttons'),
                options = list(
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                  # Horizontal and vertical scrolling
                  scroller = TRUE,
                  scrollX = TRUE,
                  scrollY = '200px',
                  # Freeze rownames, which are time, the left-most "column"
                  fixedColumns = list(leftColumns = 1),
                  # Set column widths
#                  autoWidth = TRUE,
                  columnDefs = list(list(width = '50px', targets = "_all")),
                  # Remove sorting
                  ordering = FALSE,
                  # Remove filtering
                  searching = FALSE,
                  # B for button, t for table, r for processing
                  dom = 'Btr'
                ))
})
