# sidebar.R - Sidebar panel UI elements

sidebarPanel_ui <- sidebarPanel(
    fileInput("file", "Upload CTT GPS Data (CSV)",
              accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
    
    # Data filtering options
    numericInput("hdop_filter", "HDOP Filter (<2.5 is good):",
                 value = 2.5, min = 0, max = 10, step = 0.1),
    checkboxInput("downsample", "Downsample large datasets", TRUE),
    numericInput("sample_size", "Maximum points to display:", 
                 value = 5000, min = 1000, max = 50000),
    
    # Time and behavior filtering
    dateRangeInput("date_range", "Date Range:", 
                   start = Sys.Date() - 30, end = Sys.Date()),
    selectInput("season_filter", "Behavioral Season:",
                choices = c("All", "Nesting", "Molting", "Flocking"), 
                selected = "All"),
    
    hr(),
    
    # Map visualization controls
    checkboxInput("show_averaged_points", "Show Averaged Points (Daily/Weekly/Monthly)", FALSE),
    conditionalPanel(
        condition = "input.show_averaged_points == true",
        radioButtons("average_interval", "Averaging Interval:",
                     choices = c("Daily", "Weekly", "Monthly"),
                     selected = "Daily")
    ),
    
    checkboxInput("show_heatmap", "Show Heatmap", FALSE),
    checkboxInput("show_tracks", "Show Movement Tracks", FALSE),
    
    hr(),
    
    # Export
    downloadButton("download_data", "Download Filtered Data"),
    
    width = 3
)
