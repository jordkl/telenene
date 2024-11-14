# mainpanel.R - Main panel UI elements

mainPanel_ui <- mainPanel(
    tabsetPanel(
        tabPanel("Map", 
                 leafletOutput("map", height = "700px"),
                 htmlOutput("stats_summary"),
                 h4("Telemetry Health"),
                 DTOutput("telemetry_health")),
        
        tabPanel("Transmission Analysis",
                 selectInput("gap_serial", "Select Transmitter:", choices = NULL),
                 plotOutput("gap_plot", height = "300px"),
                 h4("Transmission Gaps"),
                 DTOutput("gap_table")),
        
        tabPanel("Data Explorer", 
                 DTOutput("data_table")),
        
        tabPanel("Debug Info",
                 verbatimTextOutput("debug_info"),
                 h4("Data Structure"),
                 verbatimTextOutput("data_structure"),
                 h4("Performance Metrics"),
                 verbatimTextOutput("performance_metrics")),
        
        tabPanel("About", 
                 includeMarkdown("about.md"))
    ),
    width = 9
)
