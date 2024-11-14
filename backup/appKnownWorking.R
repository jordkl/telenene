# app.R - Part 1
library(shiny)
library(leaflet)
library(dplyr)
library(DT)
library(sf)
library(readr)
library(tidyr)
library(lubridate)
library(leaflet.extras)
library(pryr)
library(ggplot2)

# Application Version
APP_VERSION <- "1.0.4"

# UI Definition
ui <- fluidPage(
    tags$head(
        tags$style(HTML("
            .title-banner {
                background-color: #2c3e50;
                color: white;
                padding: 15px;
                margin-bottom: 20px;
            }
            .title-banner h1 {
                margin: 0;
                font-size: 24px;
            }
            .title-banner p {
                margin: 5px 0 0 0;
                font-size: 14px;
                opacity: 0.8;
            }
            .version {
                font-size: 12px;
                color: #ccc;
                text-align: right;
            }
            .debug-info {
                font-family: monospace;
                background-color: #f8f9fa;
                padding: 10px;
                border-radius: 5px;
                white-space: pre-wrap;
            }
            .stats-panel {
                margin-top: 15px;
                padding: 10px;
                background-color: #f8f9fa;
                border-radius: 5px;
            }
            .warning {
                color: #dc3545;
                font-weight: bold;
            }
        "))
    ),
    
    div(class = "title-banner",
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            div(
                h1("NeneMap", tags$small(" - Nēnē GPS Tracking Visualization")),
                p("Track movements of the endangered Hawaiian Goose (Branta sandvicensis)")
            ),
            div(class = "version", paste("v", APP_VERSION))
        )
    ),
    
    sidebarLayout(
        sidebarPanel(
            fileInput("file", "Upload CTT GPS Data (CSV)",
                     accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
            
            # Data filtering options
            numericInput("hdop_filter", "HDOP Filter (<2.5 is good):",
                        value = 2.5, min = 0, max = 10, step = 0.1),
            checkboxInput("downsample", "Downsample large datasets", TRUE),
            numericInput("sample_size", "Maximum points to display:", 
                        value = 5000, min = 1000, max = 50000),
            
            # Time and transmitter filtering
            dateRangeInput("date_range", "Date Range:", 
                          start = Sys.Date() - 30, end = Sys.Date()),
            selectInput("serial_filter", "Select Transmitter:",
                       choices = NULL, multiple = TRUE),
            
            hr(),
            
            # Map visualization controls
            checkboxInput("show_heatmap", "Show Heatmap", FALSE),
            checkboxInput("show_tracks", "Show Movement Tracks", FALSE),
            conditionalPanel(
                condition = "input.show_tracks == true",
                checkboxInput("color_tracks_by_time", "Color tracks by time", FALSE),
                selectInput("track_serial", "Select Transmitter for Tracks:",
                           choices = NULL, multiple = TRUE)
            ),
            
            radioButtons("map_type", "Base Map:",
                        choices = c("Satellite" = "satellite", 
                                  "OpenStreetMap" = "osm"),
                        selected = "satellite"),
            checkboxInput("show_locations", "Show Important Locations", TRUE),
            
            hr(),
            
            # Export
            downloadButton("download_data", "Download Filtered Data"),
            
            width = 3
        ),
        
        mainPanel(
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
                        p("NeneMap is a tool for tracking and visualizing Nēnē GPS telemetry data."))
            ),
            width = 9
        )
    )
)

# Server Logic - Part 1
server <- function(input, output, session) {
    
    # Reactive values
    rv <- reactiveValues(
        data = NULL,
        filtered_data = NULL,
        debug_log = character(),
        performance_metrics = list()
    )
    
    # Logging function
    log_debug <- function(message) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        rv$debug_log <- c(rv$debug_log, paste(timestamp, "-", message))
    }
    
    # Function to identify transmission gaps
    identify_gaps <- function(data, threshold = hours(24)) {
        data %>%
            arrange(datetime) %>%
            mutate(
                gap_start = datetime,
                gap_end = lead(datetime),
                gap_duration = as.numeric(difftime(gap_end, gap_start, units = "hours")),
                has_gap = !is.na(gap_duration) & gap_duration > threshold
            )
    }
    
    # Reactive filtered data
    filtered_data <- reactive({
        req(rv$data)
        df <- rv$data %>%
            filter(datetime >= input$date_range[1] & 
                   datetime <= input$date_range[2] &
                   hdop <= input$hdop_filter)
        
        if (!is.null(input$serial_filter) && length(input$serial_filter) > 0) {
            df <- df %>% filter(serial %in% input$serial_filter)
        }
        
        return(df)
    })

        # File upload handling
    observeEvent(input$file, {
        req(input$file)
        log_debug(paste("File uploaded:", input$file$name))
        
        start_time <- Sys.time()
        
        # Read CSV
        data <- try(read_csv(input$file$datapath, show_col_types = FALSE), silent = TRUE)
        
        if (inherits(data, "try-error")) {
            log_debug("Error reading file")
            showNotification("Error reading file", type = "error")
            return()
        }
        
        log_debug(paste("Columns in uploaded file:", paste(names(data), collapse=", ")))
        
        # Process data
        data <- data %>%
            mutate(
                datetime = as.POSIXct(paste(`GPS_date_YYYY-MM-DD`, `GPS_utc_HH:MM:SS`), 
                                    format = "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                datetime = with_tz(datetime, tzone = "Pacific/Honolulu"),
                lat = as.numeric(lat),
                lon = as.numeric(lon),
                alt = as.numeric(alt),
                hdop = as.numeric(hdop)
            ) %>%
            filter(
                !is.na(lat), 
                !is.na(lon),
                !is.na(datetime),
                hdop < 5
            )
        
        rv$data <- data
        
        # Update date range to match data
        updateDateRangeInput(session, "date_range",
                           start = min(data$datetime),
                           end = max(data$datetime))
        
        # Update performance metrics
        end_time <- Sys.time()
        rv$performance_metrics$processing_time <- as.numeric(end_time - start_time, units="secs")
        rv$performance_metrics$memory_used <- mem_used()
        rv$performance_metrics$records_processed <- nrow(data)
        
        log_debug(sprintf("Processed %d records in %.2f seconds", 
                         nrow(data), 
                         rv$performance_metrics$processing_time))
        
        # Update filter options
        updateSelectInput(session, "serial_filter", choices = unique(data$serial))
        updateSelectInput(session, "track_serial", choices = unique(data$serial))
        updateSelectInput(session, "gap_serial", choices = unique(data$serial))
    })
    
    # Map rendering
    output$map <- renderLeaflet({
        req(rv$data)
        map <- leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
            addLayersControl(
                baseGroups = c("Satellite", "OpenStreetMap"),
                options = layersControlOptions(collapsed = FALSE)
            )
        
        if (input$show_locations) {
            important_locations <- data.frame(
                name = c("Hakalau Forest NWR", "Kahuku Unit", "Kīpuka 'Ainahou Nēnē Sanctuary"),
                lat = c(19.8333, 19.2478, 19.3714),
                lon = c(-155.3333, -155.6861, -155.3714)
            )
            
            map <- map %>%
                addCircleMarkers(
                    data = important_locations,
                    lng = ~lon,
                    lat = ~lat,
                    radius = 6,
                    color = "red",
                    popup = ~name
                )
        }
        
        if (!is.null(filtered_data())) {
            pal <- colorFactor(palette = "Set3", domain = filtered_data()$serial)
            
            # Create time-based color palette
            if(input$show_tracks && input$color_tracks_by_time) {
                time_range <- range(filtered_data()$datetime)
                time_pal <- colorNumeric(
                    palette = "viridis",
                    domain = as.numeric(filtered_data()$datetime)
                )
            }
            
            map <- map %>%
                addCircleMarkers(
                    data = filtered_data(),
                    lng = ~lon,
                    lat = ~lat,
                    color = ~pal(serial),
                    popup = ~paste(
                        "<b>Time:</b>", format(datetime, "%Y-%m-%d %H:%M HST"), "<br>",
                        "<b>Altitude:</b>", round(alt), "m<br>",
                        "<b>Serial:</b>", serial, "<br>",
                        "<b>HDOP:</b>", round(hdop, 2), "<br>",
                        "<b>Voltage:</b>", round(data_voltage, 2), "V<br>",
                        "<b>Solar:</b>", round(solar_charge, 2)
                    )
                )
            
            if (input$show_tracks && !is.null(input$track_serial)) {
                for (serial in input$track_serial) {
                    track_data <- filtered_data() %>% 
                        filter(serial == !!serial) %>%
                        arrange(datetime) %>%
                        identify_gaps()
                    
                    # Regular connections
                    map <- map %>%
                        addPolylines(
                            data = track_data %>% filter(!has_gap),
                            lng = ~lon,
                            lat = ~lat,
                            color = if(input$color_tracks_by_time) {
                                ~time_pal(as.numeric(datetime))
                            } else {
                                pal(serial)
                            },
                            weight = 2,
                            opacity = 0.8
                        )
                    
                    # Gap segments
                    gap_segments <- track_data %>% filter(has_gap)
                    if(nrow(gap_segments) > 0) {
                        map <- map %>%
                            addPolylines(
                                data = gap_segments,
                                lng = ~lon,
                                lat = ~lat,
                                color = "red",
                                weight = 1,
                                opacity = 0.5,
                                dashArray = "5,10",
                                popup = ~paste("Gap of", round(gap_duration/24, 1), "days")
                            )
                    }
                }
                
                # Add legend if coloring by time
                if(input$color_tracks_by_time) {
                    map <- map %>%
                        addLegend(
                            position = "bottomright",
                            pal = time_pal,
                            values = as.numeric(track_data$datetime),
                            title = "Time"
                        )
                }
            }
            
            if (input$show_heatmap) {
                map <- map %>%
                    addHeatmap(
                        data = filtered_data(),
                        lng = ~lon,
                        lat = ~lat,
                        radius = 15,
                        blur = 20
                    )
            }
        }
        
        map
    })
    
    # Telemetry health statistics
    output$telemetry_health <- renderDT({
        req(filtered_data())
        
        health_stats <- filtered_data() %>%
            group_by(serial) %>%
            arrange(datetime) %>%
            summarize(
                Mean_Voltage = round(mean(data_voltage, na.rm = TRUE), 2),
                Min_Voltage = round(min(data_voltage, na.rm = TRUE), 2),
                Mean_Solar_Charge = round(mean(solar_charge, na.rm = TRUE), 2),
                Mean_Solar_Current = round(mean(solar_current, na.rm = TRUE), 2),
                Mean_HDOP = round(mean(hdop, na.rm = TRUE), 2),
                Points_Last_24h = sum(datetime >= max(datetime) - days(1)),
                Total_Points = n(),
                Last_Transmission = format(max(datetime), "%Y-%m-%d %H:%M HST"),
                Max_Gap_Days = round(max(diff(as.Date(datetime))), 1),
                Recent_Gap_Days = round(as.numeric(difftime(Sys.time(), max(datetime), units="days")), 1),
                Gaps_Over_24h = sum(diff(datetime) > dhours(24))
            )
        
        datatable(health_stats,
                 options = list(
                     pageLength = 5,
                     scrollX = TRUE
                 ),
                 rownames = FALSE)
    })
    
    # Gap analysis plot
    output$gap_plot <- renderPlot({
        req(filtered_data(), input$gap_serial)
        
        gap_data <- filtered_data() %>%
            filter(serial == input$gap_serial) %>%
            arrange(datetime) %>%
            mutate(date = as.Date(datetime)) %>%
            group_by(date) %>%
            summarise(
                points = n(),
                mean_voltage = mean(data_voltage, na.rm = TRUE),
                mean_solar = mean(solar_charge, na.rm = TRUE)
            )
        
        ggplot(gap_data) +
            geom_bar(aes(x = date, y = points), stat = "identity", fill = "steelblue", alpha = 0.7) +
            geom_line(aes(x = date, y = mean_voltage * 10, color = "Voltage"), size = 1) +
            geom_line(aes(x = date, y = mean_solar * 10, color = "Solar"), size = 1) +
            scale_y_continuous(
                "Daily Transmissions",
                sec.axis = sec_axis(~./10, name = "Voltage/Solar")
            ) +
            scale_color_manual(values = c("Voltage" = "red", "Solar" = "orange")) +
            theme_minimal() +
            labs(title = paste("Transmission Pattern -", input$gap_serial),
                 x = "Date",
                 color = "Metrics") +
            theme(legend.position = "bottom")
    })
    
    # Gap analysis table
    output$gap_table <- renderDT({
        req(filtered_data(), input$gap_serial)
        
        gap_data <- filtered_data() %>%
            filter(serial == input$gap_serial) %>%
            arrange(datetime) %>%
            mutate(
                next_transmission = lead(datetime),
                gap_hours = as.numeric(difftime(next_transmission, datetime, units = "hours"))
            ) %>%
            filter(gap_hours > 24) %>%
            select(
                gap_start = datetime,
                gap_end = next_transmission,
                gap_duration = gap_hours,
                voltage = data_voltage,
                solar = solar_charge
            ) %>%
            mutate(
                gap_start = format(gap_start, "%Y-%m-%d %H:%M HST"),
                gap_end = format(gap_end, "%Y-%m-%d %H:%M HST"),
                gap_duration = round(gap_duration/24, 1)
            )
        
        datatable(gap_data,
                 options = list(
                     pageLength = 10,
                     scrollX = TRUE,
                     order = list(list(2, 'desc'))
                 ),
                 rownames = FALSE) %>%
            formatStyle(
                'gap_duration',
                backgroundColor = styleInterval(
                    c(2, 5),
                    c('none', 'yellow', 'pink')
                )
            )
    })
    
    # Debug info outputs
    output$debug_info <- renderText({
        paste(rv$debug_log, collapse = "\n")
    })
    
    output$data_structure <- renderPrint({
        req(rv$data)
        str(rv$data)
    })
    
    output$performance_metrics <- renderPrint({
        req(rv$performance_metrics)
        cat("Processing Time:", round(rv$performance_metrics$processing_time, 2), "seconds\n")
        cat("Memory Used:", format(rv$performance_metrics$memory_used, units = "auto"), "\n")
        cat("Records Processed:", rv$performance_metrics$records_processed, "\n")
    })
    
    # Data summary
    output$stats_summary <- renderUI({
        req(filtered_data())
        HTML(sprintf(
            "<div class='stats-panel'>
                <h4>Data Summary:</h4>
                <ul>
                    <li>Total Locations: %d</li>
                    <li>Unique Transmitters: %d</li>
                    <li>Time Range: %s to %s</li>
                    <li>Mean HDOP: %.2f</li>
                </ul>
            </div>",
            nrow(filtered_data()),
            length(unique(filtered_data()$serial)),
            format(min(filtered_data()$datetime), "%Y-%m-%d %H:%M HST"),
            format(max(filtered_data()$datetime), "%Y-%m-%d %H:%M HST"),
            mean(filtered_data()$hdop, na.rm = TRUE)
        ))
    })
    
    # Data table
    output$data_table <- renderDT({
        req(filtered_data())
        datatable(
            filtered_data() %>%
                select(serial, datetime, lat, lon, alt, hdop, 
                       data_voltage, solar_charge, solar_current) %>%
                arrange(desc(datetime)) %>%
                mutate(
                    datetime = format(datetime, "%Y-%m-%d %H:%M HST"),
                    lat = round(lat, 6),
                    lon = round(lon, 6),
                    alt = round(alt),
                    hdop = round(hdop, 2),
                    data_voltage = round(data_voltage, 2),
                    solar_charge = round(solar_charge, 2),
                    solar_current = round(solar_current, 2)
                ),
            options = list(
                pageLength = 25,
                scrollX = TRUE,
                order = list(list(1, 'desc'))
            ),
            caption = "GPS Locations"
        )
    })
    
    # Export filtered data
    output$download_data <- downloadHandler(
        filename = function() {
            paste("filtered_nene_data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(filtered_data(), file, row.names = FALSE)
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)