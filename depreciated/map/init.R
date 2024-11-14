# server/map/init.R

setup_map_handlers <- function(input, output, session, rv) {
    # Define Hawaiʻi bounds as a local constant
    hawaii_bounds <- list(
        lat_min = 18.5,  # Southern boundary
        lat_max = 22.5,  # Northern boundary
        lon_min = -160.5,  # Western boundary
        lon_max = -154.5   # Eastern boundary
    )

    # Helper function to calculate distance between two points (Haversine formula)
    haversine <- function(lat1, lon1, lat2, lon2) {
        R <- 6371  # Earth radius in kilometers
        delta_lat <- (lat2 - lat1) * pi / 180
        delta_lon <- (lon2 - lon1) * pi / 180
        a <- sin(delta_lat / 2)^2 +
            cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(delta_lon / 2)^2
        c <- 2 * atan2(sqrt(a), sqrt(1 - a))
        R * c
    }

    # Render the map
    output$map <- renderLeaflet({
        req(rv$data)
        
        # Get filtered data directly from the reactive
        filtered_data <- rv$filtered_data()

        # Filter to Hawaii bounds
        filtered_data <- filtered_data %>%
            filter(
                !is.na(lat), !is.na(lon),
                lat >= hawaii_bounds$lat_min,
                lat <= hawaii_bounds$lat_max,
                lon >= hawaii_bounds$lon_min,
                lon <= hawaii_bounds$lon_max
            )

        # Apply date range filtering
        if (!is.null(input$date_range) && length(input$date_range) == 2) {
            filtered_data <- filtered_data %>%
                filter(as.Date(datetime) >= as.Date(input$date_range[1]) &
                       as.Date(datetime) <= as.Date(input$date_range[2]))
        }
        
        if (nrow(filtered_data) == 0) {
            showNotification("No valid data points found after filtering.", type = "warning")
            return(leaflet())
        }

        # Add season information
        filtered_data <- filtered_data %>%
            mutate(season = case_when(
                month(datetime) %in% c(10, 11, 12, 1) ~ "Nesting",
                month(datetime) %in% c(2, 3, 4) ~ "Molting",
                month(datetime) %in% c(5, 6, 7, 8, 9) ~ "Flocking",
                TRUE ~ NA_character_
            ))

        # Filter by behavioral season
        if (!is.null(input$season_filter) && input$season_filter != "All") {
            filtered_data <- filtered_data %>%
                filter(season == input$season_filter)
        }

        if (nrow(filtered_data) == 0) {
            showNotification("No data available for the selected season.", type = "warning")
            return(leaflet())
        }

        # Get interval data
        interval <- if (!is.null(input$average_interval)) input$average_interval else "Daily"
        interval_data <- summarize_by_interval(filtered_data, interval)

        if (nrow(interval_data) == 0) {
            showNotification("No data available for the selected interval.", type = "warning")
            return(leaflet())
        }

        log_debug(paste("Interval data points:", nrow(interval_data)))

        # Initialize base map
        map <- leaflet() %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
            addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
            addLayersControl(
                baseGroups = c("Satellite", "OpenStreetMap"),
                options = layersControlOptions(collapsed = FALSE)
            )

        # Add heatmap if enabled
        if (!is.null(input$show_heatmap) && input$show_heatmap) {
            map <- map %>%
                addHeatmap(
                    data = filtered_data,
                    lng = ~lon,
                    lat = ~lat,
                    radius = 15,
                    blur = 20
                )
        } else {
            # Render summarized points
            pal <- colorFactor(palette = "Set3", domain = interval_data$serial)
            map <- map %>%
                addCircleMarkers(
                    data = interval_data,
                    lng = ~lon,
                    lat = ~lat,
                    color = ~pal(serial),
                    popup = ~paste(
                        "<b>Time:</b>", format(datetime, "%Y-%m-%d %H:%M HST"), "<br>",
                        "<b>Season:</b>", season, "<br>",
                        "<b>Points:</b>", count
                    )
                )
        }

        # Render movement tracks if enabled
        if (!is.null(input$show_tracks) && input$show_tracks && nrow(interval_data) > 1) {
            # Log track rendering attempt
            log_debug("Rendering movement tracks...")
            
            # Color palette for time-based coloring
            if(input$color_tracks_by_time) {
                time_pal <- colorNumeric(
                    palette = "viridis",
                    domain = as.numeric(interval_data$datetime)
                )
            }

            # Process each serial separately to maintain track continuity
            for (serial_id in unique(interval_data$serial)) {
                log_debug(paste("Processing track for serial:", serial_id))
                
                track_data <- interval_data %>%
                    filter(serial == serial_id) %>%
                    arrange(datetime)
                
                log_debug(paste("Points for track:", nrow(track_data)))

                if(nrow(track_data) > 1) {
                    map <- map %>%
                        addPolylines(
                            data = track_data,
                            lng = ~lon,
                            lat = ~lat,
                            color = if(input$color_tracks_by_time) {
                                ~time_pal(as.numeric(datetime))
                            } else {
                                pal(serial_id)
                            },
                            weight = 2,
                            opacity = 0.8
                        )
                    log_debug(paste("Added track for serial:", serial_id))
                }
            }

            # Add time-based legend if needed
            if(input$color_tracks_by_time) {
                map <- map %>%
                    addLegend(
                        position = "bottomright",
                        pal = time_pal,
                        values = as.numeric(interval_data$datetime),
                        title = "Time"
                    )
            }
        }

        map
    })
}