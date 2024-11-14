# map.R - Map rendering functions

setup_map_handlers <- function(input, output, session, rv) {
    # Define Hawaiʻi bounds as a local constant
    hawaii_bounds <- list(
        lat_min = 18.5,
        lat_max = 22.5,
        lon_min = -160.5,
        lon_max = -154.5
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

    # Function to summarize data by interval (Daily, Weekly, Monthly)
    summarize_by_interval <- function(data, interval) {
        if (nrow(data) == 0) return(data.frame())

        data <- data %>%
            mutate(date = as.Date(datetime),
                   week = format(date, "%Y-%U"),
                   month = format(date, "%Y-%m"))

        summarized <- switch(interval,
            "Daily" = data %>%
                group_by(serial, date) %>%
                summarize(
                    lat = stats::median(lat, na.rm = TRUE),
                    lon = stats::median(lon, na.rm = TRUE),
                    datetime = min(datetime, na.rm = TRUE),
                    count = n(),
                    season = first(season),
                    .groups = "drop"
                ),
            "Weekly" = data %>%
                group_by(serial, week) %>%
                summarize(
                    lat = stats::median(lat, na.rm = TRUE),
                    lon = stats::median(lon, na.rm = TRUE),
                    datetime = min(datetime, na.rm = TRUE),
                    count = n(),
                    season = first(season),
                    .groups = "drop"
                ),
            "Monthly" = data %>%
                group_by(serial, month) %>%
                summarize(
                    lat = stats::median(lat, na.rm = TRUE),
                    lon = stats::median(lon, na.rm = TRUE),
                    datetime = min(datetime, na.rm = TRUE),
                    count = n(),
                    season = first(season),
                    .groups = "drop"
                ),
            data  # Return raw data if no match
        )

        summarized
    }

    # Render the map
    output$map <- renderLeaflet({
        req(rv$data)

        # Pre-filter data
        filtered_data <- rv$data %>%
            filter(
                !is.na(lat), !is.na(lon),
                lat >= hawaii_bounds$lat_min, lat <= hawaii_bounds$lat_max,
                lon >= hawaii_bounds$lon_min, lon <= hawaii_bounds$lon_max
            )

        if (!is.null(input$date_range) && length(input$date_range) == 2) {
            filtered_data <- filtered_data %>%
                filter(datetime >= as.Date(input$date_range[1]) &
                       datetime <= as.Date(input$date_range[2]))
        }

        filtered_data <- filtered_data %>%
            mutate(season = case_when(
                month(datetime) %in% c(10, 11, 12, 1) ~ "Nesting",
                month(datetime) %in% c(2, 3, 4) ~ "Molting",
                month(datetime) %in% c(5, 6, 7, 8, 9) ~ "Flocking",
                TRUE ~ "Unknown"
            ))

        if (!is.null(input$season_filter) && input$season_filter != "All") {
            filtered_data <- filtered_data %>%
                filter(season == input$season_filter)
        }

        if (nrow(filtered_data) == 0) {
            showNotification("No valid data points found after filtering.", type = "warning")
            return(leaflet())
        }

        # Summarize by selected interval
        average_interval <- if (!is.null(input$average_interval)) input$average_interval else "Daily"
        interval_data <- summarize_by_interval(filtered_data, average_interval)

        if (nrow(interval_data) == 0) {
            showNotification("No data available for the selected interval.", type = "warning")
            return(leaflet())
        }

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

        # Add movement tracks if enabled
        if (!is.null(input$show_tracks) && input$show_tracks && nrow(interval_data) > 1) {
            map <- map %>%
                addPolylines(
                    data = interval_data,
                    lng = ~lon,
                    lat = ~lat,
                    color = ~pal(serial),
                    weight = 2,
                    opacity = 0.8
                )
        }

        map
    })
}
