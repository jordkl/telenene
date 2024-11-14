# layers.R - Map layer functions

add_map_layers <- function(map, filtered_data, interval_data, input) {
    # Create color palette
    pal <- colorFactor(palette = "Set3", domain = interval_data$serial)
    
    # Add heatmap if enabled
    if (!is.null(input$show_heatmap) && input$show_heatmap) {
        map <- add_heatmap_layer(map, filtered_data)
    } else {
        map <- add_point_layer(map, interval_data, pal)
    }
    
    # Add tracks if enabled
    if (!is.null(input$show_tracks) && input$show_tracks) {
        map <- add_track_layers(map, interval_data, pal)
    }
    
    map
}

add_heatmap_layer <- function(map, data) {
    map %>%
        addHeatmap(
            data = data,
            lng = ~lon,
            lat = ~lat,
            radius = 15,
            blur = 20
        )
}

add_point_layer <- function(map, data, pal) {
    map %>%
        addCircleMarkers(
            data = data,
            lng = ~lon,
            lat = ~lat,
            color = ~pal(serial),
            popup = ~create_point_popup(.)
        )
}

add_track_layers <- function(map, data, pal) {
    selected_serials <- unique(data$serial)
    
    for (serial_id in selected_serials) {
        track_data <- data %>%
            filter(serial == serial_id) %>%
            arrange(datetime)
        
        if(nrow(track_data) > 1) {
            map <- map %>%
                addPolylines(
                    data = track_data,
                    lng = ~lon,
                    lat = ~lat,
                    color = pal(serial_id),
                    weight = 2,
                    opacity = 0.8,
                    popup = ~create_track_popup(.)
                )
        }
    }
    map
}