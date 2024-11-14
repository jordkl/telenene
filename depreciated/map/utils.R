# utils.R - Helper functions

# Hawaii bounds
HAWAII_BOUNDS <- list(
    lat_min = 18.5,  # Southern boundary
    lat_max = 22.5,  # Northern boundary
    lon_min = -160.5,  # Western boundary
    lon_max = -154.5   # Eastern boundary
)

# Haversine distance calculation
haversine <- function(lat1, lon1, lat2, lon2) {
    R <- 6371  # Earth radius in kilometers
    delta_lat <- (lat2 - lat1) * pi / 180
    delta_lon <- (lon2 - lon1) * pi / 180
    a <- sin(delta_lat / 2)^2 +
        cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(delta_lon / 2)^2
    c <- 2 * atan2(sqrt(a), sqrt(1 - a))
    R * c
}

# Create base map
create_base_map <- function() {
    leaflet() %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
        addLayersControl(
            baseGroups = c("Satellite", "OpenStreetMap"),
            options = layersControlOptions(collapsed = FALSE)
        )
}