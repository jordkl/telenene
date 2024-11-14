# server/common.R
summarize_by_interval <- function(data, interval) {
    if (nrow(data) == 0) return(data.frame())
    
    switch(interval,
        "Daily" = summarize_daily(data),
        "Weekly" = summarize_weekly(data),
        "Monthly" = summarize_monthly(data),
        {
            showNotification("Invalid averaging interval. Defaulting to raw data.", type = "warning")
            data
        }
    )
}

# Individual summarization functions
summarize_daily <- function(data) {
    data %>%
        mutate(date = as.Date(datetime)) %>%
        group_by(serial, date) %>%
        summarise(
            lat = stats::median(lat, na.rm = TRUE),
            lon = stats::median(lon, na.rm = TRUE),
            datetime = min(datetime, na.rm = TRUE),
            season = first(season),
            count = n(),
            .groups = "drop"
        )
}

summarize_weekly <- function(data) {
    data %>%
        mutate(week = as.integer(format(as.Date(datetime), "%U"))) %>%
        group_by(serial, week) %>%
        summarise(
            lat = stats::median(lat, na.rm = TRUE),
            lon = stats::median(lon, na.rm = TRUE),
            datetime = min(datetime, na.rm = TRUE),
            season = first(season),
            count = n(),
            .groups = "drop"
        )
}

summarize_monthly <- function(data) {
    data %>%
        mutate(month = format(as.Date(datetime), "%Y-%m")) %>%
        group_by(serial, month) %>%
        summarise(
            lat = stats::median(lat, na.rm = TRUE),
            lon = stats::median(lon, na.rm = TRUE),
            datetime = min(datetime, na.rm = TRUE),
            season = first(season),
            count = n(),
            .groups = "drop"
        )
}