# Load required libraries
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
library(geosphere)

# Application Version
APP_VERSION <- "1.2.4"

# Important locations data
IMPORTANT_LOCATIONS <- data.frame(
    name = c("Hakalau Forest NWR", "Kahuku Unit", "Kīpuka 'Ainahou Nēnē Sanctuary"),
    lat = c(19.8333, 19.2478, 19.3714),
    lon = c(-155.3333, -155.6861, -155.3714)
)

# Global functions
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

# CSS Styles
CSS_STYLES <- tags$style(
    ".title-banner {
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
    }"
)