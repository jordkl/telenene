# popups.R - Popup content formatting

create_point_popup <- function(point) {
    paste(
        "<b>Time:</b>", format(point$datetime, "%Y-%m-%d %H:%M HST"), "<br>",
        "<b>Season:</b>", point$season, "<br>",
        "<b>Points:</b>", point$count
    )
}

create_track_popup <- function(track) {
    paste(
        "Serial:", track$serial, "<br>",
        "Season:", track$season, "<br>",
        "Date:", format(track$datetime, "%Y-%m-%d")
    )
}