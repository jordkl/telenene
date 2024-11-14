# debug.R - Debug and performance monitoring functions

setup_debug_handlers <- function(input, output, session, rv) {
    # Debug log
    output$debug_info <- renderText({
        paste(rv$debug_log, collapse = "\n")
    })

    # Data structure
    output$data_structure <- renderPrint({
        req(rv$data)
        str(rv$data)
    })

    # Performance metrics
    output$performance_metrics <- renderPrint({
        req(rv$performance_metrics)
        cat("Processing Time:", round(rv$performance_metrics$processing_time, 2), "seconds\n")
        cat("Memory Used:", format(rv$performance_metrics$memory_used, units = "auto"), "\n")
        cat("Records Processed:", rv$performance_metrics$records_processed, "\n")
    })

    # Data table
    output$data_table <- renderDT({
        req(rv$filtered_data())
        datatable(
            rv$filtered_data() %>%
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
}