# data.R - Data processing and filtering functions

setup_data_handlers <- function(input, output, session, rv) {
    # Logging function
    log_debug <- function(message) {
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        rv$debug_log <- c(rv$debug_log, paste(timestamp, "-", message))
    }

    # Reactive filtered data
    rv$filtered_data <- reactive({
        req(rv$data)
        df <- rv$data %>%
            filter(datetime >= input$date_range[1] & 
                   datetime <= input$date_range[2] &
                   hdop <= input$hdop_filter)

        if (!is.null(input$serial_filter) && length(input$serial_filter) > 0) {
            df <- df %>% filter(serial %in% input$serial_filter)
        }
        df
    })

    # File upload handling
    observeEvent(input$file, {
        req(input$file)
        log_debug(paste("File uploaded:", input$file$name))

        start_time <- Sys.time()

        # Read and process uploaded CSV
        data <- try(read_csv(input$file$datapath, show_col_types = FALSE), silent = TRUE)
        if (inherits(data, "try-error")) {
            log_debug("Error reading file")
            showNotification("Error reading file", type = "error")
            return()
        }

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
            filter(!is.na(lat), !is.na(lon), !is.na(datetime), hdop < 5)

        rv$data <- data

        # Update date range and filters
        updateDateRangeInput(session, "date_range",
                             start = min(data$datetime),
                             end = max(data$datetime))
        updateSelectInput(session, "serial_filter", choices = unique(data$serial))
        updateSelectInput(session, "track_serial", choices = unique(data$serial))
        updateSelectInput(session, "gap_serial", choices = unique(data$serial))

        # Performance metrics
        end_time <- Sys.time()
        rv$performance_metrics$processing_time <- as.numeric(end_time - start_time, units = "secs")
        rv$performance_metrics$records_processed <- nrow(data)

        log_debug(sprintf("Processed %d records in %.2f seconds", 
                          nrow(data), rv$performance_metrics$processing_time))
    })

    # Download filtered data
    output$download_data <- downloadHandler(
        filename = function() {
            paste("filtered_nene_data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            req(rv$filtered_data())
            write.csv(rv$filtered_data(), file, row.names = FALSE)
        }
    )
}
