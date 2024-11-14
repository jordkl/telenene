# server.R - Main server function

server <- function(input, output, session) {
    # Reactive values
    rv <- reactiveValues(
        data = NULL,
        filtered_data = NULL,
        debug_log = character(),
        performance_metrics = list()
    )

    # Source and initialize server-side modules
    source("server/data.R", local = TRUE)
    setup_data_handlers(input, output, session, rv)

    source("server/map.R", local = TRUE)
    setup_map_handlers(input, output, session, rv)

    source("server/telemetry.R", local = TRUE)
    setup_telemetry_handlers(input, output, session, rv)

    # Source debug handlers
    source("server/debug.R", local = TRUE)
    setup_debug_handlers(input, output, session, rv)

    # Source telemetry handlers
    source("server/telemetry.R", local = TRUE)
    setup_telemetry_handlers(input, output, session, rv)
}