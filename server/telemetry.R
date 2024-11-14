# telemetry.R - Telemetry health statistics and gap analysis

setup_telemetry_handlers <- function(input, output, session, rv) {
    # Telemetry health table
    output$telemetry_health <- renderDT({
        req(rv$filtered_data())

        health_stats <- rv$filtered_data() %>%
            group_by(serial) %>%
            arrange(datetime) %>%
            summarize(
                Mean_Voltage = round(mean(data_voltage, na.rm = TRUE), 2),
                Min_Voltage = round(min(data_voltage, na.rm = TRUE), 2),
                Mean_Solar_Charge = round(mean(solar_charge, na.rm = TRUE), 2),
                Mean_Solar_Current = round(mean(solar_current, na.rm = TRUE), 2),
                Mean_HDOP = round(mean(hdop, na.rm = TRUE), 2),
                Points_Last_24h = sum(datetime >= max(datetime) - days(1)),
                Total_Points = n(),
                Last_Transmission = format(max(datetime), "%Y-%m-%d %H:%M HST"),
                Max_Gap_Days = round(max(diff(as.Date(datetime))), 1),
                Recent_Gap_Days = round(as.numeric(difftime(Sys.time(), max(datetime), units = "days")), 1),
                Gaps_Over_24h = sum(diff(datetime) > dhours(24))
            )

        datatable(health_stats,
                  options = list(
                      pageLength = 5,
                      scrollX = TRUE
                  ),
                  rownames = FALSE)
    })

    # Gap analysis plot
    output$gap_plot <- renderPlot({
        req(rv$filtered_data(), input$gap_serial)

        gap_data <- rv$filtered_data() %>%
            filter(serial == input$gap_serial) %>%
            arrange(datetime) %>%
            mutate(date = as.Date(datetime)) %>%
            group_by(date) %>%
            summarize(
                points = n(),
                mean_voltage = mean(data_voltage, na.rm = TRUE),
                mean_solar = mean(solar_charge, na.rm = TRUE)
            )

        ggplot(gap_data) +
            geom_bar(aes(x = date, y = points), stat = "identity", fill = "steelblue", alpha = 0.7) +
            geom_line(aes(x = date, y = mean_voltage * 10, color = "Voltage"), size = 1) +
            geom_line(aes(x = date, y = mean_solar * 10, color = "Solar"), size = 1) +
            scale_y_continuous(
                "Daily Transmissions",
                sec.axis = sec_axis(~./10, name = "Voltage/Solar")
            ) +
            scale_color_manual(values = c("Voltage" = "red", "Solar" = "orange")) +
            theme_minimal() +
            labs(title = paste("Transmission Pattern -", input$gap_serial),
                 x = "Date",
                 color = "Metrics") +
            theme(legend.position = "bottom")
    })

    # Gap analysis table
    output$gap_table <- renderDT({
        req(rv$filtered_data(), input$gap_serial)

        gap_data <- rv$filtered_data() %>%
            filter(serial == input$gap_serial) %>%
            arrange(datetime) %>%
            mutate(
                next_transmission = lead(datetime),
                gap_hours = as.numeric(difftime(next_transmission, datetime, units = "hours"))
            ) %>%
            filter(gap_hours > 24) %>%
            select(
                gap_start = datetime,
                gap_end = next_transmission,
                gap_duration = gap_hours,
                voltage = data_voltage,
                solar = solar_charge
            ) %>%
            mutate(
                gap_start = format(gap_start, "%Y-%m-%d %H:%M HST"),
                gap_end = format(gap_end, "%Y-%m-%d %H:%M HST"),
                gap_duration = round(gap_duration / 24, 1)
            )

        datatable(gap_data,
                  options = list(
                      pageLength = 10,
                      scrollX = TRUE,
                      order = list(list(2, 'desc'))
                  ),
                  rownames = FALSE) %>%
            formatStyle(
                'gap_duration',
                backgroundColor = styleInterval(
                    c(2, 5),
                    c('none', 'yellow', 'pink')
                )
            )
    })
}