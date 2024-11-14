source("global.R")

# Source UI components
source("ui/ui.R")
source("ui/sidebar.R")
source("ui/mainpanel.R")

# Source server components
source("server/server.R")
source("server/data.R")
source("server/telemetry.R")
source("server/debug.R")
source("server/map.R")

# Run the application
shinyApp(ui = ui, server = server)