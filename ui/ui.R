# ui.R - Main UI definition
# Source all UI components
source("ui/sidebar.R")
source("ui/mainpanel.R")

ui <- fluidPage(
    CSS_STYLES,
    
    div(class = "title-banner",
        div(style = "display: flex; justify-content: space-between; align-items: center;",
            div(
                h1("NeneMap", tags$small(" - Nēnē GPS Tracking Visualization")),
                p("Track movements of the endangered Hawaiian Goose (Branta sandvicensis)")
            ),
            div(class = "version", paste("v", APP_VERSION))
        )
    ),
    
    sidebarLayout(
        sidebarPanel_ui,  # Defined in sidebar.R
        mainPanel_ui      # Defined in mainpanel.R
    )
)