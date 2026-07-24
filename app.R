# app.R
# Main entrypoint to launch the ClinDataExplorer Shiny application.

source("global.R")
source("app_ui.R")
source("app_server.R")

shinyApp(ui = app_ui, server = app_server)
