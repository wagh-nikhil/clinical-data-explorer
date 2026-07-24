# app_ui.R
# Main user interface using bslib page_navbar for modern responsive layouts.

library(bslib)

app_ui <- page_navbar(
  title = "ClinDataExplorer 🔬",
  id = "main_nav",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#0066cc",      # Professional clinical blue
    secondary = "#6c757d",
    success = "#198754",
    warning = "#ffc107",
    danger = "#dc3545",
    base_font = font_google("Outfit"),
    heading_font = font_google("Outfit")
  ),
  
  # Custom CSS for clinical layout styling
  header = tags$head(
    tags$style(HTML("
      /* Professional tweaks to value_boxes and headers */
      .card-header {
        font-weight: 600;
        background-color: #f8f9fa;
      }
      .navbar-brand {
        font-weight: 700 !important;
        letter-spacing: 0.5px;
      }
      /* Uniform styling for badges */
      .badge {
        font-size: 0.85rem;
      }
    "))
  ),
  
  # Configuration Page
  nav_panel(
    title = "Configuration & Registry",
    icon = icon("cogs"),
    mod_config_ui("config_tab")
  ),
  
  # Global Data Explorer Page
  nav_panel(
    title = "Global Data Explorer",
    icon = icon("database"),
    mod_data_explorer_ui("explorer_tab")
  ),
  
  # Patient Profile Explorer Page
  nav_panel(
    title = "Patient Profile Explorer",
    icon = icon("user-injured"),
    mod_patient_profile_ui("patient_tab")
  )
)
