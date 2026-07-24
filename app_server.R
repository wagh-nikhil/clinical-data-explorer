# app_server.R
# Main application server coordinating reactive state and modular server instances.

app_server <- function(input, output, session) {
  
  # Centralized application state
  # This stores scanned datasets, in-memory caches, and derived filters to prevent 
  # redundant disk reads and cascading reactive calculations.
  state <- reactiveValues(
    metadata = NULL,            # data.frame listing all found datasets (updated dynamically on load)
    dataset_list = NULL,        # data.frame of dataset names and types (updated ONLY on ingestion)
    dataset_cols = list(),      # named list of column names for each scanned dataset
    dataset_selections = list(),# named list of selected column vectors for configured datasets
    dataset_status = list(),    # named list of statuses ("Unconfigured", "Configured", "Loaded", "Excluded")
    dataset_inclusion = list(), # named list of checkbox load inclusion statuses
    cohort_subset = NULL,       # character vector of USUBJIDs from Excel for global cohort subsetting
    loaded_data = list(),       # named list containing active data.frames
    subjects = character(0),    # character vector of all unique USUBJID values
    core_status = NULL          # data.frame listing required core datasets check status
  )
  
  # Instantiate Module Servers
  mod_config_server("config_tab", state)
  mod_data_explorer_server("explorer_tab", state)
  mod_patient_profile_server("patient_tab", state)
  
}
