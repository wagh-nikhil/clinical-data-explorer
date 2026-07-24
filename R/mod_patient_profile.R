# R/mod_patient_profile.R
# Patient Profile Explorer Module

#' Patient Profile Explorer UI
#' @param id Module ID
mod_patient_profile_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Subject Profile Selection",
        width = 320,
        card(
          card_header("Select Patient"),
          selectizeInput(
            ns("selected_patient"),
            "Patient Subject ID (USUBJID)",
            choices = NULL,
            options = list(placeholder = "Search and select a subject ID")
          )
        ),
        card(
          card_header("Select & Order Datasets"),
          selectizeInput(
            ns("selected_profile_datasets"),
            "Choose Datasets (in Tab Order)",
            choices = NULL,
            multiple = TRUE,
            options = list(
              placeholder = "Select datasets to display",
              plugins = list('remove_button', 'drag_drop')
            )
          ),
          helpText("Select one or more loaded datasets. The tabs will render in the exact order selected.")
        )
      ),
      
      # Main panel contents
      uiOutput(ns("profile_main_ui"))
    )
  )
}

#' Patient Profile Explorer Server
#' @param id Module ID
#' @param state Central reactiveValues state
mod_patient_profile_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Local variable to track registered grids and prevent duplicate observer definitions
    registered_profile_grids <- character(0)
    
    # 1. Update patient selector when subjects list changes
    observe({
      req(state$subjects)
      
      # Default to the first subject
      default_patient <- if (length(state$subjects) > 0) state$subjects[1] else NULL
      
      updateSelectizeInput(
        session,
        "selected_patient",
        choices = state$subjects,
        selected = default_patient,
        server = TRUE
      )
    })
    
    # 2. Update profile datasets selection choices (don't select by default)
    observe({
      req(state$dataset_list)
      
      updateSelectizeInput(
        session,
        "selected_profile_datasets",
        choices = state$dataset_list$dataset,
        selected = NULL # Don't select by default
      )
    })
    
    # 4. Main profile area (Demographics Header & Selected Tabs Grids)
    output$profile_main_ui <- renderUI({
      if (is.null(state$dataset_list) || nrow(state$dataset_list) == 0) {
        return(
          card(
            class = "text-center py-5 text-muted",
            div(
              icon("user-shield", class = "fa-3x mb-3"),
              h4("No Profiles Available"),
              p("Please scan and load datasets on the Configuration page first.")
            )
          )
        )
      }
      
      req(input$selected_patient)
      
      # Fetch demographic details
      demo <- get_patient_demographics(input$selected_patient, state$loaded_data)
      
      # Demographic Summary text bar displayed in a single line
      demo_bar <- div(
        class = "alert alert-info py-2 px-3 mb-2 d-flex align-items-center justify-content-between flex-wrap g-2",
        style = "font-size: 0.95rem; border-radius: 6px; border: 1px solid #bde5f8;",
        div(
          tags$strong("Patient Profile: "),
          span(class = "badge bg-primary text-white ms-1 me-3 py-1 px-2 fs-6", input$selected_patient)
        ),
        div(
          class = "d-flex gap-4 flex-wrap align-items-center",
          span(tags$strong("Age: "), demo$Age),
          span(tags$strong("Sex: "), demo$Sex),
          span(tags$strong("Race: "), demo$Race),
          span(tags$strong("Treatment Arm: "), class = "text-primary fw-bold", demo$Arm)
        )
      )
      
      selected_ds <- input$selected_profile_datasets
      
      # If no datasets are selected, prompt the user
      if (length(selected_ds) == 0) {
        return(
          tagList(
            demo_bar,
            card(
              class = "text-center py-5 text-muted mt-3",
              div(
                icon("list-ol", class = "fa-3x mb-3"),
                h4("No Datasets Selected"),
                p("Please select and arrange datasets in the sidebar to view patient-specific tables.")
              )
            )
          )
        )
      }
      
      # Build the panels dynamically in the selected order
      panels <- lapply(selected_ds, function(ds) {
        nav_panel(
          title = ds,
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span(class = "fw-bold text-primary", paste("Patient Records:", ds))
            ),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              dtsmartr::dtsmartrOutput(ns(paste0("profile_grid_", ds)), height = "500px"),
              type = 6,
              color = "#0275d8"
            )
          )
        )
      })
      
      tabset_card <- do.call(navset_card_tab, c(panels, list(id = ns("profile_tabs"))))
      
      tagList(
        demo_bar,
        tabset_card
      )
    })
    
    # 5. Register output renderers ONCE per discovered dataset
    # This prevents duplicate observer registrations and blank grids on dynamic UI refreshes.
    observeEvent(state$dataset_list, {
      req(state$dataset_list)
      all_datasets <- state$dataset_list$dataset
      
      walk(all_datasets, function(ds) {
        if (ds %in% registered_profile_grids) {
          return()
        }
        
        # Add to local tracker immediately
        registered_profile_grids <<- c(registered_profile_grids, ds)
        
        grid_id <- paste0("profile_grid_", ds)
        
        output[[grid_id]] <- dtsmartr::renderDtsmartr({
          df <- state$loaded_data[[ds]]
          
          shiny::validate(
            need(!is.null(df), paste("Dataset", ds, "not loaded.")),
            need(input$selected_patient, "No patient selected.")
          )
          
          # Filter for selected patient if USUBJID exists
          if ("USUBJID" %in% colnames(df)) {
            df <- df %>% filter(USUBJID == input$selected_patient)
            shiny::validate(
              need(nrow(df) > 0, paste("No records found for subject in dataset", ds))
            )
          } else {
            shiny::validate(
              need(FALSE, paste("Dataset", ds, "does not contain a USUBJID column to filter by."))
            )
          }
          
          # Render the high-performance grid
          dtsmartr::dtsmartr(
            df,
            datasetName = paste("Patient", ds),
            options = dtsmartr::dtsmartr_options(
              advanced_filter = TRUE,
              show_labels = TRUE,
              column_picker = TRUE,
              allow_export = FALSE
            )
          )
        })
      })
    })
    
    # --- Demographic Extraction Helper ---
    get_patient_demographics <- function(subj_id, loaded_data) {
      df <- NULL
      if ("ADSL" %in% names(loaded_data)) {
        df <- loaded_data[["ADSL"]]
      } else if ("DM" %in% names(loaded_data)) {
        df <- loaded_data[["DM"]]
      }
      
      if (is.null(df)) {
        return(list(Age = "N/A", Sex = "N/A", Race = "N/A", Arm = "N/A"))
      }
      
      subj_df <- df %>% filter(USUBJID == subj_id)
      if (nrow(subj_df) == 0) {
        return(list(Age = "N/A", Sex = "N/A", Race = "N/A", Arm = "N/A"))
      }
      
      # Standardize extracting columns
      age <- if ("AGE" %in% colnames(subj_df)) as.character(subj_df$AGE[1]) else "N/A"
      ageu <- if ("AGEU" %in% colnames(subj_df)) as.character(subj_df$AGEU[1]) else ""
      sex <- if ("SEX" %in% colnames(subj_df)) as.character(subj_df$SEX[1]) else "N/A"
      race <- if ("RACE" %in% colnames(subj_df)) as.character(subj_df$RACE[1]) else "N/A"
      
      arm <- if ("ARM" %in% colnames(subj_df)) {
        as.character(subj_df$ARM[1])
      } else if ("TRT01A" %in% colnames(subj_df)) {
        as.character(subj_df$TRT01A[1])
      } else if ("TRT01P" %in% colnames(subj_df)) {
        as.character(subj_df$TRT01P[1])
      } else {
        "N/A"
      }
      
      list(
        Age = paste(age, ageu),
        Sex = sex,
        Race = race,
        Arm = arm
      )
    }
  })
}
