# R/mod_data_explorer.R
# Global Data Explorer Module

#' Global Data Explorer UI
#' @param id Module ID
mod_data_explorer_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Dataset & Subject Filters",
        width = 320,
        # Implemented accordion for clean sidebar groupings
        accordion(
          id = ns("filter_accordion"),
          open = c("Select Datasets", "Subject Level Filters"),
          multiple = TRUE,
          accordion_panel(
            "Select Datasets",
            icon = icon("database"),
            # Replaced with clean selectizeInput (Option A) matching Patient Profile page
            selectizeInput(
              ns("selected_datasets"),
              "Target Datasets",
              choices = NULL,
              multiple = TRUE,
              options = list(
                placeholder = "Select datasets to display",
                plugins = list('remove_button', 'drag_drop')
              )
            )
          ),
          accordion_panel(
            "Subject Level Filters",
            icon = icon("filter"),
            checkboxInput(
              ns("restrict_five"),
              "Restrict to First 5 Subjects",
              value = TRUE
            ),
            helpText("Slices all datasets to only show the first 5 unique subjects for fast preview."),
            hr(),
            selectizeInput(
              ns("selected_subjects"),
              "Filter by Subject IDs",
              choices = NULL,
              multiple = TRUE,
              options = list(placeholder = "Search and select USUBJID(s)")
            )
          )
        )
      ),
      
      # Main panel contents
      uiOutput(ns("tabs_container_ui"))
    )
  )
}

#' Global Data Explorer Server
#' @param id Module ID
#' @param state Central reactiveValues state
mod_data_explorer_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Local variable to track registered grids and prevent duplicate observer definitions
    registered_grids <- character(0)
    
    # 1. Update dataset selection choices (only displays configured loaded datasets)
    observe({
      req(state$dataset_list)
      
      # Prefer ADSL as default if loaded, else none selected by default
      default_sel <- if ("ADSL" %in% state$dataset_list$dataset) "ADSL" else NULL
      
      updateSelectizeInput(
        session,
        "selected_datasets",
        choices = state$dataset_list$dataset,
        selected = default_sel
      )
    })
    
    # 2. Update subject filter choices when global subjects list changes
    observe({
      req(state$subjects)
      
      updateSelectizeInput(
        session,
        "selected_subjects",
        choices = state$subjects,
        selected = NULL
      )
    })
    
    # 3. Register output renderers ONCE per discovered dataset
    # This prevents duplicate observer registrations and blank grids on dynamic UI refreshes.
    observeEvent(state$dataset_list, {
      req(state$dataset_list)
      all_datasets <- state$dataset_list$dataset
      
      walk(all_datasets, function(ds) {
        if (ds %in% registered_grids) {
          return()
        }
        
        # Add to local tracker immediately
        registered_grids <<- c(registered_grids, ds)
        
        grid_id <- paste0("grid_", ds)
        row_id <- paste0("row_count_", ds)
        
        # Render grid row count
        output[[row_id]] <- renderText({
          df <- get_filtered_data(ds)
          if (is.null(df)) "0" else as.character(nrow(df))
        })
        
        # Render high-performance smart data grid
        output[[grid_id]] <- dtsmartr::renderDtsmartr({
          df <- get_filtered_data(ds)
          
          shiny::validate(
            need(!is.null(df), paste("Dataset", ds, "not loaded.")),
            need(nrow(df) > 0, "No records match current subject filters.")
          )
          
          # Render the high-performance grid (CSV export disabled)
          dtsmartr::dtsmartr(
            df,
            datasetName = ds,
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
    
    # 4. Dynamic UI Container for selected dataset tabs
    output$tabs_container_ui <- renderUI({
      if (is.null(state$dataset_list) || nrow(state$dataset_list) == 0) {
        return(
          card(
            class = "text-center py-5 text-muted",
            div(
              icon("cogs", class = "fa-3x mb-3"),
              h4("No Datasets Ingested"),
              p("Please configure and load your clinical datasets on the Configuration & Registry page first.")
            )
          )
        )
      }
      
      current_ds <- input$selected_datasets
      if (length(current_ds) == 0) {
        return(
          card(
            class = "text-center py-5 text-muted",
            div(
              icon("check-square", class = "fa-3x mb-3"),
              h4("No Datasets Selected"),
              p("Select one or more datasets in the sidebar to view their contents.")
            )
          )
        )
      }
      
      # Build navigation panels dynamically
      panels <- lapply(current_ds, function(ds) {
        nav_panel(
          title = ds,
          card(
            card_header(
              class = "d-flex justify-content-between align-items-center",
              div(
                span(class = "fw-bold text-primary", ds),
                span(class = "text-muted ms-2", "(", textOutput(ns(paste0("row_count_", ds)), inline = TRUE), " rows)")
              )
            ),
            full_screen = TRUE,
            shinycssloaders::withSpinner(
              dtsmartr::dtsmartrOutput(ns(paste0("grid_", ds)), height = "550px"),
              type = 6,
              color = "#0275d8"
            )
          )
        )
      })
      
      # Return tabset card
      do.call(navset_card_tab, c(panels, list(id = ns("explorer_tabs"))))
    })
    
    # Helper to retrieve and slice data reactively based on filters
    get_filtered_data <- function(ds) {
      df <- state$loaded_data[[ds]]
      if (is.null(df) || nrow(df) == 0) {
        return(df)
      }
      
      # Subject level filters
      if ("USUBJID" %in% colnames(df)) {
        if (input$restrict_five) {
          # Slice to first 5 subjects present in this dataset
          subjs <- head(sort(unique(as.character(df$USUBJID))), 5)
          df <- df %>% filter(USUBJID %in% subjs)
        } else if (length(input$selected_subjects) > 0) {
          df <- df %>% filter(USUBJID %in% input$selected_subjects)
        }
      }
      
      df
    }
  })
}
