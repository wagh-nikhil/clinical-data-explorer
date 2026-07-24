# R/mod_config.R
# Configuration & Dataset Registry Module

library(shiny)
library(dplyr)
library(purrr)
library(openxlsx)

#' Configuration UI
#' @param id Module ID
mod_config_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    layout_sidebar(
      sidebar = sidebar(
        title = "Directory Configuration",
        width = 320,
        card(
          card_header("Data Path Settings"),
          textInput(ns("sdtm_path"), "SDTM Path", value = "./data/sdtm"),
          textInput(ns("adam_path"), "ADaM Path", value = "./data/adam"),
          textInput(ns("derived_path"), "Derived Path", value = "./data/derived"),
          actionButton(
            ns("scan_btn"),
            "Scan Directories",
            class = "btn-primary w-100 mt-2",
            icon = icon("search")
          )
        ),
        card(
          card_header("Saved Configuration"),
          fileInput(
            ns("upload_config"),
            "Upload Config Excel",
            accept = c(".xlsx")
          ),
          helpText("Upload a previously saved configuration Excel file to auto-fill load checkboxes and variable selections.")
        )
      ),
      
      # Main panel contents
      uiOutput(ns("main_config_panel"))
    )
  )
}

#' Configuration Server
#' @param id Module ID
#' @param state Central reactiveValues state
mod_config_server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Track scanning trigger (initialized to 1 to auto-scan at startup)
    scan_trigger <- reactiveVal(1)
    
    # Store uploaded configuration for post-scan restoration
    uploaded_config <- reactiveVal(NULL)
    
    # Active dataset currently being configured inside the popup modal
    active_modal_dataset <- reactiveVal(NULL)
    
    observeEvent(input$scan_btn, {
      scan_trigger(scan_trigger() + 1)
    })
    
    # 1. Listen for Excel configuration uploads
    observeEvent(input$upload_config, {
      req(input$upload_config)
      
      file_path <- input$upload_config$datapath
      
      # Get sheet names to inspect structure
      sheets <- tryCatch({
        openxlsx::getSheetNames(file_path)
      }, error = function(e) {
        showNotification(paste("Failed to read Excel workbook:", e$message), type = "error")
        NULL
      })
      
      req(sheets)
      
      # Verify 'Data' sheet is present
      if (!("Data" %in% sheets)) {
        showNotification("Invalid Excel configuration: Must contain a 'Data' sheet.", type = "error", duration = 8)
        return()
      }
      
      # Read Data sheet
      data_df <- tryCatch({
        openxlsx::read.xlsx(file_path, sheet = "Data")
      }, error = function(e) {
        showNotification(paste("Failed to read 'Data' sheet:", e$message), type = "error")
        NULL
      })
      
      req(data_df)
      
      # Validate schema columns
      if (!all(c("Dataset", "Loaded", "Path") %in% colnames(data_df))) {
        showNotification("Invalid 'Data' sheet format. Must contain columns: Dataset, Loaded, Path.", type = "error", duration = 8)
        return()
      }
      
      # Extract global cohort subset from the USUBJID sheet if present
      cohort_subjs <- NULL
      if ("USUBJID" %in% sheets) {
        usub_df <- tryCatch({
          openxlsx::read.xlsx(file_path, sheet = "USUBJID")
        }, error = function(e) {
          NULL
        })
        if (!is.null(usub_df) && "USUBJID" %in% colnames(usub_df)) {
          cohort_subjs <- as.character(usub_df$USUBJID)
          # Clean list of any empty strings
          cohort_subjs <- trimws(cohort_subjs)
          cohort_subjs <- cohort_subjs[cohort_subjs != ""]
        }
      }
      
      # Parse dataset-specific tabs
      datasets_config <- list()
      for (sheet in sheets) {
        if (sheet %in% c("Data", "USUBJID")) next
        
        var_df <- tryCatch({
          openxlsx::read.xlsx(file_path, sheet = sheet)
        }, error = function(e) {
          NULL
        })
        
        if (!is.null(var_df) && "Variables" %in% colnames(var_df)) {
          datasets_config[[sheet]] <- as.character(var_df$Variables)
        } else {
          datasets_config[[sheet]] <- character(0)
        }
      }
      
      # Save configuration and trigger a scan to load the headers
      uploaded_excel_config <- list(
        data = data_df,
        datasets = datasets_config,
        cohort = cohort_subjs
      )
      uploaded_config(uploaded_excel_config)
      scan_trigger(scan_trigger() + 1)
    })
    
    # 2. Directory scanning process (metadata and column names scan)
    observeEvent(scan_trigger(), {
      req(scan_trigger() > 0)
      
      # Visual feedback
      showNotification("Scanning directories for clinical datasets...", type = "message", id = "scan_notif")
      
      sdtm_path <- input$sdtm_path
      adam_path <- input$adam_path
      derived_path <- input$derived_path
      
      # Step 1: Scan directories for files
      meta_df <- tryCatch({
        scan_directories(sdtm_path, adam_path, derived_path)
      }, error = function(e) {
        showNotification(paste("Error scanning paths:", e$message), type = "error")
        return(NULL)
      })
      
      if (is.null(meta_df) || nrow(meta_df) == 0) {
        showNotification("No clinical datasets found in target directories.", type = "warning", duration = 8)
        state$metadata <- NULL
        state$loaded_data <- list()
        state$subjects <- character(0)
        state$cohort_subset <- NULL
        state$core_status <- NULL
        removeNotification("scan_notif")
        return()
      }
      
      # Step 2: Read headers only to fetch variable choices (instantaneous)
      cols_list <- list()
      for (i in seq_len(nrow(meta_df))) {
        ds <- meta_df$dataset[i]
        path <- meta_df$path[i]
        
        cols_list[[ds]] <- tryCatch({
          ext <- tolower(fs::path_ext(path))
          if (ext == "sas7bdat") {
            colnames(read_sas(path, n_max = 0))
          } else if (ext == "rds") {
            colnames(readRDS(path))
          } else {
            character(0)
          }
        }, error = function(e) {
          character(0)
        })
      }
      
      # Step 3: Populate central state metadata and column schema
      state$metadata <- meta_df
      state$dataset_cols <- cols_list
      state$core_status <- check_core_datasets(meta_df)
      state$cohort_subset <- NULL # Reset cohort filter on a new directory scan
      
      # Initialize default selections (all variables) and inclusion checkboxes
      # (ticked for ADSL, DM, ADAE, AE by default to ensure baseline details are loaded)
      selections <- list()
      statuses <- list()
      inclusions <- list()
      
      default_checked <- c("ADSL", "DM", "ADAE", "AE")
      
      for (i in seq_len(nrow(meta_df))) {
        ds <- meta_df$dataset[i]
        selections[[ds]] <- cols_list[[ds]]
        
        is_included <- ds %in% default_checked
        inclusions[[ds]] <- is_included
        statuses[[ds]] <- if (is_included) "Configured" else "Excluded"
      }
      
      # If none of the preferred datasets were found, check the first 2 as fallback
      if (sum(unlist(inclusions)) == 0) {
        for (i in seq_len(nrow(meta_df))) {
          if (i <= 2) {
            ds <- meta_df$dataset[i]
            inclusions[[ds]] <- TRUE
            statuses[[ds]] <- "Configured"
          }
        }
      }
      
      state$dataset_selections <- selections
      state$dataset_status <- statuses
      state$dataset_inclusion <- inclusions
      
      removeNotification("scan_notif")
      showNotification("Directory scanned successfully.", type = "default", duration = 4)
      
      # Step 4: Restore selections if an uploaded config exists
      excel_config <- uploaded_config()
      if (!is.null(excel_config)) {
        uploaded_config(NULL) # Reset
        
        data_df <- excel_config$data
        datasets_config <- excel_config$datasets
        state$cohort_subset <- excel_config$cohort # Save cohort subset
        
        restored_selections <- list()
        restored_statuses <- list()
        restored_inclusions <- list()
        
        for (ds in meta_df$dataset) {
          row_match <- data_df[data_df$Dataset == ds, ]
          
          if (nrow(row_match) > 0) {
            loaded_flag <- toupper(trimws(as.character(row_match$Loaded[1])))
            is_included <- (loaded_flag %in% c("Y", "YES", "TRUE", "T"))
            
            # Parse variables list from sheet configuration
            if (ds %in% names(datasets_config)) {
              selected_cols <- datasets_config[[ds]]
              selected_cols <- trimws(selected_cols)
              selected_cols <- selected_cols[selected_cols != ""]
              # Validate column name checks against actual schema to prevent typos
              selected_cols <- intersect(selected_cols, state$dataset_cols[[ds]])
            } else {
              selected_cols <- state$dataset_cols[[ds]]
            }
            
            # Fallback if variable selection is empty
            if (length(selected_cols) == 0) {
              selected_cols <- state$dataset_cols[[ds]]
            }
            
            restored_selections[[ds]] <- selected_cols
            restored_inclusions[[ds]] <- is_included
            restored_statuses[[ds]] <- if (is_included) "Configured" else "Excluded"
          } else {
            restored_selections[[ds]] <- NULL
            restored_statuses[[ds]] <- "Excluded"
            restored_inclusions[[ds]] <- FALSE
          }
        }
        
        state$dataset_selections <- restored_selections
        state$dataset_status <- restored_statuses
        state$dataset_inclusion <- restored_inclusions
        
        # Update row checkbox UI bindings to match restored values
        for (ds in meta_df$dataset) {
          updateCheckboxInput(session, paste0("load_", ds), value = restored_inclusions[[ds]])
        }
        
        # Show alert if a cohort was loaded
        if (!is.null(state$cohort_subset)) {
          showNotification(
            sprintf("Active Cohort Filter: Loaded %d subjects from USUBJID sheet.", length(state$cohort_subset)),
            type = "warning",
            duration = 8
          )
        }
        
        showNotification("Configuration Excel restored successfully.", type = "default", duration = 5)
      }
    })
    
    # 3. Dynamic observer list registry for table configuration cogs and row checkboxes
    observed_buttons <- character(0)
    observed_checkboxes <- character(0)
    
    observe({
      req(state$metadata)
      
      walk(state$metadata$dataset, function(ds) {
        # Observe Cog Button
        btn_id <- paste0("btn_cfg_", ds)
        if (!(btn_id %in% observed_buttons)) {
          observed_buttons <<- c(observed_buttons, btn_id)
          observeEvent(input[[btn_id]], {
            active_modal_dataset(ds)
            show_schema_modal(ds)
          }, ignoreInit = TRUE)
        }
        
        # Observe Row Inclusion Checkbox
        cb_id <- paste0("load_", ds)
        if (!(cb_id %in% observed_checkboxes)) {
          observed_checkboxes <<- c(observed_checkboxes, cb_id)
          observeEvent(input[[cb_id]], {
            state$dataset_inclusion[[ds]] <- input[[cb_id]]
            
            # Auto-update status badge based on checkbox toggle
            if (input[[cb_id]]) {
              if (state$dataset_status[[ds]] == "Excluded") {
                state$dataset_status[[ds]] <- "Configured"
              }
            } else {
              state$dataset_status[[ds]] <- "Excluded"
            }
          }, ignoreInit = TRUE)
        }
      })
    })
    
    # 4. Display Variable Selection Modal Popup with live search filter
    show_schema_modal <- function(ds) {
      cols <- state$dataset_cols[[ds]]
      current_selection <- state$dataset_selections[[ds]]
      
      showModal(modalDialog(
        title = paste("Select Display Variables:", ds),
        size = "l",
        easyClose = FALSE,
        fade = TRUE,
        
        tags$div(
          class = "container-fluid p-0",
          tags$div(
            class = "row mb-3 align-items-center g-2",
            tags$div(
              class = "col-md-6",
              tags$input(
                id = "var_search",
                type = "text",
                class = "form-control",
                placeholder = "🔍 Type to filter variables instantly..."
              )
            ),
            tags$div(
              class = "col-md-6 text-end d-flex gap-2 justify-content-end",
              actionButton(ns("btn_select_all"), "Select All", class = "btn-outline-secondary btn-sm"),
              actionButton(ns("btn_deselect_all"), "Clear All", class = "btn-outline-secondary btn-sm")
            )
          ),
          
          tags$div(
            id = "var_list_container",
            style = "max-height: 350px; overflow-y: auto; border: 1px solid #dee2e6; padding: 15px; border-radius: 4px; background: #fafafa;",
            checkboxGroupInput(
              ns("modal_cols"),
              label = NULL,
              choices = cols,
              selected = current_selection
            )
          ),
          helpText("Check the variables you want to display in the explorer grids. Rest of the variables will be skipped during load.")
        ),
        
        # Client-side jQuery filter script for instant searches (Zero server round-trips!)
        tags$script(HTML("
          $('#var_search').on('input', function() {
            var query = $(this).val().toLowerCase();
            $('#var_list_container .checkbox').each(function() {
              var labelText = $(this).find('span').text().toLowerCase();
              if (labelText.indexOf(query) > -1) {
                $(this).show();
              } else {
                $(this).hide();
              }
            });
          });
        ")),
        
        footer = tagList(
          actionButton(ns("save_modal_btn"), "Save & Close", class = "btn-primary"),
          modalButton("Cancel")
        )
      ))
    }
    
    # Save modal configurations
    observeEvent(input$save_modal_btn, {
      ds <- active_modal_dataset()
      req(ds)
      
      selected_cols <- input$modal_cols
      all_cols <- state$dataset_cols[[ds]]
      
      # Force USUBJID to be preserved if present
      if ("USUBJID" %in% all_cols && !("USUBJID" %in% selected_cols)) {
        selected_cols <- c("USUBJID", selected_cols)
      }
      
      state$dataset_selections[[ds]] <- selected_cols
      
      # If checked cols is non-empty, mark as Configured
      if (length(selected_cols) > 0) {
        state$dataset_status[[ds]] <- "Configured"
      }
      
      removeModal()
      showNotification(paste("Saved variables configuration for", ds), type = "default", duration = 3)
    })
    
    # Observe select all / deselect all actions in the modal
    observeEvent(input$btn_select_all, {
      ds <- active_modal_dataset()
      req(ds)
      updateCheckboxGroupInput(session, "modal_cols", selected = state$dataset_cols[[ds]])
    })
    
    observeEvent(input$btn_deselect_all, {
      updateCheckboxGroupInput(session, "modal_cols", selected = character(0))
    })
    
    # 5. Excel Save Configuration download handler (Multi-sheet workbook!)
    output$download_config <- downloadHandler(
      filename = function() {
        paste0("clindata_config_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        wb <- openxlsx::createWorkbook()
        
        # 1. Build and add the 'Data' tab (Registry overview)
        datasets <- state$metadata$dataset
        
        data_rows <- lapply(datasets, function(ds) {
          is_included <- state$dataset_inclusion[[ds]]
          if (is_included) is_included <- TRUE
          
          path <- state$metadata$path[state$metadata$dataset == ds]
          if (length(path) == 0) path <- ""
          
          data.frame(
            Dataset = ds,
            Loaded = if (is_included) "Y" else "N",
            Path = path[1],
            stringsAsFactors = FALSE
          )
        })
        
        data_df <- do.call(rbind, data_rows)
        openxlsx::addWorksheet(wb, "Data")
        openxlsx::writeData(wb, "Data", data_df)
        
        # 2. Add 'USUBJID' tab listing the distinct subjects (cohort list)
        subjs_df <- data.frame(USUBJID = state$subjects, stringsAsFactors = FALSE)
        openxlsx::addWorksheet(wb, "USUBJID")
        openxlsx::writeData(wb, "USUBJID", subjs_df)
        
        # 3. Add individual tab per dataset listing its selected column subsets
        for (ds in datasets) {
          cols <- state$dataset_selections[[ds]]
          if (is.null(cols) || length(cols) == 0) {
            cols <- state$dataset_cols[[ds]]
          }
          
          openxlsx::addWorksheet(wb, ds)
          openxlsx::writeData(wb, ds, data.frame(Variables = cols, stringsAsFactors = FALSE))
        }
        
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
    
    # 6. Ingest subsetted configured data on load button click
    observeEvent(input$load_btn, {
      # Ingest datasets where inclusion checkbox is ticked
      datasets_to_load <- names(state$dataset_inclusion)[unlist(state$dataset_inclusion) == TRUE]
      
      shiny::validate(
        need(length(datasets_to_load) > 0, "No datasets are selected for load. Please tick the 'Load' checkbox for at least one dataset.")
      )
      
      showNotification("Ingesting configured clinical subset...", type = "message", id = "load_notif")
      
      loaded_data <- list()
      subjects <- character(0)
      
      # Reset active caches
      state$loaded_data <- list()
      state$subjects <- character(0)
      
      for (ds in datasets_to_load) {
        ds_row <- state$metadata[state$metadata$dataset == ds, ]
        if (nrow(ds_row) == 0) next
        
        file_path <- ds_row$path[1]
        ext <- tolower(fs::path_ext(file_path))
        selected_cols <- state$dataset_selections[[ds]]
        
        # If no variables configured (e.g. empty selection), fall back to all columns
        if (is.null(selected_cols) || length(selected_cols) == 0) {
          selected_cols <- state$dataset_cols[[ds]]
        }
        
        df <- tryCatch({
          if (ext == "sas7bdat") {
            haven::read_sas(file_path, col_select = all_of(selected_cols))
          } else if (ext == "rds") {
            full_df <- readRDS(file_path)
            colnames(full_df) <- toupper(colnames(full_df))
            full_df[, intersect(selected_cols, colnames(full_df)), drop = FALSE]
          } else {
            stop("Unsupported format")
          }
        }, error = function(e) {
          showNotification(sprintf("Failed to load %s: %s", ds, e$message), type = "error")
          NULL
        })
        
        if (!is.null(df)) {
          colnames(df) <- toupper(colnames(df))
          
          # Apply cohort subset filtering if loaded from Excel configuration
          if (!is.null(state$cohort_subset) && "USUBJID" %in% colnames(df)) {
            df <- df %>% filter(USUBJID %in% state$cohort_subset)
          }
          
          loaded_data[[ds]] <- df
          
          # Extract subjects
          if ("USUBJID" %in% colnames(df)) {
            subjects <- union(subjects, unique(as.character(df$USUBJID)))
          }
          
          # Update metadata loaded sizes
          state$metadata$rows[state$metadata$dataset == ds] <- nrow(df)
          state$metadata$cols[state$metadata$dataset == ds] <- ncol(df)
          
          # Update status
          state$dataset_status[[ds]] <- "Loaded"
        }
      }
      
      # Update central states
      state$loaded_data <- loaded_data
      state$subjects <- sort(subjects)
      state$dataset_list <- state$metadata %>%
        filter(dataset %in% names(loaded_data)) %>%
        select(dataset, type)
      
      removeNotification("load_notif")
      
      msg <- sprintf("Successfully ingested %d datasets with %d subjects available.", length(state$loaded_data), length(state$subjects))
      if (!is.null(state$cohort_subset)) {
        msg <- paste(msg, sprintf("(Restricted to %d cohort subjects)", length(state$cohort_subset)))
      }
      
      showNotification(msg, type = "default", duration = 5)
    })
    
    # 7. Render dynamic summary text for loaded data
    output$config_summary_text <- renderUI({
      req(state$dataset_status)
      statuses <- unlist(state$dataset_status)
      inclusions <- unlist(state$dataset_inclusion)
      
      loaded_cnt <- sum(statuses == "Loaded")
      configured_cnt <- sum(inclusions == TRUE & statuses != "Loaded")
      excluded_cnt <- sum(inclusions == FALSE)
      
      cohort_badge <- if (!is.null(state$cohort_subset)) {
        span(class = "badge bg-warning py-2 px-3", paste("Cohort Filter:", length(state$cohort_subset), "Subjects"))
      } else {
        NULL
      }
      
      div(
        class = "d-flex gap-2 flex-wrap mb-2 mt-1",
        span(class = "badge bg-success py-2 px-3", paste(loaded_cnt, "Active (Loaded)")),
        span(class = "badge bg-primary py-2 px-3", paste(configured_cnt, "Pending Load")),
        span(class = "badge bg-danger py-2 px-3", paste(excluded_cnt, "Excluded")),
        cohort_badge
      )
    })
    
    # 8. Main Configuration panel switcher
    output$main_config_panel <- renderUI({
      if (is.null(state$metadata) || nrow(state$metadata) == 0) {
        return(
          card(
            class = "text-center py-5 text-muted",
            div(
              icon("folder-open", class = "fa-3x mb-3"),
              h4("No Directories Scanned"),
              p("Configure the paths in the sidebar and click 'Scan Directories' to start.")
            )
          )
        )
      }
      
      layout_column_wrap(
        width = 1,
        # Scanned Registry and Configuration Settings Table (On Top!)
        card(
          card_header("Scanned Datasets Schema Registry"),
          full_screen = TRUE,
          uiOutput(ns("config_table_ui"))
        ),
        
        # Combined Ingestion & Configuration Controller (Compact and small, at the bottom)
        card(
          card_header("Ingestion Control Center"),
          layout_column_wrap(
            width = 1/2,
            # Left: Ingestion trigger and count
            div(
              uiOutput(ns("config_summary_text")),
              actionButton(
                ns("load_btn"),
                "Load Configured Data",
                class = "btn-success w-100 mt-2",
                icon = icon("download")
              )
            ),
            # Right: Config Save/Load file inputs (in one row)
            div(
              class = "d-flex flex-column gap-2 mt-2",
              downloadButton(ns("download_config"), "Save Configuration Excel", class = "btn-outline-primary w-100 btn-sm"),
              p(class = "text-muted small mb-0", "Download your configuration workbook to edit paths, variables, and cohort USUBJID lists in Excel dynamically.")
            )
          )
        )
      )
    })
    
    # 9. HTML configuration Table renderer with load checkbox column
    output$config_table_ui <- renderUI({
      req(state$metadata)
      df <- state$metadata
      
      rows <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        ds <- row$dataset
        
        status <- state$dataset_status[[ds]]
        if (is.null(status)) status <- "Unconfigured"
        
        badge_class <- switch(
          status,
          "Unconfigured" = "bg-secondary",
          "Configured" = "bg-primary",
          "Loaded" = "bg-success",
          "Excluded" = "bg-danger",
          "bg-secondary"
        )
        
        is_ticked <- state$dataset_inclusion[[ds]]
        if (is.null(is_ticked)) is_ticked <- (i <= 2)
        
        tags$tr(
          # Render load checkbox
          tags$td(
            style = "text-align: center; width: 60px;",
            checkboxInput(ns(paste0("load_", ds)), label = NULL, value = is_ticked)
          ),
          tags$td(tags$strong(ds)),
          tags$td(row$type),
          tags$td(row$size),
          tags$td(as.character(row$modified)),
          tags$td(tags$span(class = paste("badge py-1 px-2", badge_class), status)),
          tags$td(
            actionButton(
              ns(paste0("btn_cfg_", ds)),
              label = "Configure Variables",
              class = "btn-outline-primary btn-sm",
              icon = icon("cog")
            )
          )
        )
      })
      
      tags$table(
        class = "table table-striped table-hover align-middle mb-0",
        tags$thead(
          tags$tr(
            tags$th(style = "text-align: center; width: 60px;", "Load"),
            tags$th("Dataset Name"),
            tags$th("Dataset Type"),
            tags$th("File Size"),
            tags$th("Date Modified"),
            tags$th("Load Status"),
            tags$th("Schema Settings")
          )
        ),
        tags$tbody(rows)
      )
    })
  })
}
