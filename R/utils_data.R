# R/utils_data.R
# Core helper functions for scanning, lazy-loading, caching, and filtering datasets.

library(fs)
library(purrr)
library(dplyr)
library(haven)
library(shiny)

#' Scan directories for SAS and RDS datasets and extract metadata (Instant metadata scan)
#' @param sdtm_path Path to SDTM directory
#' @param adam_path Path to ADaM directory
#' @param derived_path Path to Derived directory
#' @return A data.frame of dataset metadata
scan_directories <- function(sdtm_path, adam_path, derived_path) {
  paths_map <- list(
    SDTM = sdtm_path,
    ADaM = adam_path,
    Derived = derived_path
  )
  
  meta_list <- list()
  
  for (type_name in names(paths_map)) {
    path <- paths_map[[type_name]]
    
    if (is.null(path) || path == "" || !dir_exists(path)) {
      next
    }
    
    # Scan for .sas7bdat and .rds files
    files <- tryCatch({
      dir_info(path, recurse = FALSE) %>%
        filter(type == "file", grepl("\\.(sas7bdat|rds)$", path, ignore.case = TRUE))
    }, error = function(e) {
      NULL
    })
    
    if (is.null(files) || nrow(files) == 0) {
      next
    }
    
    # Extract file details
    type_meta <- files %>%
      transmute(
        dataset = toupper(path_ext_remove(path_file(path))),
        filename = path_file(path),
        path = path_abs(path),
        type = type_name,
        size = as.character(size),
        modified = modification_time,
        rows = NA_integer_,
        cols = NA_integer_
      )
    
    meta_list[[type_name]] <- type_meta
  }
  
  if (length(meta_list) == 0) {
    return(data.frame(
      dataset = character(),
      filename = character(),
      path = character(),
      type = character(),
      size = character(),
      modified = Sys.time()[0],
      rows = integer(),
      cols = integer(),
      stringsAsFactors = FALSE
    ))
  }
  
  bind_rows(meta_list) %>%
    arrange(type, dataset)
}

#' Lazily load a dataset into memory if it is not already loaded
#' @param ds_name Name of the dataset (e.g. "ADAE")
#' @param state Central reactiveValues state
#' @return Logical indicating success
ensure_dataset_loaded <- function(ds_name, state) {
  if (is.null(state$metadata) || nrow(state$metadata) == 0) {
    return(FALSE)
  }
  
  # Return immediately if already cached
  if (ds_name %in% names(state$loaded_data)) {
    return(TRUE)
  }
  
  # Find row in metadata
  ds_row <- state$metadata[state$metadata$dataset == ds_name, ]
  if (nrow(ds_row) == 0) {
    return(FALSE)
  }
  
  file_path <- ds_row$path[1]
  ext <- tolower(path_ext(file_path))
  
  # Show temporary visual load notification if session exists
  has_session <- !is.null(getDefaultReactiveDomain())
  notif_id <- NULL
  if (has_session) {
    notif_id <- showNotification(
      paste("Loading dataset", ds_name, "from disk..."), 
      type = "message", 
      duration = NULL
    )
  }
  
  df <- tryCatch({
    if (ext == "sas7bdat") {
      read_sas(file_path)
    } else if (ext == "rds") {
      readRDS(file_path)
    } else {
      stop("Unsupported file extension")
    }
  }, error = function(e) {
    if (has_session) {
      showNotification(sprintf("Failed to read %s: %s", ds_name, e$message), type = "error")
    }
    NULL
  })
  
  # Remove notification
  if (has_session && !is.null(notif_id)) {
    removeNotification(notif_id)
  }
  
  if (is.null(df)) {
    return(FALSE)
  }
  
  # Standardize columns to uppercase
  colnames(df) <- toupper(colnames(df))
  
  # Cache dataframe
  state$loaded_data[[ds_name]] <- df
  
  # Extract unique subjects list if USUBJID is present and merge
  if ("USUBJID" %in% colnames(df)) {
    new_subjs <- unique(as.character(df$USUBJID))
    state$subjects <- sort(union(state$subjects, new_subjs))
  }
  
  # Update metadata dimension cache
  state$metadata$rows[state$metadata$dataset == ds_name] <- nrow(df)
  state$metadata$cols[state$metadata$dataset == ds_name] <- ncol(df)
  
  if (has_session) {
    showNotification(
      sprintf("Loaded %s successfully (%d rows, %d columns)", ds_name, nrow(df), ncol(df)), 
      type = "default", 
      duration = 3
    )
  }
  
  TRUE
}

#' Get unique list of subjects (USUBJID) from cached datasets
#' @param loaded_data A named list of loaded dataframes
#' @return Character vector of unique USUBJID values
get_subject_list <- function(loaded_data) {
  if (length(loaded_data) == 0) {
    return(character(0))
  }
  
  # Try ADSL first, then DM
  if ("ADSL" %in% names(loaded_data)) {
    df <- loaded_data[["ADSL"]]
    if ("USUBJID" %in% colnames(df)) {
      return(sort(unique(as.character(df$USUBJID))))
    }
  }
  
  if ("DM" %in% names(loaded_data)) {
    df <- loaded_data[["DM"]]
    if ("USUBJID" %in% colnames(df)) {
      return(sort(unique(as.character(df$USUBJID))))
    }
  }
  
  # Fallback: Union of USUBJID across all loaded datasets
  subj_union <- character(0)
  for (ds in names(loaded_data)) {
    df <- loaded_data[[ds]]
    if ("USUBJID" %in% colnames(df)) {
      subj_union <- union(subj_union, as.character(df$USUBJID))
    }
  }
  
  sort(subj_union)
}

#' Check core clinical datasets status
#' @param metadata_df The dataset metadata dataframe
#' @return A data.frame showing status of core datasets
check_core_datasets <- function(metadata_df) {
  core_list <- c("DM", "ADSL", "AE", "ADAE", "LB", "ADLB")
  
  discovered <- if (!is.null(metadata_df) && nrow(metadata_df) > 0) {
    metadata_df$dataset
  } else {
    character(0)
  }
  
  data.frame(
    Dataset = core_list,
    Required = TRUE,
    Status = core_list %in% discovered,
    stringsAsFactors = FALSE
  )
}
