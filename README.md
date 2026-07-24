# Clinical Data Explorer (ClinDataExplorer)

An advanced, high-performance R Shiny web application designed for clinical trials data managers, statisticians, and medical monitors. The explorer offers a responsive interface built on `bslib` and `dtsmartr` to dynamically scan, configure, subset, and visualize SDTM, ADaM, and custom derived clinical datasets.

---

## 🚀 Key Features

### 1. Schema Ingestion & Registry Configuration
* **Directory Scanner**: Scan local SDTM, ADaM, and custom derived data paths (`.sas7bdat` and `.rds` file formats supported) with zero server latency.
* **Granular Variable Configuration**: Checkbox list of variables to load per dataset, featuring an **instant client-side jQuery search box** (filter columns in real-time with zero server lag), and batch "Select All" / "Clear All" helpers.
* **Row-Level Load Controls**: Select which datasets to load in memory using checkboxes directly inside the scanned registry table.
* **Core Dataset Checks**: Automatic checklist warning indicators if baseline safety datasets (`ADSL`, `ADAE`, `DM`, `AE`) are missing.

### 2. Multi-Sheet Excel Configuration Engine
Save and restore entire session states directly using custom structured Excel workbooks (`.xlsx` format):
* **`Data` sheet**: Master registry of datasets, file paths, and inclusion load status flags (`Y`/`N`).
* **`USUBJID` sheet**: Automatically generated tab containing a unique list of cohort subject IDs. **You can manually edit this list in Excel to define study cohort filters!** Upon upload, the app subsets all tables to match only these subjects.
* **Dataset-specific sheets**: Separate tabs (e.g. `ADSL`, `ADAE`) containing a single column `Variables` indicating exactly which columns should be read from disk.

### 3. Global Data Explorer
* **Dataset Selector**: High-performance multi-select dropdown grouping datasets cleanly.
* **Reactivity Decoupled**: Dynamic dropdown list is pinned to scanned variables, preventing resets during active loading.
* **Virtualized Ag-Grid Tables**: Utilizes the modern `dtsmartr` engine to render datasets with full column picking, search bars, and filtering controls.
* **Cohort Filtering**: Direct multi-subject selectize filter, pre-configured with a **"Restrict to First 5 Subjects"** default toggle to keep rendering instantaneous.

### 4. Patient Profile Explorer
* **Patient Selector**: Quick-search drop-down list of all subject IDs.
* **Compact Demographic Banner**: A single-line layout alert showing **Patient ID, Age, Sex, Race, and Treatment Arm** cleanly at the top of the workspace.
* **Dynamic Tab Ordering**: Drag, drop, and choose which datasets to display in which tab sequence for the selected patient.

---

## 📦 How to Copy and Setup the Project

Follow these steps to run the application locally on your machine.

### Prerequisites

You need R installed (version >= 4.1 recommended). Install the required R packages by running the following command in your R console:

```R
install.packages(c("shiny", "dplyr", "purrr", "openxlsx", "bslib", "haven", "shinycssloaders", "fs"))

# Install dtsmartr (ensure you have the local package tarball or CRAN source)
# Example: install.packages("dtsmartr_0.4.1.tar.gz", repos = NULL, type = "source")
```

### Installation Steps

1. **Clone the Repository**:
   ```bash
   git clone https://github.com/wagh-nikhil/clinical-data-explorer.git
   cd clinical-data-explorer
   ```

2. **Generate Dummy Clinical Data**:
   The app scans directories for clinical SAS datasets. We have provided a test data generation script. Run this in your terminal to set up the default clinical paths and mock files:
   ```bash
   Rscript setup_test_data.R
   ```
   *This creates mock SAS datasets in `./data/sdtm/`, `./data/adam/`, and `./data/derived/` representing common domains like ADSL, ADAE, ADLB, DM, AE, etc.*

3. **Run the Shiny Application**:
   Launch the app using the command line:
   ```bash
   Rscript app.R
   ```
   Or open R and run:
   ```R
   shiny::runApp('.')
   ```

---

## 🛠️ Modifying the Excel Configuration File Manually

The generated configuration workbook can be easily edited in Microsoft Excel or Google Sheets to control how the app behaves next time it is uploaded:

1. **Changing Ingestion Paths**: Edit the paths in the `Path` column of the `Data` sheet to point to new dataset directories.
2. **Adding/Removing Datasets from Load**: Toggle the `Loaded` column in the `Data` sheet between `Y` and `N`.
3. **Restricting Cohorts (Global Subsetting)**: Edit the list under `USUBJID` in the `USUBJID` sheet. Any patient ID you add or remove there will instantly limit the loaded datasets to just those rows.
4. **Column Filtering**: Open any dataset tab (like `ADAE`) and add or remove column names to subset the loaded columns.

---

## 📁 Repository Structure

```text
├── R/
│   ├── mod_config.R           # Configuration & Registry tab
│   ├── mod_data_explorer.R    # Global Data Explorer tab
│   ├── mod_patient_profile.R  # Patient Profile Explorer tab
│   └── utils_data.R           # Disk file scan and cohort utilities
├── app.R                      # Main entrypoint script
├── app_ui.R                   # Navigation bar and theme styles
├── app_server.R               # Central reactive state manager
├── global.R                   # Shared setup and library imports
├── setup_test_data.R          # Mock dataset generator
└── .gitignore                 # Protected data and cache files
```
