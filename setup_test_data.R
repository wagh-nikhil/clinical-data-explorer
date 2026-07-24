# setup_test_data.R
# Pre-requisite script to generate local clinical SAS datasets for testing.

library(pharmaversesdtm)
library(pharmaverseadam)
library(haven)
library(fs)
library(dplyr)

cat("Starting ClinDataExplorer test data generation...\n")

# Create standard local directories within the project
sdtm_dir <- "./data/sdtm"
adam_dir <- "./data/adam"
derived_dir <- "./data/derived"

dir_create(sdtm_dir)
dir_create(adam_dir)
dir_create(derived_dir)

cat("Created directories:\n - ", sdtm_dir, "\n - ", adam_dir, "\n - ", derived_dir, "\n")

# Core SDTM datasets from pharmaversesdtm
# Note: pharmaversesdtm stores them in lowercase variables inside the package environment.
# We will write them as uppercase filenames to look like standard SAS datasets.
sdtm_maps <- list(
  DM = pharmaversesdtm::dm,
  AE = pharmaversesdtm::ae,
  LB = pharmaversesdtm::lb,
  VS = pharmaversesdtm::vs,
  CM = pharmaversesdtm::cm,
  MH = pharmaversesdtm::mh
)

cat("Writing SDTM datasets...\n")
for (name in names(sdtm_maps)) {
  file_path <- file.path(sdtm_dir, paste0(name, ".sas7bdat"))
  df <- sdtm_maps[[name]]
  
  # Standardize all column names to uppercase just in case
  colnames(df) <- toupper(colnames(df))
  
  write_sas(df, file_path)
  cat("  Written:", file_path, "(", nrow(df), "rows,", ncol(df), "cols)\n")
}

# Core ADaM datasets from pharmaverseadam
adam_maps <- list(
  ADSL = pharmaverseadam::adsl,
  ADAE = pharmaverseadam::adae,
  ADLB = pharmaverseadam::adlb,
  ADVS = pharmaverseadam::advs,
  ADCM = pharmaverseadam::adcm
)

cat("Writing ADaM datasets...\n")
for (name in names(adam_maps)) {
  file_path <- file.path(adam_dir, paste0(name, ".sas7bdat"))
  df <- adam_maps[[name]]
  
  # Standardize all column names to uppercase just in case
  colnames(df) <- toupper(colnames(df))
  
  write_sas(df, file_path)
  cat("  Written:", file_path, "(", nrow(df), "rows,", ncol(df), "cols)\n")
}

# Create a sample derived dataset in ./data/derived
# For example, merge ADSL with ADAE to create a summary of AE counts per subject
cat("Creating and writing derived dataset...\n")
adsl_df <- pharmaverseadam::adsl
adae_df <- pharmaverseadam::adae

derived_df <- adae_df %>%
  group_by(USUBJID) %>%
  summarize(
    AE_COUNT = n(),
    SERIOUS_AE_COUNT = sum(AESER == "Y" | AESER == "Y", na.rm = TRUE),
    SEV_AE_COUNT = sum(AESEV == "SEVERE", na.rm = TRUE)
  ) %>%
  left_join(
    adsl_df %>% select(USUBJID, SUBJID, ARM, SEX, AGE),
    by = "USUBJID"
  )

# Add standard uppercase names
colnames(derived_df) <- toupper(colnames(derived_df))

derived_path <- file.path(derived_dir, "ADAE_SUMM.sas7bdat")
write_sas(derived_df, derived_path)
cat("  Written derived dataset:", derived_path, "(", nrow(derived_df), "rows,", ncol(derived_df), "cols)\n")

cat("Test data generation complete!\n")
