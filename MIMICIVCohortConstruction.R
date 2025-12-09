############################################################
## 0. Setup
############################################################
library(readr)
library(dplyr)
library(tidyr)

############################################################
## 1. Set data directory + helper
############################################################
data_dir <- "/Users/williamstewart/Downloads/mimic-iv-3.1/hosp"

read_mimic <- function(filename) {
  full_path <- file.path(data_dir, paste0(filename, ".csv.gz"))
  message("Loading: ", filename, " ...")
  read_csv(full_path, progress = TRUE, show_col_types = FALSE)
}

# helper: first non-missing value in a vector
first_non_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) NA else x[1]
}

############################################################
## 2. Load tables
############################################################
message("== Loading tables ==")
admissions    <- read_mimic("admissions")
diagnoses_icd <- read_mimic("diagnoses_icd")
omr           <- read_mimic("omr")
patients      <- read_mimic("patients")
transfers     <- read_mimic("transfers")

############################################################
## 3. Find subjects that are present in *all* sources
############################################################
message("== Finding intersection of subject_id across all sources ==")

common_subjects <- Reduce(
  intersect,
  list(
    unique(admissions$subject_id),
    unique(diagnoses_icd$subject_id),
    unique(omr$subject_id),
    unique(patients$subject_id),
    unique(transfers$subject_id)
  )
)

length_common <- length(common_subjects)
message("Number of subject_id present in ALL tables: ", length_common)

############################################################
## 4. Restrict all tables to common_subjects
############################################################
message("== Restricting all tables to common_subjects ==")

admissions_f    <- admissions    %>% filter(subject_id %in% common_subjects)
diagnoses_icd_f <- diagnoses_icd %>% filter(subject_id %in% common_subjects)
omr_f           <- omr           %>% filter(subject_id %in% common_subjects)
patients_f      <- patients      %>% filter(subject_id %in% common_subjects)
transfers_f     <- transfers     %>% filter(subject_id %in% common_subjects)

############################################################
## 5. CUMULATIVE subject-level diagnoses summary
############################################################
message("== Summarizing Diagnoses table (subject-level, cumulative) ==")

diagnoses_icd_sub <- diagnoses_icd_f %>%
  group_by(subject_id) %>%
  summarize(
    n_diagnoses    = n(),
    n_distinct_icd = n_distinct(icd_code),
    .groups = "drop"
  )

############################################################
## 6. CUMULATIVE subject-level transfers summary
############################################################
message("== Summarizing Transfers table (subject-level, cumulative) ==")

icu_pattern <- "ICU"   # careunit names contain "ICU" somewhere

transfers_subject <- transfers_f %>%
  mutate(
    is_icu = grepl(icu_pattern, careunit, ignore.case = TRUE)
  ) %>%
  group_by(subject_id) %>%
  summarize(
    n_hadm             = n_distinct(hadm_id),
    n_transfers        = n(),
    first_intime       = suppressWarnings(min(intime, na.rm = TRUE)),
    last_outtime       = suppressWarnings(max(outtime, na.rm = TRUE)),
    n_icu_transfers    = sum(is_icu, na.rm = TRUE),
    n_icu_hadm         = n_distinct(hadm_id[is_icu]),
    ever_icu_any_visit = any(is_icu),
    .groups = "drop"
  )

############################################################
## 7. CUMULATIVE subject-level demographics
############################################################
message("== Collapsing demographics to subject-level (cumulative) ==")

demographics <- admissions_f %>%
  group_by(subject_id) %>%
  summarize(
    race           = first_non_na(race),
    language       = first_non_na(language),
    marital_status = first_non_na(marital_status),
    insurance      = first_non_na(insurance),
    .groups = "drop"
  )

patients_min <- patients_f %>%
  select(
    subject_id,
    gender,
    anchor_age,
    anchor_year,
    anchor_year_group
  )

############################################################
## 8. OMR: standardize variable names, summarize over ALL visits
##    (cumulative per subject across all visits)
############################################################
message("== Processing OMR cumulatively over all visits ==")

# Keep the vitals you care about (no eGFR)
keep_results <- c(
  "Blood Pressure", "Weight (Lbs)", "BMI (kg/m2)", "Height (Inches)",
  "Blood Pressure Sitting", "Blood Pressure Standing (1 min)",
  "BMI", "Weight", "Height", "Blood Pressure Lying",
  "Blood Pressure Standing (3 mins)", "Blood Pressure Standing"
)

omr_small <- omr_f %>%
  filter(result_name %in% keep_results) %>%
  mutate(
    # Map all variant names onto a small set of standard variable names
    var_std = dplyr::case_when(
      result_name %in% c("BMI", "BMI (kg/m2)") ~ "BMI",
      result_name %in% c("Height", "Height (Inches)") ~ "Height",
      result_name %in% c("Weight", "Weight (Lbs)") ~ "Weight_Lbs",
      result_name %in% c(
        "Blood Pressure",
        "Blood Pressure Sitting",
        "Blood Pressure Standing (1 min)",
        "Blood Pressure Standing (3 mins)",
        "Blood Pressure Standing",
        "Blood Pressure Lying"
      ) ~ "BP",
      TRUE ~ result_name
    )
  )

# CUMULATIVE vital summary per subject across ALL visits
# Here we use the LAST available measurement overall;
# you can change to mean/min/max if preferred.
vitals_cum <- omr_small %>%
  arrange(subject_id, chartdate, seq_num) %>%
  group_by(subject_id, var_std) %>%
  summarize(
    value_last = dplyr::last(result_value),
    .groups    = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = var_std,         # BMI, Height, Weight_Lbs, BP
    values_from = value_last
  )

# Optional: also cumulative counts of each vital type per subject
vital_counts_cum <- omr_small %>%
  group_by(subject_id, var_std) %>%
  summarize(
    n_meas = n(),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from  = var_std,         # BMI, Height, Weight_Lbs, BP
    values_from = n_meas,
    names_prefix = "n_"
  )
# Gives n_BMI, n_Height, n_Weight_Lbs, n_BP per subject (optional)

############################################################
## 9. Build cumulative one-row-per-subject dataset
##    ONLY for subjects present in ALL sources
############################################################
message("== Merging into cumulative subject-level dataset (all sources) ==")

subject_level_all <- patients_min %>%
  left_join(demographics,      by = "subject_id") %>%
  left_join(diagnoses_icd_sub, by = "subject_id") %>%
  left_join(transfers_subject, by = "subject_id") %>%
  left_join(vitals_cum,        by = "subject_id") %>%
  left_join(vital_counts_cum,  by = "subject_id")   # optional counts

# Clean up vitals: rename Weight_Lbs -> Weight so there is one clean weight column
subject_level_all <- subject_level_all %>%
  rename(Weight = Weight_Lbs)

############################################################
## 10. Final check
############################################################
message("== Final cumulative subject-level dataset (all sources) summary ==")
cat("Rows (subjects): ", nrow(subject_level_all), "\n")
cat("Distinct subject_id: ", n_distinct(subject_level_all$subject_id), "\n")
cat("Expected (intersection size): ", length_common, "\n\n")

cat("Proportion non-missing BMI:\n")
print(mean(!is.na(subject_level_all$BMI)))

cat("\nProportion non-missing Height:\n")
print(mean(!is.na(subject_level_all$Height)))

cat("\nProportion non-missing Weight:\n")
print(mean(!is.na(subject_level_all$Weight)))

cat("\nProportion non-missing BP:\n")
print(mean(!is.na(subject_level_all$BP)))

glimpse(subject_level_all)

############################################################
## 11. Save dataset
############################################################
message("== Saving subject-level dataset ==")

# Save to current working directory
write_csv(subject_level_all, "subject_level_FC.csv")

# If you prefer to save inside the MIMIC hosp folder, you could instead do:
# write_csv(subject_level_all, file.path(data_dir, "subject_level_all.csv"))
# saveRDS(subject_level_all, file.path(data_dir, "subject_level_all.rds"))
