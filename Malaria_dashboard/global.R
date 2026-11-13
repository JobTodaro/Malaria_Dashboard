# ================================================================
# GLOBAL.R
# Malaria OPD Surveillance Dashboard
# ================================================================

# ------------------------------------------------
# Packages
# ------------------------------------------------

suppressPackageStartupMessages({
  
  library(shiny)
  library(shinydashboard)
  library(shinymanager)
  library(bslib)
  library(bsicons)
  
  
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  
  library(readxl)
  library(openxlsx)
  library(lubridate)
  library(haven)
  
  library(DT)
  library(plotly)
  library(ggplot2)
  library(scales)
  library(tidytext)
  library(leaflet)
  library(sf)
  library(htmltools)
  
  library(gtsummary)
  library(gt)
  
  library(REDCapR)
  library(data.table)
  
})

startup_time <- Sys.time()

startup_log <- function(step) {
  cat(
    "\n[STARTUP]",
    format(Sys.time(), "%H:%M:%S"),
    "-",
    step,
    "- elapsed:",
    round(
      as.numeric(
        difftime(Sys.time(), startup_time, units = "secs")
      ),
      1
    ),
    "seconds\n"
  )
}

# ------------------------------------------------
# File paths
# ------------------------------------------------

redcap_token_file <- file.path(
  getwd(),
  "secure_data",
  "redcap_token.csv"
)

old_siaya_file <- file.path(
  getwd(),
  "secure_data",
  "GSK_Health_Facility_Surveilance_clean_(09052026).xlsx"
)

# Check that files exist reading them
stopifnot(file.exists(redcap_token_file))
stopifnot(file.exists(old_siaya_file))

# Read REDCap credentials
redcap_credentials <- read.csv(
  redcap_token_file,
  stringsAsFactors = FALSE
)

# Read old Siaya data
old_siaya_data <- readxl::read_excel(old_siaya_file)

# ==================================
# SIAYA REDCAP DATA
# ==================================
startup_log("Starting Siaya REDCap")

credential_siaya <- REDCapR::retrieve_credential_local(
  path_credential = redcap_token_file,
  project_id = 180
)

gsk_data_siaya <- REDCapR::redcap_read(
  redcap_uri = credential_siaya$redcap_uri,
  token = credential_siaya$token,
  raw_or_label = "label",
  export_survey_fields = TRUE,
  export_checkbox_label = TRUE
)

opd_new <- gsk_data_siaya$data |>
  select(-c(125:135)) |> 
  mutate(
    age_months = as.numeric(age_months)
  )

startup_log("Finished Siaya REDCap")

startup_log("Starting old Siaya Excel")
opd_old <- readxl::read_excel(
  path = old_siaya_file,
  sheet = "Sheet1",
  guess_max = Inf
) |> 
  mutate(
    bp_diastolic = as.character(bp_diastolic),
    height = as.character(height), 
    age_months =  as.numeric(age_months)
  )

startup_log("Finished old Siaya Excel")

opd <- bind_rows(opd_old, opd_new)

# ------------------------------------------------
# Siaya facility details
# ------------------------------------------------

opd <- opd |>
  mutate(
    health_facility = case_when(
      record_id == 1 ~ "Naya Health Centre",
      record_id %in% c(2, 7) ~ "Mahaya Health Centre",
      record_id == 3 ~ "Manyuanda Health Centre",
      record_id == 4 ~ "Misori Dispensary",
      record_id == 5 ~ "Abidha Health Centre",
      record_id == 6 ~ "Saradidi Dispensary",
      TRUE ~ as.character(health_facility)
    ),
    
    kmhfl_id = case_when(
      record_id == 1 ~ 13837,
      record_id %in% c(2, 7) ~ 13757,
      record_id == 3 ~ 13771,
      record_id == 4 ~ 13815,
      record_id == 5 ~ 13461,
      record_id == 6 ~ 14068,
      TRUE ~ as.numeric(kmhfl_id)
    ),
    
    facility_level = case_when(
      record_id %in% c(1, 2, 3, 5, 6, 7) ~ "level 3",
      record_id == 4 ~ "level 2",
      TRUE ~ as.character(facility_level)
    ),
    
    hf_county = "Siaya",
    hf_sub_county = "Rarieda"
  ) |>
  rename(visit_date = date_of_tt) |>
  filter(redcap_event_name != "administrative_arm_1") |>
  select(-any_of(c("facility_details_complete", "dov")))

# -------------------------------------
# Siaya age variables
# -------------------------------------

opd <- opd |>
  mutate(
    across(any_of(c("age_years_u5", "age_years_o5")), as.numeric),
    
    age_years = case_when(
      age_group == "Under 5 years of age" & !is.na(age_years_u5) ~ age_years_u5,
      age_group == "Over 5 years of age" & !is.na(age_years_o5) ~ age_years_o5,
      TRUE ~ NA_real_
    ),
    age_years = case_when(
      is.na(age_years)& age_group=="Under 5 years of age" ~ 0, 
      TRUE ~ age_years
    ) 
  ) |>
  filter(visit_date > as.Date("2026-01-31"))

# ---------------------------------------
# Siaya labels
# ---------------------------------------

opd <- opd |>
  mutate(
    visit_type = case_when(
      visit_type == "1" ~ "New",
      visit_type == "2" ~ "Revisit",
      TRUE ~ as.character(visit_type)
    ),
    
    age_group = case_when(
      age_group == "Over 5 years of age" ~ "Over 5",
      age_group == "Under 5 years of age" ~ "Under 5",
      TRUE ~ as.character(age_group)
    ),
    
    sex = case_when(
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      TRUE ~ as.character(sex)
    ),
    
    county = case_when(
      county == "37" ~ "Kakamega",
      county == "38" ~ "Vihiga",
      county == "39" ~ "Bungoma",
      county == "40" ~ "Busia",
      county == "41" ~ "Siaya",
      county == "42" ~ "Kisumu",
      county == "43" ~ "Homa Bay",
      county == "44" ~ "Migori",
      county == "45" ~ "Others",
      TRUE ~ as.character(county)
    ),
    
    subcounty_siaya = case_when(
      subcounty_siaya == "4101" ~ "4101 ALEGO USONGA",
      subcounty_siaya == "4102" ~ "4102 GEM",
      subcounty_siaya == "4103" ~ "4103 UGENYA",
      subcounty_siaya == "4104" ~ "4104 UGUNJA",
      subcounty_siaya == "4105" ~ "4105 BONDO",
      subcounty_siaya == "4106" ~ "4106 RARIEDA",
      TRUE ~ as.character(subcounty_siaya)
    ),
    
    referred_from = case_when(
      referred_from == "1" ~ "Community Unit",
      referred_from == "2" ~ "From another health facility",
      referred_from == "3" ~ "Within health facility",
      referred_from == "4" ~ "N/A (Not referred)",
      TRUE ~ as.character(referred_from)
    ),
    
    fever_48h = case_when(
      fever_48h == "1" ~ "Yes",
      fever_48h == "0" ~ "No",
      TRUE ~ as.character(fever_48h)
    ),
    
    mal_symptoms = case_when(
      mal_symptoms == "1" ~ "Yes",
      mal_symptoms == "0" ~ "No",
      TRUE ~ as.character(mal_symptoms)
    ),
    
    sus_malaria = case_when(
      sus_malaria == "1" ~ "Not tested",
      sus_malaria == "2" ~ "RDT Tested (-ve)",
      sus_malaria == "3" ~ "Microscopy Tested (-ve)",
      sus_malaria == "4" ~ "RDT Tested (+ve)",
      sus_malaria == "5" ~ "Microscopy Tested (+ve)",
      TRUE ~ as.character(sus_malaria)
    )
  )

# ------------------------------------------------
# Siaya combined location variables
# ------------------------------------------------

subcounty_cols <- grep("^subcounty_", names(opd), value = TRUE)
sublocation_cols <- grep("^sublocation_", names(opd), value = TRUE)
village_cols <- grep("^village_", names(opd), value = TRUE)
other_vill_cols <- grep("^ov_", names(opd), value = TRUE)

opd <- opd |>
  mutate(
    across(
      any_of(c(
        subcounty_cols,
        sublocation_cols,
        village_cols,
        other_vill_cols
      )),
      as.character
    )
  )

if (length(subcounty_cols) > 0) {
  opd$subcounty <- do.call(coalesce, opd[subcounty_cols])
}

if (length(sublocation_cols) > 0) {
  opd$sublocation <- do.call(coalesce, opd[sublocation_cols])
}

if (length(village_cols) > 0) {
  opd$village <- do.call(coalesce, opd[village_cols])
}

if (length(other_vill_cols) > 0) {
  opd$other_village <- do.call(coalesce, opd[other_vill_cols])
}

# ------------------------------------------------
# Siaya diagnosis variables
# ------------------------------------------------

opd_labeled <- opd |>
  mutate(
    diagnosis1 = case_when(
      age_group == "Under 5" ~ as.character(diagnosis_u5_1),
      age_group == "Over 5" ~ as.character(diagnosis_o5_1),
      TRUE ~ NA_character_
    ),
    
    diagnosis2 = case_when(
      age_group == "Under 5" ~ as.character(diagnosis_u5_2),
      age_group == "Over 5" ~ as.character(diagnosis_o5_2),
      TRUE ~ NA_character_
    ),
    
    other_diagnosis1 = case_when(
      age_group == "Under 5" ~ as.character(other_specify_diagnosis_u51),
      age_group == "Over 5" ~ as.character(other_specify_diagnosis_o5_1),
      TRUE ~ NA_character_
    ),
    
    other_diagnosis2 = case_when(
      age_group == "Under 5" ~ as.character(other_specify_diagnosis_u5_2),
      age_group == "Over 5" ~ as.character(other_specify_diagnosis_o5_2),
      TRUE ~ NA_character_
    )
  ) |>
  select(
    -any_of(c(
      "diagnosis_u5_1",
      "other_specify_diagnosis_u51",
      "diagnosis_u5_2",
      "other_specify_diagnosis_u5_2",
      "diagnosis_o5_1",
      "other_specify_diagnosis_o5_1",
      "diagnosis_o5_2",
      "other_specify_diagnosis_o5_2"
    ))
  )

# ------------------------------------------------
# Siaya malaria treatment variables
# ------------------------------------------------

opd_labeled <- opd_labeled %>%
  mutate(
    danger_signs = if_else(
      if_any(
        matches("^danger_signs___[1-5]$"),
        ~ !is.na(.) & . != "Unchecked"
      ),
      "Yes",
      "No"
    )
  )

opd_labeled <- opd_labeled |>
  mutate(
    across(
      starts_with("mal_treatment___"),
      ~ case_when(
        as.character(.x) %in% c(
          "Checked", "1", "Quinine (oral)", "Quinine (Paren)",
          "Artesunate", "AL", "None", "N/A", "Other"
        ) ~ "1",
        as.character(.x) %in% c("Unchecked", "0", "") ~ "0",
        TRUE ~ NA_character_
      )
    )
  ) |>
  rename(
    tx_none = mal_treatment___6,
    tx_na = mal_treatment___7,
    other_mal_tx = mal_treatment_other
  ) |>
  mutate(
    yy = format(as_date(visit_date), "%Y"),
    mm = format(as_date(visit_date), "%m"),
    
    test_rdt = ifelse(
      sus_malaria %in% c("RDT Tested (-ve)", "RDT Tested (+ve)"),
      1, 0
    ),
    
    test_bs = ifelse(
      sus_malaria %in% c("Microscopy Tested (-ve)", "Microscopy Tested (+ve)"),
      1, 0
    ),
    
    temp2 = ifelse(temp > 43 | temp < 33, NA, temp),
    
    febrile = data.table::fcase(
      temp2 >= 37.5 | fever_48h == "Yes", "Yes",
      is.na(temp2) & is.na(fever_48h), NA_character_,
      default = "No"
    ),
    
    malaria_symptomatic = ifelse(
      febrile == "Yes" | mal_symptoms == "Yes" | danger_signs=="Yes",
      1, 0
    ),
    
    tested = ifelse(test_rdt == 1 | test_bs == 1, 1, 0),
    
    mal_result =  case_when(
      sus_malaria %in% c("RDT Tested (+ve)","Microscopy Tested (+ve)") ~ "Positive", 
      sus_malaria %in% c("Microscopy Tested (-ve)", "RDT Tested (-ve)") ~ "Negative", 
      TRUE ~ NA_character_
    ), 
    
    positive = ifelse(
      sus_malaria %in% c(
        "RDT Tested (+ve)",
        "Microscopy Tested (+ve)"
      ),
      1, 0
    )
  ) |>
  mutate(
    across(
      starts_with("mal_treatment___"),
      ~ case_when(
        as.character(.x) %in% c("Checked", "Yes", "1") ~ 1,
        as.character(.x) %in% c("Unchecked", "No", "0") ~ 0,
        TRUE ~ NA_real_
      )
    ),
    
    totaltx = rowSums(
      across(starts_with("mal_treatment___")),
      na.rm = TRUE
    ),
    
    treatment_category = case_when(
      totaltx >= 1 & positive == 1 ~ 1,
      totaltx == 0 & positive == 0 ~ 2,
      totaltx == 0 & positive == 1 ~ 3,
      totaltx >= 1 & positive == 0 ~ 4,
      TRUE ~ NA_real_
    )
  ) |>
  rename(
    quinine_oral = mal_treatment___1,
    quinine_paren = mal_treatment___2,
    artesunate = mal_treatment___3,
    dha_ppq = mal_treatment___4,
    al = mal_treatment___5,
    other_tx = mal_treatment___8
  )


# ========================================
# KAKAMEGA REDCAP DATA
# ========================================

credential_kk <- REDCapR::retrieve_credential_local(
  path_credential = redcap_token_file,
  project_id = 184
)
startup_log("Starting Kakamega REDCap")

gsk_data_kk <- REDCapR::redcap_read(
  redcap_uri = credential_kk$redcap_uri,
  token = credential_kk$token,
  raw_or_label = "label",
  export_survey_fields = TRUE,
  export_checkbox_label = TRUE
)

startup_log("Finished Kakamega REDCap")


opd_kk <- gsk_data_kk$data |>
  mutate(
    health_facility = as.character(record_id),
    
    facility_level = "level 3",
    
    kmhfl_id = case_when(
      record_id == "Bukaya HC" ~ 15817,
      record_id == "Bungasi HC" ~ 15827,
      record_id == "Mumias Model HC" ~ 16035,
      record_id == "Eshimukoko HC" ~ 16114,
      record_id == "Mabole HC" ~ 17297,
      record_id == "Shitsitswi HC" ~ 16121,
      TRUE ~ NA_real_
    ),
    
    hf_county = "Kakamega",
    
    hf_sub_county = case_when(
      record_id %in% c(
        "Bukaya HC",
        "Bungasi HC",
        "Mumias Model HC"
      ) ~ "Mumias West",
      
      record_id %in% c(
        "Eshimukoko HC",
        "Mabole HC",
        "Shitsitswi HC"
      ) ~ "Butere",
      
      TRUE ~ NA_character_
    )
  ) |>
  rename(visit_date = date_of_tt) |>
  filter(redcap_event_name != "Administrative") |>
  select(-any_of(c("facility_details_complete", "dov")))|>
  filter(visit_date > as.Date("2026-05-31"))


# ------------------------------------------------
# Kakamega age and labels
# ------------------------------------------------

opd_kk <- opd_kk |>
  mutate(
    across(any_of(c("age_years_u5", "age_years_o5")), as.numeric),
    
    age_years = case_when(
      age_group == "Under 5 years of age" ~ age_years_u5,
      age_group == "Over 5 years of age" ~ age_years_o5,
      TRUE ~ NA_real_),
    
    age_years = case_when(
      is.na(age_years)& age_group=="Under 5 years of age" ~ 0, 
      TRUE ~ age_years
    ),
    
    visit_type = case_when(
      visit_type == "1" ~ "New",
      visit_type == "2" ~ "Revisit",
      TRUE ~ as.character(visit_type)
    ),
    
    age_group = case_when(
      age_group == "Under 5 years of age" ~ "Under 5",
      age_group == "Over 5 years of age" ~ "Over 5",
      TRUE ~ as.character(age_group)
    ),
    
    sex = case_when(
      sex == "M" ~ "Male",
      sex == "F" ~ "Female",
      TRUE ~ as.character(sex)
    ),
    
    fever_48h = case_when(
      fever_48h == "1" ~ "Yes",
      fever_48h == "0" ~ "No",
      TRUE ~ as.character(fever_48h)
    ),
    
    mal_symptoms = case_when(
      mal_symptoms == "1" ~ "Yes",
      mal_symptoms == "0" ~ "No",
      TRUE ~ as.character(mal_symptoms)
    ),
    
    sus_malaria = case_when(
      sus_malaria == "1" ~ "Not tested",
      sus_malaria == "2" ~ "RDT Tested (-ve)",
      sus_malaria == "3" ~ "Microscopy Tested (-ve)",
      sus_malaria == "4" ~ "RDT Tested (+ve)",
      sus_malaria == "5" ~ "Microscopy Tested (+ve)",
      TRUE ~ as.character(sus_malaria)
    )
  )

# ------------------------------------------------
# Kakamega locations
# ------------------------------------------------
kk_village_cols <- names(opd_kk)[grepl("hf\\d+$", names(opd_kk))]
kk_other_cols   <- names(opd_kk)[grepl("hf\\d+_other$", names(opd_kk))]

# Ensure all candidate location fields have a consistent character type.
location_cols <- unique(c(kk_village_cols, kk_other_cols))

opd_kk <- opd_kk |>
  mutate(
    across(any_of(location_cols), as.character)
  )

# Combine the first non-missing value across the relevant columns.
coalesce_location <- function(data, cols) {
  cols <- intersect(cols, names(data))
  
  if (length(cols) == 0L) {
    return(rep(NA_character_, nrow(data)))
  }
  
  do.call(dplyr::coalesce, unname(data[cols]))
}

kk_village <- coalesce_location(opd_kk, kk_village_cols)
kk_other   <- coalesce_location(opd_kk, kk_other_cols)

opd_kk <- opd_kk |>
  mutate(
    village_kakamega_butere = if_else(
      subcounty_kakamega == "Butere", kk_village, NA_character_
    ),
    ov_kakamega_butere = if_else(
      subcounty_kakamega == "Butere", kk_other, NA_character_
    ),
    village_kakamega_mumias_west = if_else(
      subcounty_kakamega == "Mumias West", kk_village, NA_character_
    ),
    ov_kakamega_mumias_west = if_else(
      subcounty_kakamega == "Mumias West", kk_other, NA_character_
    ),
    village_kakamega_mumias_east = if_else(
      subcounty_kakamega == "Mumias East", kk_village, NA_character_
    ),
    ov_kakamega_mumias_east = if_else(
      subcounty_kakamega == "Mumias East", kk_other, NA_character_
    ),
    village_kakamega_matungu = if_else(
      subcounty_kakamega == "Matungu", kk_village, NA_character_
    ),
    ov_kakamega_matungu = if_else(
      subcounty_kakamega == "Matungu", kk_other, NA_character_
    )
  )

subcounty_cols_kk <- grep("^subcounty_", names(opd_kk), value = TRUE)
village_cols_kk <- grep("^village_", names(opd_kk), value = TRUE)
other_village_cols_kk <- grep("^ov_", names(opd_kk), value = TRUE)

opd_kk <- opd_kk |>
  mutate(
    across(
      any_of(c(
        subcounty_cols_kk,
        village_cols_kk,
        other_village_cols_kk
      )),
      as.character
    )
  )

if (length(subcounty_cols_kk) > 0) {
  opd_kk$subcounty <- do.call(coalesce, opd_kk[subcounty_cols_kk])
}

if (length(village_cols_kk) > 0) {
  opd_kk$village <- do.call(coalesce, opd_kk[village_cols_kk])
}

if (length(other_village_cols_kk) > 0) {
  opd_kk$other_village <- do.call(coalesce, opd_kk[other_village_cols_kk])
}

# ------------------------------------------------
# Kakamega diagnosis variables
# ------------------------------------------------

kk_opd_labeled <- opd_kk |>
  mutate(
    diagnosis1 = case_when(
      age_group == "Under 5" ~ as.character(diagnosis_u5_1),
      age_group == "Over 5" ~ as.character(diagnosis_o5_1),
      TRUE ~ NA_character_
    ),
    
    diagnosis2 = case_when(
      age_group == "Under 5" ~ as.character(diagnosis_u5_2),
      age_group == "Over 5" ~ as.character(diagnosis_o5_2),
      TRUE ~ NA_character_
    ),
    
    other_diagnosis1 = case_when(
      age_group == "Under 5" ~ as.character(other_specify_diagnosis_u51),
      age_group == "Over 5" ~ as.character(other_specify_diagnosis_o5_1),
      TRUE ~ NA_character_
    ),
    
    other_diagnosis2 = case_when(
      age_group == "Under 5" ~ as.character(other_specify_diagnosis_u5_2),
      age_group == "Over 5" ~ as.character(other_specify_diagnosis_o5_2),
      TRUE ~ NA_character_
    )
  ) |>
  select(
    -any_of(c(
      "diagnosis_u5_1",
      "other_specify_diagnosis_u51",
      "diagnosis_u5_2",
      "other_specify_diagnosis_u5_2",
      "diagnosis_o5_1",
      "other_specify_diagnosis_o5_1",
      "diagnosis_o5_2",
      "other_specify_diagnosis_o5_2"
    ))
  )

kk_opd_labeled<-kk_opd_labeled|>
  mutate(sus_malaria = case_when(
    sus_malaria == "Microscopy tested (-ve)" ~"Microscopy Tested (-ve)",
    sus_malaria == "Microscopy tested (+ve)" ~"Microscopy Tested (+ve)",
    sus_malaria == "RDT tested (-ve)" ~"RDT Tested (-ve)",
    sus_malaria == "RDT tested (+ve)" ~"RDT Tested (+ve)",
    sus_malaria %in% c("N/A") ~ "NOT tested", 
    TRUE ~sus_malaria
    
  ))


# ------------------------------------------------
# Kakamega malaria treatment cascade
# ------------------------------------------------

kk_opd_labeled <- kk_opd_labeled %>%
  mutate(
    danger_signs = if_else(
      if_any(
        matches("^danger_signs___[1-5]$"),
        ~ !is.na(.) & . != "Unchecked"
      ),
      "Yes",
      "No"
    )
  )


kk_opd_labeled <- kk_opd_labeled |>
  mutate(
    across(
      starts_with("mal_treatment___"),
      ~ case_when(
        as.character(.x) %in% c(
          "Checked", "1", "Quinine (oral)", "Quinine (Paren)",
          "Artesunate", "AL", "None", "N/A", "Other"
        ) ~ "1",
        as.character(.x) %in% c("Unchecked", "0", "") ~ "0",
        TRUE ~ NA_character_
      )
    )
  ) |>
  rename(
    tx_none = mal_treatment___6,
    tx_na = mal_treatment___7,
    other_mal_tx = mal_treatment_other
  ) |>
  mutate(
    yy = format(as_date(visit_date), "%Y"),
    mm = format(as_date(visit_date), "%m"),
    
    test_rdt = ifelse(
      sus_malaria %in% c("RDT Tested (-ve)", "RDT Tested (+ve)"),
      1, 0
    ),
    
    test_bs = ifelse(
      sus_malaria %in% c("Microscopy Tested (-ve)", "Microscopy Tested (+ve)"),
      1, 0
    ),
    
    temp2 = ifelse(temp > 43 | temp < 33, NA, temp),
    
    febrile = data.table::fcase(
      temp2 >= 37.5 | fever_48h=="Yes", "Yes" ,
      is.na(temp2), NA_character_,
      default = "No"
    ),
    
    malaria_symptomatic = ifelse(
      febrile == "Yes" | test_rdt == 1 | test_bs == 1, 1, 0),
    
    tested = ifelse(test_rdt == 1 | test_bs == 1, 1, 0),
    
    mal_result =  case_when(
      sus_malaria %in% c("RDT Tested (+ve)","Microscopy Tested (+ve)") ~ "Positive", 
      sus_malaria %in% c("Microscopy Tested (-ve)", "RDT Tested (-ve)") ~ "Negative", 
      TRUE ~ NA_character_
    ), 
    
    positive = ifelse(
      sus_malaria %in% c(
        "RDT Tested (+ve)",
        "Microscopy Tested (+ve)"
      ),
      1, 0
    )
  ) |>
  mutate(
    across(
      starts_with("mal_treatment___"),
      ~ case_when(
        as.character(.x) %in% c("Checked", "Yes", "1") ~ 1,
        as.character(.x) %in% c("Unchecked", "No", "0") ~ 0,
        TRUE ~ NA_real_
      )
    ),
    
    totaltx = rowSums(
      across(starts_with("mal_treatment___")),
      na.rm = TRUE
    ),
    
    treatment_category = case_when(
      totaltx >= 1 & positive == 1 ~ 1,
      totaltx == 0 & positive == 0 ~ 2,
      totaltx == 0 & positive == 1 ~ 3,
      totaltx >= 1 & positive == 0 ~ 4,
      TRUE ~ NA_real_
    )
  ) |>
  rename(
    quinine_oral = mal_treatment___1,
    quinine_paren = mal_treatment___2,
    artesunate = mal_treatment___3,
    dha_ppq = mal_treatment___4,
    al = mal_treatment___5,
    other_tx = mal_treatment___8
  ) |>
  filter(
    !is.na(opd_no),
    opd_no != "",
    opd_no != "NA"
  )


# ================================================================
# MERGE SIAYA AND KAKAMEGA DATA
# ================================================================

# Variables required by the dashboard and data-quality tables
dashboard_vars <- c(
  "record_id",
  "redcap_event_name",
  "redcap_repeat_instrument",
  "redcap_repeat_instance" ,
  "hf_county",
  "hf_sub_county",
  "health_facility",
  "facility_level",
  "kmhfl_id",
  "visit_date",
  "opd_no",
  "opd_conf",
  "visit_type",
  "age_group",
  "age_years",
  "age_months",
  "age_weeks",
  "age_days",
  "sex",
  "county",
  "county_oth",
  "subcounty",
  "sublocation",
  "village",
  "other_village",
  "referred_from",
  "referral",
  "temp",
  "temp2",
  "fever_48h",
  "febrile",
  "mal_symptoms",
  "malaria_symptomatic",
  "sus_malaria",
  "test_rdt",
  "test_bs",
  "tested",
  "positive",
  "mal_result",
  "diagnosis1",
  "diagnosis2",
  "other_diagnosis1",
  "other_diagnosis2",
  "quinine_oral",
  "quinine_paren",
  "artesunate",
  "dha_ppq",
  "al",
  "other_tx",
  "tx_none",
  "tx_na",
  "other_mal_tx",
  "totaltx",
  "treatment_category",
  "yy",
  "mm",
  "danger_signs",
  "danger_signs___1"  ,          
  "danger_signs___2" ,            
  "danger_signs___3" ,           
  "danger_signs___4",             
  "danger_signs___5" ,           
  "danger_signs___6"
)

standardise_opd <- function(data, variables) {
  
  missing_vars <- setdiff(variables, names(data))
  data[missing_vars] <- NA
  
  data |>
    mutate(
      # Make merge-key / identifier variables consistent across counties
      record_id = as.character(record_id),
      
      opd_no = as.character(opd_no),
      opd_conf = as.character(opd_conf),
      kmhfl_id = as.numeric(kmhfl_id),
      
      visit_date = as.Date(visit_date),
      age_years = as.numeric(age_years),
      # age_months = as.numeric(age_months),
      age_weeks = as.numeric(age_weeks),
      age_days = as.numeric(age_days),
      
      temp = as.numeric(temp),
      temp2 = as.numeric(temp2)
    ) |>
    select(all_of(variables))
}

opd_labeled_std <- standardise_opd(
  data = opd_labeled,
  variables = dashboard_vars
)

kk_opd_labeled_std <- standardise_opd(
  data = kk_opd_labeled,
  variables = dashboard_vars
)

# Merge the two standardised datasets
opd_merged <- bind_rows(
  opd_labeled_std,
  kk_opd_labeled_std
) |>
  filter(
    !is.na(opd_no),
    opd_no != "",
    opd_no != "NA"
  ) |>
  mutate(
    suspected_mal = if_else(
      sus_malaria %in% c(
        "RDT Tested (+ve)",
        "Microscopy Tested (+ve)",
        "RDT Tested (-ve)",
        "Microscopy Tested (-ve)"
      ),
      1,
      0,
      missing = 0
    ), 
    
    temp_cat = case_when(
      temp >=37.5 ~ "≥37.5°C", 
      temp <37.5 ~ "<37.5°C", 
      TRUE ~ NA_character_
    ),
    
    temp_cat = factor(temp_cat, 
                      levels = c("≥37.5°C", "<37.5°C"))
  )

opd_merged<-opd_merged |> 
  filter(
    health_facility %in% c("Abidha Health Centre",
                           "Mahaya Health Centre",
                           "Manyuanda Health Centre",
                           "Misori Dispensary",
                           "Naya Health Centre",
                           "Saradidi Dispensary",
                           "Bukaya HC",
                           "Bungasi HC",
                           "Eshimukoko HC",
                           "Mabole HC",
                           "Mumias Model HC",
                           "Shitsitswi HC")
  )

# ================================================================
# DATA QUALITY QUERY TABLES
# ================================================================

missing_location <- opd_merged |>
  select(
    county,
    subcounty,
    village,
    hf_county,
    health_facility,
    opd_no,
    visit_date,
    visit_type,
    age_group
  ) |>
  filter(
    is.na(county) |
      is.na(subcounty) |
      is.na(village)
  )

age_var <- opd_merged |>
  select(
    hf_county,
    health_facility,
    visit_date,
    opd_no,
    age_group,
    sex,
    age_years,
    age_months,
    age_weeks,
    age_days
  ) |>
  filter(
    (age_group == "Over 5" & is.na(age_years)) |
      (
        age_group == "Under 5" &
          is.na(age_years) &
          is.na(age_months) &
          is.na(age_weeks) &
          is.na(age_days)
      ) |
      is.na(visit_date) |
      age_months > 11 |
      age_weeks > 3 |
      age_days > 6 |
      is.na(sex)
  )

temp <- opd_merged |>
  select(
    hf_county,
    health_facility,
    opd_no,
    visit_date,
    age_group,
    temp,
    fever_48h,
    mal_symptoms
  ) |>
  filter((temp >= 43 | temp < 33) & !is.na(temp))

missing_mal_ind <- opd_merged |>
  select(
    hf_county,
    health_facility,
    opd_no,
    county,
    subcounty,
    visit_date,
    age_group,
    temp,
    fever_48h,
    sus_malaria
  ) |>
  filter(if_any(c(fever_48h, sus_malaria), is.na))

missing_temp <- opd_merged |>
  select(
    hf_county,
    health_facility,
    opd_no,
    county,
    subcounty,
    visit_date,
    age_group,
    temp,
    fever_48h,
    sus_malaria
  ) |>
  filter(is.na(temp))

missed_testing <- opd_merged |>
  select(
    hf_county,
    health_facility,
    age_group,
    opd_no,
    visit_date,
    fever_48h,
    temp,
    febrile,
    mal_symptoms,
    malaria_symptomatic,
    diagnosis1,
    diagnosis2,
    other_diagnosis1,
    tested
  ) |>
  filter(
    (malaria_symptomatic==1)&
      tested != 1
  )

tested_assymptomatic<-opd_merged|>
  select(    hf_county,
             health_facility,
             age_group,
             opd_no,
             visit_date,
             fever_48h,
             temp,
             febrile,
             mal_symptoms,
             malaria_symptomatic,
             diagnosis1,
             diagnosis2,
             other_diagnosis1,
             tested
  )|>
  filter(
    (malaria_symptomatic==0 & tested==1)
  )

missing_tx <- opd_merged |>
  filter(
    sus_malaria %in% c(
      "RDT Tested (+ve)",
      "Microscopy Tested (+ve)"
    ),
    
    if_all(
      c(
        quinine_oral,
        quinine_paren,
        artesunate,
        dha_ppq,
        al
      ),
      ~ .x %in% c(0, "0", NA)
    )
  )

negative_treated <- opd_merged |>
  filter(
    if_any(
      c(
        quinine_oral,
        quinine_paren,
        artesunate,
        dha_ppq,
        al,
        other_tx
      ),
      ~ .x %in% c(1, "1")
    ) &
      positive == 0
  )

diag_dups <- opd_merged |>
  select(
    hf_county,
    health_facility,
    visit_date,
    opd_no,
    visit_type,
    age_group,
    diagnosis1,
    diagnosis2,
    other_diagnosis1,
    other_diagnosis2,
    sus_malaria
  ) |>
  mutate(
    across(
      c(diagnosis1, diagnosis2, other_diagnosis1, other_diagnosis2),
      ~ na_if(stringr::str_squish(as.character(.x)), "")
    )
  )|>
  filter(
    (
      !is.na(diagnosis1) &
        !is.na(diagnosis2) &
        diagnosis1 != "" &
        diagnosis2 != "" &
        diagnosis1 == diagnosis2 &
        diagnosis1 != "Other Disease (Specify)"
    ) |
      (
        !is.na(other_diagnosis1) &
          !is.na(other_diagnosis2) &
          other_diagnosis1 != "" &
          other_diagnosis2 != "" &
          other_diagnosis1 == other_diagnosis2
      ) |
      (
        diagnosis1 == "Malaria - Suspected" &
          sus_malaria %in% c(
            "Microscopy Tested (+ve)",
            "RDT Tested (+ve)"
          )
      ) |
      (
        diagnosis2 == "Malaria - Suspected" &
          sus_malaria %in% c(
            "Microscopy Tested (+ve)",
            "RDT Tested (+ve)"
          )
      )
  )

mal_diagnosis <- opd_merged |>
  filter(
    positive == 1,
    if_all(
      c(diagnosis1, diagnosis2),
      ~ !.x %in% c(
        "Malaria - Confirmed",
        "Malaria - in Pregnancy",
        "Malaria - Suspected"
      )
    )
  )

duplicates <- opd_merged |>
  add_count(
    health_facility,
    opd_no,
    visit_date,
    visit_type,
    age_group,
    # age_years,
    # age_months,
    # sex,
    name = "dupe_n"
  ) |>
  filter(dupe_n > 1)

opd_check <- opd_merged |>
  select(
    health_facility,
    opd_no,
    opd_conf,
    visit_date,
    age_group
  ) |>
  filter(
    !is.na(opd_no),
    !is.na(opd_conf),
    opd_no != opd_conf
  )

#incorrect event
month_lookup <- tibble(
  redcap_event_name = c(
    "Jan 2026", "Feb 2026", "Mar 2026", "Apr 2026",
    "May 2026", "June 2026", "July 2026", "Aug 2026",
    "Sep 2026", "Oct 2026", "Nov 2026", "Dec 2026"
  ),
  month_num = 1:12
) |>
  mutate(
    start_date = make_date(2026, month_num, 1),
    end_date = start_date %m+% months(1)
  ) |>
  select(-month_num)

incorrect_event <- opd_merged |>
  select(record_id,redcap_event_name,redcap_repeat_instrument,redcap_repeat_instance, 
         health_facility, opd_no, visit_date,
         age_group, age_years, age_months, sex, visit_type) |>
  mutate(visit_date = as.Date(visit_date)) |>
  left_join(month_lookup, by = "redcap_event_name") |>
  filter(!is.na(start_date)) |>
  # keep rows where date is missing or outside the month window
  filter(is.na(visit_date) | visit_date < start_date | visit_date >= end_date)






#-----------------------------------
# Daily summary 
#----------------------------

#Daily dataset
# 
opd_daily <- opd_labeled |>
  mutate(
    visit_date = as.Date(visit_date),
    age_group = factor(age_group, levels = c("Under 5", "Over 5")),
    month = floor_date(visit_date, "month")
  ) |>
  filter(visit_date > as.Date("2026-01-18")) |>
  count(health_facility, age_group, month, visit_date,
        name = "total_attendance") |>
  group_by(health_facility, age_group, month) |>
  group_modify(~{
    tibble(
      visit_date = seq.Date(
        .y$month,
        ceiling_date(.y$month, "month") - days(1),
        by = "day"
      )
    ) |>
      left_join(.x, by = "visit_date")
  }) |>
  mutate(
    total_attendance = coalesce(total_attendance, 0L),
    source = "REDCap"
  ) |>
  ungroup()

# ---- Split by month ----
monthly_list <- opd_daily |>
  group_by(month) |>
  group_split()

month_names <- opd_daily |>
  distinct(month) |>
  arrange(month) |>
  pull(month) |>
  format("%b_%Y")

# ---- Create Excel workbook ----
wb <- createWorkbook()

for(i in seq_along(monthly_list)){

  df <- monthly_list[[i]] |>
    select(health_facility, age_group, source, visit_date, total_attendance) |>
    pivot_wider(
      names_from = visit_date,
      values_from = total_attendance
    ) |>
    mutate(row_label = paste(age_group, source)) |>
    arrange(health_facility, age_group)

  addWorksheet(wb, month_names[i])

  writeData(wb, month_names[i], df)

  freezePane(wb, month_names[i], firstRow = TRUE, firstCol = TRUE)

  setColWidths(wb, month_names[i], cols = 1:ncol(df), widths = "auto")
}

saveWorkbook(wb, "data/Daily_Summary_By_Month.xlsx", overwrite = TRUE)


# ---- Prepare kakamega opd_daily ----

kk_opd_daily <- kk_opd_labeled |>
  mutate(
    visit_date = as.Date(visit_date),
    age_group = factor(age_group, levels = c("Under 5", "Over 5")),
    month = floor_date(visit_date, "month")
  ) |>
  filter(visit_date > as.Date("2026-01-18")) |>
  count(health_facility, age_group, month, visit_date,
        name = "total_attendance") |>
  group_by(health_facility, age_group, month) |>
  group_modify(~{
    tibble(
      visit_date = seq.Date(
        .y$month,
        ceiling_date(.y$month, "month") - days(1),
        by = "day"
      )
    ) |>
      left_join(.x, by = "visit_date")
  }) |>
  mutate(
    total_attendance = coalesce(total_attendance, 0L),
    source = "REDCap"
  ) |>
  ungroup()

# ---- Split by month ----
monthly_list <- kk_opd_daily |>
  group_by(month) |>
  group_split()

month_names <- kk_opd_daily |>
  distinct(month) |>
  arrange(month) |>
  pull(month) |>
  format("%b_%Y")

# ---- Create Excel workbook ----
wb <- createWorkbook()

for(i in seq_along(monthly_list)){

  df <- monthly_list[[i]] |>
    select(health_facility, age_group, source, visit_date, total_attendance) |>
    pivot_wider(
      names_from = visit_date,
      values_from = total_attendance
    ) |>
    mutate(row_label = paste(age_group, source)) |>
    arrange(health_facility, age_group)

  addWorksheet(wb, month_names[i])

  writeData(wb, month_names[i], df)

  freezePane(wb, month_names[i], firstRow = TRUE, firstCol = TRUE)

  setColWidths(wb, month_names[i], cols = 1:ncol(df), widths = "auto")
}

saveWorkbook(wb, "data/Kakamega_Daily_Summary_By_Month.xlsx", overwrite = TRUE)


# 
# # ================================================================
# # DAILY SUMMARY DATA
# # ================================================================
# 
# safe_header_date <- function(x) {
#   
#   x <- as.character(x)
#   x <- str_squish(x)
#   
#   if (is.na(x) || x == "" || tolower(x) %in% c("total", "totals")) {
#     return(as.Date(NA))
#   }
#   
#   if (str_detect(x, "^\\d+(\\.\\d+)?$")) {
#     
#     value <- suppressWarnings(as.numeric(x))
#     
#     if (!is.na(value) && value > 20000 && value < 60000) {
#       return(as.Date(value, origin = "1899-12-30"))
#     }
#   }
#   
#   parsed <- suppressWarnings(
#     parse_date_time(
#       x,
#       orders = c(
#         "d-b-Y",
#         "d-b-y",
#         "d/m/Y",
#         "d/m/y",
#         "d_m_Y",
#         "d_m_y",
#         "Y-m-d",
#         "Y_m_d",
#         "dmy",
#         "mdy",
#         "d b Y",
#         "d B Y"
#       ),
#       quiet = TRUE
#     )
#   )
#   
#   as.Date(parsed)
# }
# 
# find_header_row <- function(raw_sheet, max_rows_to_check = 25) {
#   
#   rows_to_check <- seq_len(
#     min(max_rows_to_check, nrow(raw_sheet))
#   )
#   
#   date_counts <- map_int(rows_to_check, function(i) {
#     
#     row_values <- raw_sheet[i, ] |>
#       unlist(use.names = FALSE)
#     
#     header_dates <- vapply(
#       row_values,
#       safe_header_date,
#       as.Date(NA)
#     )
#     
#     sum(!is.na(header_dates))
#   })
#   
#   candidate_rows <- rows_to_check[date_counts >= 3]
#   
#   if (length(candidate_rows) == 0) {
#     return(NA_integer_)
#   }
#   
#   candidate_rows[1]
# }
# 
# clean_attendance <- function(x) {
#   
#   x <- as.character(x)
#   x <- str_squish(x)
#   
#   x <- na_if(x, "")
#   x <- na_if(x, "-")
#   x <- na_if(x, "NA")
#   x <- na_if(x, "N/A")
#   
#   suppressWarnings(as.numeric(x))
# }
# 
# read_daily_summary_workbook <- function(file_path, county_name) {
#   
#   sheet_names <- excel_sheets(file_path)
#   
#   map_dfr(sheet_names, function(sheet) {
#     
#     raw_sheet <- readxl::read_excel(
#       file_path,
#       sheet = sheet,
#       col_names = FALSE,
#       col_types = "text",
#       .name_repair = "minimal"
#     )
#     
#     if (nrow(raw_sheet) == 0 || ncol(raw_sheet) < 4) {
#       return(tibble())
#     }
#     
#     header_row_index <- find_header_row(raw_sheet)
#     
#     if (is.na(header_row_index)) {
#       return(tibble())
#     }
#     
#     header_row <- raw_sheet[header_row_index, ] |>
#       unlist(use.names = FALSE)
#     
#     dat <- raw_sheet[
#       (header_row_index + 1):nrow(raw_sheet),
#       ,
#       drop = FALSE
#     ]
#     
#     keep_cols <- !is.na(header_row) &
#       str_squish(as.character(header_row)) != ""
#     
#     header_row <- header_row[keep_cols]
#     dat <- dat[, keep_cols, drop = FALSE]
#     
#     if (ncol(dat) < 4) {
#       return(tibble())
#     }
#     
#     header_dates <- vapply(
#       header_row,
#       safe_header_date,
#       as.Date(NA)
#     )
#     
#     date_positions <- which(!is.na(header_dates))
#     
#     if (length(date_positions) == 0) {
#       return(tibble())
#     }
#     
#     id_positions <- 1:3
#     keep_positions <- unique(c(id_positions, date_positions))
#     
#     dat <- dat[, keep_positions, drop = FALSE]
#     header_dates <- header_dates[keep_positions]
#     
#     names(dat) <- paste0("col_", seq_len(ncol(dat)))
#     names(dat)[1:3] <- c(
#       "health_facility",
#       "age_group",
#       "source"
#     )
#     
#     date_col_positions <- which(!is.na(header_dates))
#     date_col_positions <- date_col_positions[date_col_positions > 3]
#     
#     if (length(date_col_positions) == 0) {
#       return(tibble())
#     }
#     
#     date_col_names <- names(dat)[date_col_positions]
#     
#     date_lookup <- tibble(
#       date_column = date_col_names,
#       visit_date = header_dates[date_col_positions]
#     )
#     
#     dat |>
#       mutate(
#         across(all_of(date_col_names), as.character),
#         
#         health_facility = str_squish(as.character(health_facility)),
#         age_group = str_squish(as.character(age_group)),
#         source = str_squish(as.character(source))
#       ) |>
#       fill(
#         health_facility,
#         age_group,
#         .direction = "down"
#       ) |>
#       filter(
#         !is.na(health_facility),
#         health_facility != "",
#         !is.na(source),
#         source != ""
#       ) |>
#       mutate(
#         source = case_when(
#           str_to_lower(source) == "redcap" ~ "REDCap",
#           str_to_lower(source) == "register" ~ "Register",
#           TRUE ~ source
#         )
#       ) |>
#       filter(source %in% c("REDCap", "Register")) |>
#       pivot_longer(
#         cols = all_of(date_col_names),
#         names_to = "date_column",
#         values_to = "attendance"
#       ) |>
#       left_join(
#         date_lookup,
#         by = "date_column"
#       ) |>
#       mutate(
#         attendance = clean_attendance(attendance),
#         attendance = replace_na(attendance, 0),
#         hf_county = county_name,
#         month_sheet = sheet
#       ) |>
#       filter(!is.na(visit_date)) |>
#       select(
#         hf_county,
#         health_facility,
#         age_group,
#         source,
#         visit_date,
#         attendance,
#         month_sheet
#       )
#   })
# }
# 
# daily_summary <- bind_rows(
#   read_daily_summary_workbook(
#     file_path = siaya_daily_file,
#     county_name = "Siaya"
#   ),
#   
#   read_daily_summary_workbook(
#     file_path = kakamega_daily_file,
#     county_name = "Kakamega"
#   )
# ) |>
#   mutate(
#     hf_county = str_squish(hf_county),
#     health_facility = str_squish(health_facility),
#     age_group = str_squish(age_group),
#     source = str_squish(source),
#     attendance = replace_na(attendance, 0)
#   ) |>
#   distinct(
#     hf_county,
#     health_facility,
#     age_group,
#     source,
#     visit_date,
#     attendance,
#     month_sheet,
#     .keep_all = TRUE
#   ) |>
#   arrange(
#     hf_county,
#     health_facility,
#     age_group,
#     visit_date,
#     source
#   )