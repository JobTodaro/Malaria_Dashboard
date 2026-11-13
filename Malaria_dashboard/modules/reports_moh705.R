#=========================================================
# MOH 705 MODULE
#=========================================================


#=========================================================
# BUILD DAILY SECTION
#=========================================================

build_daily_section <- function(
    data,
    group_vars,
    label = NULL,
    period_start = NULL,
    period_end = NULL,
    period_type = "monthly"
) {
  
  
  #-------------------------------------------------------
  # Check data
  #-------------------------------------------------------
  
  if (is.null(data) || nrow(data) == 0) {
    return(data.frame())
  }
  
  
  #-------------------------------------------------------
  # Make sure visit_date is Date
  #-------------------------------------------------------
  
  data <- data %>%
    dplyr::mutate(
      visit_date = as.Date(visit_date)
    )
  
  
  #-------------------------------------------------------
  # Determine reporting dates
  #-------------------------------------------------------
  
  period_start <- as.Date(period_start)
  period_end   <- as.Date(period_end)
  
  
  if (
    is.na(period_start) ||
    is.na(period_end)
  ) {
    
    stop(
      "Valid period_start and period_end are required."
    )
    
  }
  
  
  #-------------------------------------------------------
  # Filter to selected reporting period
  #-------------------------------------------------------
  
  data <- data %>%
    
    dplyr::filter(
      
      visit_date >= period_start,
      
      visit_date <= period_end
      
    )
  
  
  if (nrow(data) == 0) {
    return(data.frame())
  }
  
  
  #=======================================================
  # CREATE DATE LABELS
  #=======================================================
  
  reporting_dates <- seq.Date(
    
    from = period_start,
    
    to = period_end,
    
    by = "day"
    
  )
  
  
  #-------------------------------------------------------
  # Monthly report
  #
  # Keep official-style day numbers:
  # 1, 2, 3 ... 31
  #-------------------------------------------------------
  
  if (period_type == "monthly") {
    
    day_labels <- as.character(
      
      lubridate::day(
        reporting_dates
      )
      
    )
    
    data <- data %>%
      
      dplyr::mutate(
        
        day = as.character(
          
          lubridate::day(
            visit_date
          )
          
        )
        
      )
    
  }
  
  
  #-------------------------------------------------------
  # Weekly/custom report
  #
  # Use actual dates to avoid ambiguity across months.
  #-------------------------------------------------------
  
  else {
    
    day_labels <- format(
      
      reporting_dates,
      
      "%d-%b"
      
    )
    
    data <- data %>%
      
      dplyr::mutate(
        
        day = format(
          
          visit_date,
          
          "%d-%b"
          
        )
        
      )
    
  }
  
  
  #=======================================================
  # BUILD REPORT
  #=======================================================
  
  report <- data %>%
    
    tidyr::pivot_longer(
      
      cols = dplyr::all_of(group_vars),
      
      names_to = "variable",
      
      values_to = "category",
      
      values_drop_na = TRUE
      
    ) %>%
    
    dplyr::mutate(
      
      category = stringr::str_squish(
        as.character(category)
      )
      
    ) %>%
    
    dplyr::filter(
      
      category != ""
      
    ) %>%
    
    dplyr::count(
      
      health_facility,
      
      age_group,
      
      category,
      
      day,
      
      name = "n"
      
    ) %>%
    
    tidyr::complete(
      
      health_facility,
      
      age_group,
      
      category,
      
      day = day_labels,
      
      fill = list(
        n = 0
      )
      
    ) %>%
    
    tidyr::pivot_wider(
      
      names_from = day,
      
      values_from = n,
      
      values_fill = 0
      
    )
  
  
  #=======================================================
  # ENSURE ALL SELECTED DATE COLUMNS EXIST
  #=======================================================
  
  missing_columns <- setdiff(
    
    day_labels,
    
    names(report)
    
  )
  
  
  if (length(missing_columns) > 0) {
    
    for (col in missing_columns) {
      
      report[[col]] <- 0
      
    }
    
  }
  
  
  #=======================================================
  # ORDER DATE COLUMNS
  #=======================================================
  
  report <- report %>%
    
    dplyr::select(
      
      health_facility,
      
      age_group,
      
      category,
      
      dplyr::all_of(day_labels)
      
    ) %>%
    
    dplyr::mutate(
      
      Total = rowSums(
        
        dplyr::across(
          
          dplyr::all_of(day_labels)
          
        ),
        
        na.rm = TRUE
        
      ),
      
      Section = label
      
    )
  
  
  return(
    report
  )
  
}


#=========================================================
# MOH 705 REPORT GENERATOR
#=========================================================

reports_moh705 <- function(
    opd_data,
    report_month = NULL,
    report_week = NULL,
    start_date = NULL,
    end_date = NULL,
    period_type = "monthly"
) {
  
  
  #=======================================================
  # DETERMINE REPORTING PERIOD
  #=======================================================
  
  period <- get_reporting_period(
    
    report_month = report_month,
    
    report_week = report_week,
    
    start_date = start_date,
    
    end_date = end_date,
    
    period_type = period_type
    
  )
  
  
  #=======================================================
  # PREPARE DATA
  #=======================================================
  
  opd_data <- prepare_opd_data(
    
    data = opd_data,
    
    start_date = period$start,
    
    end_date = period$end,
    
    period_type = "date_range"
    
  )
  
  
  #-------------------------------------------------------
  # No data
  #-------------------------------------------------------
  
  if (
    is.null(opd_data) ||
    nrow(opd_data) == 0
  ) {
    
    return(
      data.frame()
    )
    
  }
  
  
  #=======================================================
  # BUILD REPORT SECTIONS
  #=======================================================
  
  
  #-------------------------------------------------------
  # DISEASES
  #-------------------------------------------------------
  
  diagnosis_section <- build_daily_section(
    
    data = opd_data,
    
    group_vars = c(
      
      "diagnosis1",
      
      "diagnosis2"
      
    ),
    
    label = "DISEASES",
    
    period_start = period$start,
    
    period_end = period$end,
    
    period_type = period_type
    
  )
  
  
  #-------------------------------------------------------
  # ATTENDANCE
  #-------------------------------------------------------
  
  attendance_section <- build_daily_section(
    
    data = opd_data,
    
    group_vars = "visit_type",
    
    label = "ATTENDANCE",
    
    period_start = period$start,
    
    period_end = period$end,
    
    period_type = period_type
    
  )
  
  
  #-------------------------------------------------------
  # REFERRALS
  #-------------------------------------------------------
  
  referral_section <- build_daily_section(
    
    data = opd_data,
    
    group_vars = c(
      
      "referred_from",
      
      "referral"
      
    ),
    
    label = "REFERRALS",
    
    period_start = period$start,
    
    period_end = period$end,
    
    period_type = period_type
    
  )
  
  
  #=======================================================
  # COMBINE SECTIONS
  #=======================================================
  
  report <- dplyr::bind_rows(
    
    diagnosis_section,
    
    attendance_section,
    
    referral_section
    
  )
  
  
  #-------------------------------------------------------
  # No report
  #-------------------------------------------------------
  
  if (
    is.null(report) ||
    nrow(report) == 0
  ) {
    
    return(
      data.frame()
    )
    
  }
  
  
  #=======================================================
  # MOH 705 OFFICIAL ORDER
  #=======================================================
  
  moh705_order <- c(
    
    #-----------------------------------------------------
    # DISEASES
    #-----------------------------------------------------
    
    "Diarrhoea with no dehydration",
    "Diarrhoea with some dehydration",
    "Diarrhoea with severe dehydration",
    "Cholera",
    "Dysentery (Bloody diarrhoea)",
    "Gastroenterirtis",
    "Pneumonia",
    "Severe pneumonia",
    "Upper Respiratory Tract Infections",
    "Lower Respiratory Tract Infections",
    "Asthma",
    "Presumed Tuberculosis",
    "Suspected Malaria",
    "Tested for Malaria",
    "Confirmed malaria",
    "Ear infection",
    "Malnutrition",
    "Anaemia",
    "Meningococcal Meningitis",
    "Other Meningitis",
    "Neonatal Sepsis",
    "Neonatal Tetanus",
    "Poliomyelitis (AFP)",
    "Chicken Pox",
    "Measles",
    "Hepatitis",
    "Amoebiasis",
    "Mumps",
    "Typhoid fever",
    "Bilharzia (Schistosomiasis)",
    "Intestinal worms",
    "Eye Infections",
    "Tonsilitis",
    "Urinary Tract Infections",
    "Mental Disorders",
    "Dental Disorders",
    "Jiggers Infestation",
    "Diseases of the skin",
    "Down's syndrome",
    "Poisoning",
    "Road Traffic Injuries",
    "Deaths due to Road Traffic Injuries",
    "Violence related injuries",
    "Other injuries",
    "Sexual Violence",
    "Burns",
    "Snake Bites",
    "Dog Bites",
    "Other Bites",
    "Diabetes",
    "Epilepsy",
    "Other Convulsive Disorders",
    "Rheumatic Fever",
    "Brucellosis",
    "Rickets",
    "Cerebral Palsy",
    "Autism",
    "Tryponosomiasis",
    "Yellow Fever",
    "Viral Haemorrhagic Fever",
    "Rift valley fever",
    "Chikungunya",
    "Dengue fever",
    "Leishmaniasis(Kalaazar)",
    "Cutaneous leishmaniasis",
    "Suspected anthrax",
    "Suspected Childhood Cancers",
    "Hypoxaemia (Spo2<90%)",
    "All Other Diseases",
    
    #-----------------------------------------------------
    # BLANK ROWS ON OFFICIAL FORM
    #-----------------------------------------------------
    
    "70",
    "71",
    
    #-----------------------------------------------------
    # SERVICE STATISTICS
    #-----------------------------------------------------
    
    "NEW",
    "RE-ATT",
    "From another health facility",
    "To Other Health facility",
    "Community Unit",
    "To Community Unit"
    
  )
  
  
  #=======================================================
  # ORDER REPORT
  #=======================================================
  
  report <- report %>%
    
    dplyr::mutate(
      
      Section = factor(
        
        Section,
        
        levels = c(
          
          "DISEASES",
          
          "ATTENDANCE",
          
          "REFERRALS"
          
        )
        
      )
      
    ) %>%
    
    dplyr::arrange(
      
      health_facility,
      
      age_group,
      
      Section,
      
      category
      
    ) %>%
    
    dplyr::mutate(
      
      Section = as.character(
        Section
      )
      
    )
  
  
  return(
    report
  )
  
}


#=========================================================
# PREVIEW MOH 705 REPORT
#=========================================================

preview_moh705 <- function(
    report,
    facility = "All Facilities"
) {
  
  
  if (
    is.null(report) ||
    nrow(report) == 0
  ) {
    
    return(
      data.frame()
    )
    
  }
  
  
  if (
    facility != "All Facilities"
  ) {
    
    report <- report %>%
      
      dplyr::filter(
        
        health_facility ==
          facility
        
      )
    
  }
  
  
  return(
    report
  )
  
}


#=========================================================
# EXPORT MOH 705 REPORT TO EXCEL
#=========================================================

export_moh705 <- function(
    report,
    facility = "All Facilities",
    file
) {
  
  
  #-------------------------------------------------------
  # Check report
  #-------------------------------------------------------
  
  if (
    is.null(report) ||
    nrow(report) == 0
  ) {
    
    stop(
      "No data available for export."
    )
    
  }
  
  
  #-------------------------------------------------------
  # Filter facility
  #-------------------------------------------------------
  
  if (
    facility != "All Facilities"
  ) {
    
    report <- report %>%
      
      dplyr::filter(
        
        health_facility ==
          facility
        
      )
    
  }
  
  
  #-------------------------------------------------------
  # Facilities
  #-------------------------------------------------------
  
  facilities <- sort(
    
    unique(
      
      report$health_facility
      
    )
    
  )
  
  
  #-------------------------------------------------------
  # Workbook
  #-------------------------------------------------------
  
  wb <- openxlsx::createWorkbook()
  
  
  #=======================================================
  # STYLES
  #=======================================================
  
  header_style <- openxlsx::createStyle(
    
    fgFill = "#D9EAD3",
    
    textDecoration = "bold",
    
    halign = "center",
    
    border = "TopBottomLeftRight"
    
  )
  
  
  green_style <- openxlsx::createStyle(
    
    fgFill = "#C6EFCE"
    
  )
  
  
  total_style <- openxlsx::createStyle(
    
    textDecoration = "bold",
    
    halign = "center"
    
  )
  
  
  border_style <- openxlsx::createStyle(
    
    border = "TopBottomLeftRight"
    
  )
  
  
  #=======================================================
  # EXPORT EACH FACILITY
  #=======================================================
  
  for (
    fac in facilities
  ) {
    
    
    facility_data <- report %>%
      
      dplyr::filter(
        
        health_facility ==
          fac
        
      )
    
    
    age_groups <- unique(
      
      facility_data$age_group
      
    )
    
    
    for (
      ag in age_groups
    ) {
      
      
      #---------------------------------------------------
      # Data for age group
      #---------------------------------------------------
      
      sheet_data <- facility_data %>%
        
        dplyr::filter(
          
          age_group ==
            ag
          
        ) %>%
        
        dplyr::select(
          
          -health_facility
          
        )
      
      
      if (
        nrow(sheet_data) == 0
      ) {
        
        next
        
      }
      
      
      #---------------------------------------------------
      # Sheet name
      #---------------------------------------------------
      
      sheet_name <- substr(
        
        paste0(
          
          substr(
            fac,
            1,
            20
          ),
          
          "_",
          
          ag
          
        ),
        
        1,
        
        31
        
      )
      
      
      #---------------------------------------------------
      # Add worksheet
      #---------------------------------------------------
      
      openxlsx::addWorksheet(
        
        wb,
        
        sheet_name
        
      )
      
      
      #---------------------------------------------------
      # Write data
      #---------------------------------------------------
      
      openxlsx::writeData(
        
        wb,
        
        sheet_name,
        
        sheet_data
        
      )
      
      
      #---------------------------------------------------
      # Header
      #---------------------------------------------------
      
      openxlsx::addStyle(
        
        wb,
        
        sheet_name,
        
        style = header_style,
        
        rows = 1,
        
        cols = 1:ncol(sheet_data),
        
        gridExpand = TRUE
        
      )
      
      
      #---------------------------------------------------
      # Borders
      #---------------------------------------------------
      
      if (
        nrow(sheet_data) > 0
      ) {
        
        openxlsx::addStyle(
          
          wb,
          
          sheet_name,
          
          style = border_style,
          
          rows = 2:(nrow(sheet_data) + 1),
          
          cols = 1:ncol(sheet_data),
          
          gridExpand = TRUE,
          
          stack = TRUE
          
        )
        
      }
      
      
      #===================================================
      # HIGHLIGHT POSITIVE DAY VALUES
      #===================================================
      
      day_cols <- grep(
        
        # Monthly: 1,2,3...
        # Weekly/custom: 01-Sep, 02-Sep...
        "^[0-9]+$|^[0-9]{2}-[A-Za-z]{3}$",
        
        names(sheet_data)
        
      )
      
      
      for (
        col in day_cols
      ) {
        
        positive_rows <- which(
          
          sheet_data[[col]] > 0
          
        )
        
        
        if (
          length(positive_rows) > 0
        ) {
          
          openxlsx::addStyle(
            
            wb,
            
            sheet_name,
            
            style = green_style,
            
            rows = positive_rows + 1,
            
            cols = col,
            
            gridExpand = TRUE,
            
            stack = TRUE
            
          )
          
        }
        
      }
      
      
      #===================================================
      # HIGHLIGHT TOTAL COLUMN
      #===================================================
      
      total_col <- which(
        
        names(sheet_data) ==
          "Total"
        
      )
      
      
      if (
        length(total_col) == 1
      ) {
        
        openxlsx::addStyle(
          
          wb,
          
          sheet_name,
          
          style = total_style,
          
          rows = 2:(nrow(sheet_data) + 1),
          
          cols = total_col,
          
          gridExpand = TRUE,
          
          stack = TRUE
          
        )
        
      }
      
      
      #---------------------------------------------------
      # Column widths
      #---------------------------------------------------
      
      openxlsx::setColWidths(
        
        wb,
        
        sheet_name,
        
        cols = 1:ncol(sheet_data),
        
        widths = "auto"
        
      )
      
      
      #---------------------------------------------------
      # Freeze header
      #---------------------------------------------------
      
      openxlsx::freezePane(
        
        wb,
        
        sheet = sheet_name,
        
        firstRow = TRUE
        
      )
      
    }
    
  }
  
  
  #=======================================================
  # SAVE WORKBOOK
  #=======================================================
  
  openxlsx::saveWorkbook(
    
    wb,
    
    file,
    
    overwrite = TRUE
    
  )
  
}