#=========================================================
# OPD REPORT
#=========================================================


#---------------------------------------------------------
# Generate OPD Report
#
# Supports:
#   - Monthly
#   - Weekly
#   - Custom date range
#---------------------------------------------------------

reports_opd <- function(
    opd_data,
    report_month = NULL,
    report_week = NULL,
    start_date = NULL,
    end_date = NULL,
    period_type = "monthly"
) {
  
  
  #=======================================================
  # Determine reporting period
  #=======================================================
  
  period <- get_reporting_period(
    
    report_month = report_month,
    
    report_week = report_week,
    
    start_date = start_date,
    
    end_date = end_date,
    
    period_type = period_type
    
  )
  
  
  #=======================================================
  # Reporting period label
  #=======================================================
  
  period_label <- get_reporting_period_label(
    
    period
    
  )
  
  
  #=======================================================
  # Prepare OPD data
  #=======================================================
  
  opd_data <- prepare_opd_data(
    
    data = opd_data,
    
    report_month = report_month,
    
    report_week = report_week,
    
    start_date = start_date,
    
    end_date = end_date,
    
    period_type = period_type
    
  )
  
  
  #=======================================================
  # Build report
  #=======================================================
  
  report <- opd_data %>%
    
    #-----------------------------------------------------
  # Remove records without required reporting fields
  #-----------------------------------------------------
  
  dplyr::filter(
    
    !is.na(health_facility),
    
    !is.na(age_reports),
    
    !is.na(visit_type)
    
  ) %>%
    
    #-----------------------------------------------------
  # Set age category order
  #-----------------------------------------------------
  
  dplyr::mutate(
    
    report_period = period_label,
    
    age_reports = factor(
      
      age_reports,
      
      levels = age_levels
      
    )
    
  ) %>%
    
    #-----------------------------------------------------
  # Count visits
  #-----------------------------------------------------
  
  dplyr::count(
    
    health_facility,
    
    report_period,
    
    age_reports,
    
    visit_type,
    
    name = "n"
    
  ) %>%
    
    #-----------------------------------------------------
  # Ensure NEW and RE-ATT exist for every category
  #-----------------------------------------------------
  
  tidyr::complete(
    
    health_facility,
    
    report_period,
    
    age_reports,
    
    visit_type = c(
      
      "NEW",
      
      "RE-ATT"
      
    ),
    
    fill = list(
      
      n = 0
      
    )
    
  ) %>%
    
    #-----------------------------------------------------
  # Convert NEW / RE-ATT into columns
  #-----------------------------------------------------
  
  tidyr::pivot_wider(
    
    names_from = visit_type,
    
    values_from = n,
    
    values_fill = 0
    
  ) %>%
    
    #-----------------------------------------------------
  # Calculate total
  #-----------------------------------------------------
  
  dplyr::mutate(
    
    TOTAL = NEW + `RE-ATT`
    
  )
  
  
  #=======================================================
  # Add TOTAL GENERAL OUTPATIENTS row
  #=======================================================
  
  totals <- report %>%
    
    dplyr::group_by(
      
      health_facility,
      
      report_period
      
    ) %>%
    
    dplyr::summarise(
      
      age_reports = "TOTAL GENERAL OUTPATIENTS",
      
      NEW = sum(
        NEW,
        na.rm = TRUE
      ),
      
      `RE-ATT` = sum(
        `RE-ATT`,
        na.rm = TRUE
      ),
      
      TOTAL = sum(
        TOTAL,
        na.rm = TRUE
      ),
      
      .groups = "drop"
      
    )
  
  
  #=======================================================
  # Combine age-specific rows and total row
  #=======================================================
  
  report <- dplyr::bind_rows(
    
    report,
    
    totals
    
  ) %>%
    
    #-----------------------------------------------------
  # Order age categories
  #-----------------------------------------------------
  
  dplyr::mutate(
    
    age_reports = factor(
      
      age_reports,
      
      levels = c(
        
        age_levels,
        
        "TOTAL GENERAL OUTPATIENTS"
        
      )
      
    )
    
  ) %>%
    
    #-----------------------------------------------------
  # Sort report
  #-----------------------------------------------------
  
  dplyr::arrange(
    
    health_facility,
    
    age_reports
    
  )
  
  
  #=======================================================
  # Return report
  #=======================================================
  
  return(
    report
  )
  
}


#=========================================================
# FLEXTABLE PREVIEW
#=========================================================

create_opd_flextable <- function(
    report,
    facility
) {
  
  
  #-------------------------------------------------------
  # Filter facility
  #-------------------------------------------------------
  
  if (facility != "All Facilities") {
    
    report <- report %>%
      
      dplyr::filter(
        
        health_facility == facility
        
      )
    
  }
  
  
  #-------------------------------------------------------
  # Select table columns
  #-------------------------------------------------------
  
  table_data <- report %>%
    
    dplyr::select(
      
      report_period,
      
      age_reports,
      
      NEW,
      
      `RE-ATT`,
      
      TOTAL
      
    )
  
  
  #-------------------------------------------------------
  # Create flextable
  #-------------------------------------------------------
  
  ft <- flextable::flextable(
    
    table_data
    
  ) %>%
    
    flextable::autofit()
  
  
  #-------------------------------------------------------
  # Bold total row
  #-------------------------------------------------------
  
  total_rows <- which(
    
    as.character(
      table_data$age_reports
    ) ==
      "TOTAL GENERAL OUTPATIENTS"
    
  )
  
  
  if (length(total_rows) > 0) {
    
    ft <- flextable::bold(
      
      ft,
      
      i = total_rows,
      
      bold = TRUE
      
    )
    
  }
  
  
  #-------------------------------------------------------
  # Caption
  #-------------------------------------------------------
  
  caption <- if (
    facility == "All Facilities"
  ) {
    
    "General Outpatient Attendance"
    
  } else {
    
    paste(
      
      "General Outpatient Attendance -",
      
      facility
      
    )
    
  }
  
  
  #-------------------------------------------------------
  # Add caption
  #-------------------------------------------------------
  
  ft <- flextable::set_caption(
    
    ft,
    
    caption
    
  )
  
  
  #-------------------------------------------------------
  # Return flextable
  #-------------------------------------------------------
  
  return(
    ft
  )
  
}


#=========================================================
# EXPORT OPD REPORT TO WORD
#=========================================================

export_opd_report <- function(
    report,
    facility,
    file
) {
  
  
  #-------------------------------------------------------
  # Create Word document
  #-------------------------------------------------------
  
  doc <- officer::read_docx()
  
  
  #-------------------------------------------------------
  # Determine facilities
  #-------------------------------------------------------
  
  if (
    facility == "All Facilities"
  ) {
    
    facilities <- unique(
      
      report$health_facility
      
    )
    
    facilities <- facilities[
      
      !is.na(facilities)
      
    ]
    
  } else {
    
    facilities <- facility
    
  }
  
  
  #-------------------------------------------------------
  # Add each facility
  #-------------------------------------------------------
  
  for (
    fac in facilities
  ) {
    
    
    #-----------------------------------------------------
    # Create table
    #-----------------------------------------------------
    
    ft <- create_opd_flextable(
      
      report = report,
      
      facility = fac
      
    )
    
    
    #-----------------------------------------------------
    # Add table to Word
    #-----------------------------------------------------
    
    doc <- flextable::body_add_flextable(
      
      doc,
      
      ft
      
    )
    
    
    #-----------------------------------------------------
    # Add spacing
    #-----------------------------------------------------
    
    doc <- officer::body_add_par(
      
      doc,
      
      ""
      
    )
    
  }
  
  
  #-------------------------------------------------------
  # Save Word document
  #-------------------------------------------------------
  
  print(
    
    doc,
    
    target = file
    
  )
  
}