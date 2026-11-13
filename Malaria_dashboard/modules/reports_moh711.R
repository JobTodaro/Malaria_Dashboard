#=========================================================
# MOH 711 REPORT
#=========================================================


#=========================================================
# REPORT GENERATOR
#=========================================================

reports_moh711 <- function(
    opd_data,
    report_month = NULL,
    report_week = NULL,
    start_date = NULL,
    end_date = NULL,
    period_type = "monthly"
) {
  
  
  #-------------------------------------------------------
  # Determine reporting period
  #-------------------------------------------------------
  
  period <- get_reporting_period(
    
    report_month = report_month,
    
    report_week = report_week,
    
    start_date = start_date,
    
    end_date = end_date,
    
    period_type = period_type
    
  )
  
  
  #-------------------------------------------------------
  # Prepare OPD data
  #-------------------------------------------------------
  
  opd_data <- prepare_opd_data(
    
    data = opd_data,
    
    start_date = period$start,
    
    end_date = period$end,
    
    period_type = "date_range"
    
  )
  
  
  #-------------------------------------------------------
  # Return empty report if no data
  #-------------------------------------------------------
  
  if (
    is.null(opd_data) ||
    nrow(opd_data) == 0
  ) {
    
    return(
      list(
        
        daily_all = data.frame(),
        
        daily_under5 = data.frame(),
        
        daily_over5 = data.frame()
        
      )
    )
    
  }
  
  
  #=======================================================
  # Reporting dates
  #=======================================================
  
  reporting_dates <- seq.Date(
    
    from = period$start,
    
    to = period$end,
    
    by = "day"
    
  )
  
  
  #=======================================================
  # Date column labels
  #=======================================================
  
  if (
    period_type == "monthly"
  ) {
    
    #-----------------------------------------------
    # Monthly:
    # 1, 2, 3 ... 30/31
    #-----------------------------------------------
    
    day_labels <- as.character(
      
      lubridate::day(
        reporting_dates
      )
      
    )
    
  } else {
    
    #-----------------------------------------------
    # Weekly/custom:
    # 28-Sep, 29-Sep, etc.
    #-----------------------------------------------
    
    day_labels <- format(
      
      reporting_dates,
      
      "%d-%b"
      
    )
    
  }
  
  
  #=======================================================
  # INTERNAL FUNCTION
  #=======================================================
  
  make_daily_summary <- function(
    df,
    age_filter = "all"
  ) {
    
    
    #-----------------------------------------------------
    # Age filter
    #-----------------------------------------------------
    
    if (
      age_filter == "under5"
    ) {
      
      df <- df %>%
        
        dplyr::filter(
          
          age < 5
          
        )
      
    }
    
    
    else if (
      age_filter == "over5"
    ) {
      
      df <- df %>%
        
        dplyr::filter(
          
          age >= 5
          
        )
      
    }
    
    
    #-----------------------------------------------------
    # Return empty if no records
    #-----------------------------------------------------
    
    if (
      nrow(df) == 0
    ) {
      
      return(
        data.frame()
      )
      
    }
    
    
    #=====================================================
    # Create reporting-day variable
    #=====================================================
    
    if (
      period_type == "monthly"
    ) {
      
      df <- df %>%
        
        dplyr::mutate(
          
          day = as.character(
            
            lubridate::day(
              visit_date
            )
            
          ),
          
          Month = format(
            
            visit_date,
            
            "%B %Y"
            
          )
          
        )
      
    }
    
    
    else {
      
      df <- df %>%
        
        dplyr::mutate(
          
          day = format(
            
            visit_date,
            
            "%d-%b"
            
          ),
          
          Month = paste0(
            
            format(
              period$start,
              "%d %b %Y"
            ),
            
            " - ",
            
            format(
              period$end,
              "%d %b %Y"
            )
            
          )
          
        )
      
    }
    
    
    #=====================================================
    # Build summary
    #=====================================================
    
    result <- df %>%
      
      tidyr::pivot_longer(
        
        cols = tidyselect::any_of(
          
          c(
            
            "visit_type",
            
            "referred_from"
            
          )
          
        ),
        
        names_to = "var_group",
        
        values_to = "category"
        
      ) %>%
      
      dplyr::mutate(
        
        var_group = stringr::str_replace_all(
          
          var_group,
          
          "_",
          
          " "
          
        ),
        
        row_label = paste0(
          
          var_group,
          
          ": ",
          
          category
          
        )
        
      ) %>%
      
      dplyr::filter(
        
        !is.na(category),
        
        stringr::str_squish(
          as.character(category)
        ) != ""
        
      ) %>%
      
      dplyr::group_by(
        
        health_facility,
        
        Month,
        
        row_label,
        
        day
        
      ) %>%
      
      dplyr::summarise(
        
        n = dplyr::n(),
        
        .groups = "drop"
        
      ) %>%
      
      tidyr::pivot_wider(
        
        names_from = day,
        
        values_from = n,
        
        values_fill = 0
        
      )
    
    
    #=====================================================
    # Make sure every selected day exists as a column
    #=====================================================
    
    missing_columns <- setdiff(
      
      day_labels,
      
      names(result)
      
    )
    
    
    if (
      length(missing_columns) > 0
    ) {
      
      for (
        col in missing_columns
      ) {
        
        result[[col]] <- 0
        
      }
      
    }
    
    
    #=====================================================
    # Arrange columns in chronological order
    #=====================================================
    
    result <- result %>%
      
      dplyr::select(
        
        health_facility,
        
        Month,
        
        row_label,
        
        dplyr::all_of(
          day_labels
        )
        
      ) %>%
      
      dplyr::arrange(
        
        health_facility,
        
        row_label
        
      )
    
    
    return(
      result
    )
    
  }
  
  
  #=======================================================
  # RETURN REPORT
  #=======================================================
  
  list(
    
    daily_all = make_daily_summary(
      
      opd_data,
      
      "all"
      
    ),
    
    
    daily_under5 = make_daily_summary(
      
      opd_data,
      
      "under5"
      
    ),
    
    
    daily_over5 = make_daily_summary(
      
      opd_data,
      
      "over5"
      
    )
    
  )
  
}


#=========================================================
# PREVIEW
#=========================================================

preview_moh711 <- function(
    report,
    facility = "All Facilities"
) {
  
  
  #-------------------------------------------------------
  # Check report
  #-------------------------------------------------------
  
  if (
    is.null(report) ||
    is.null(report$daily_all) ||
    nrow(report$daily_all) == 0
  ) {
    
    return(
      data.frame()
    )
    
  }
  
  
  #-------------------------------------------------------
  # All facilities
  #-------------------------------------------------------
  
  if (
    facility == "All Facilities"
  ) {
    
    return(
      report$daily_all
    )
    
  }
  
  
  #-------------------------------------------------------
  # Selected facility
  #-------------------------------------------------------
  
  report$daily_all %>%
    
    dplyr::filter(
      
      health_facility ==
        facility
      
    )
  
}


#=========================================================
# EXPORT MOH 711
#=========================================================

export_moh711 <- function(
    report,
    facility,
    file
) {
  
  
  #-------------------------------------------------------
  # Check report
  #-------------------------------------------------------
  
  if (
    is.null(report)
  ) {
    
    stop(
      "No report available for export."
    )
    
  }
  
  
  if (
    is.null(report$daily_under5) ||
    is.null(report$daily_over5)
  ) {
    
    stop(
      "MOH 711 report contains no data."
    )
    
  }
  
  
  #-------------------------------------------------------
  # Determine facilities
  #-------------------------------------------------------
  
  if (
    facility == "All Facilities"
  ) {
    
    facilities <- unique(
      
      report$daily_all$health_facility
      
    )
    
  } else {
    
    facilities <- facility
    
  }
  
  
  facilities <- facilities[
    
    !is.na(facilities)
    
  ]
  
  
  #-------------------------------------------------------
  # Create sheets
  #-------------------------------------------------------
  
  sheets <- list()
  
  
  for (
    fac in facilities
  ) {
    
    
    #=====================================================
    # UNDER 5
    #=====================================================
    
    sheet_under5 <- paste0(
      
      fac,
      
      "_Under5"
      
    )
    
    
    # Excel sheet-name cleanup
    
    sheet_under5 <- gsub(
      
      "[:\\\\/?*\\[\\]]",
      
      "-",
      
      sheet_under5
      
    )
    
    
    sheet_under5 <- substr(
      
      sheet_under5,
      
      1,
      
      31
      
    )
    
    
    sheets[[sheet_under5]] <-
      
      report$daily_under5 %>%
      
      dplyr::filter(
        
        health_facility ==
          fac
        
      ) %>%
      
      dplyr::select(
        
        -health_facility
        
      )
    
    
    #=====================================================
    # OVER 5
    #=====================================================
    
    sheet_over5 <- paste0(
      
      fac,
      
      "_Over5"
      
    )
    
    
    sheet_over5 <- gsub(
      
      "[:\\\\/?*\\[\\]]",
      
      "-",
      
      sheet_over5
      
    )
    
    
    sheet_over5 <- substr(
      
      sheet_over5,
      
      1,
      
      31
      
    )
    
    
    sheets[[sheet_over5]] <-
      
      report$daily_over5 %>%
      
      dplyr::filter(
        
        health_facility ==
          fac
        
      ) %>%
      
      dplyr::select(
        
        -health_facility
        
      )
    
  }
  
  
  #-------------------------------------------------------
  # Write Excel
  #-------------------------------------------------------
  
  writexl::write_xlsx(
    
    sheets,
    
    path = file
    
  )
  
}