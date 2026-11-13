#=========================================================
# REPORT HELPER FUNCTIONS
#=========================================================


#---------------------------------------------------------
# Convert labelled/factor variables to character
#---------------------------------------------------------

to_label_chr <- function(x) {
  
  if (inherits(x, "haven_labelled")) {
    
    return(
      as.character(
        haven::as_factor(
          x,
          levels = "labels"
        )
      )
    )
    
  }
  
  if (is.factor(x)) {
    
    return(as.character(x))
    
  }
  
  x
  
}


#---------------------------------------------------------
# Reporting period
#
# Supported period types:
#   "monthly"
#   "weekly"
#   "date_range"
#---------------------------------------------------------

get_reporting_period <- function(
    report_month = NULL,
    report_week = NULL,
    start_date = NULL,
    end_date = NULL,
    period_type = "monthly"
) {
  
  
  #=======================================================
  # MONTHLY
  #=======================================================
  
  if (period_type == "monthly") {
    
    if (is.null(report_month)) {
      
      stop(
        "report_month must be provided for monthly reports."
      )
      
    }
    
    report_month <- as.Date(report_month)
    
    if (is.na(report_month)) {
      
      stop(
        "Invalid report_month."
      )
      
    }
    
    start <- lubridate::floor_date(
      report_month,
      "month"
    )
    
    end <- lubridate::ceiling_date(
      report_month,
      "month"
    ) - lubridate::days(1)
    
    return(
      list(
        start = start,
        end = end,
        type = "monthly"
      )
    )
    
  }
  
  
  #=======================================================
  # WEEKLY
  #
  # Week runs Monday -> Sunday
  #=======================================================
  
  if (period_type == "weekly") {
    
    if (is.null(report_week)) {
      
      stop(
        "report_week must be provided for weekly reports."
      )
      
    }
    
    report_week <- as.Date(report_week)
    
    if (is.na(report_week)) {
      
      stop(
        "Invalid report_week."
      )
      
    }
    
    start <- lubridate::floor_date(
      report_week,
      unit = "week",
      week_start = 1
    )
    
    end <- start + lubridate::days(6)
    
    return(
      list(
        start = start,
        end = end,
        type = "weekly"
      )
    )
    
  }
  
  
  #=======================================================
  # CUSTOM DATE RANGE
  #=======================================================
  
  if (period_type == "date_range") {
    
    if (
      is.null(start_date) ||
      is.null(end_date)
    ) {
      
      stop(
        "Both start_date and end_date must be provided for date-range reports."
      )
      
    }
    
    start_date <- as.Date(start_date)
    end_date <- as.Date(end_date)
    
    if (
      is.na(start_date) ||
      is.na(end_date)
    ) {
      
      stop(
        "Invalid start_date or end_date."
      )
      
    }
    
    if (start_date > end_date) {
      
      stop(
        "Start date cannot be after end date."
      )
      
    }
    
    return(
      list(
        start = start_date,
        end = end_date,
        type = "date_range"
      )
    )
    
  }
  
  
  #=======================================================
  # Invalid period type
  #=======================================================
  
  stop(
    paste0(
      "Invalid period_type: ",
      period_type,
      ". ",
      "Use 'monthly', 'weekly', or 'date_range'."
    )
  )
  
}


#---------------------------------------------------------
# Reporting period label
#---------------------------------------------------------

get_reporting_period_label <- function(period) {
  
  if (period$type == "monthly") {
    
    return(
      format(
        period$start,
        "%b %Y"
      )
    )
    
  }
  
  
  if (period$type == "weekly") {
    
    return(
      paste0(
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
  
  
  if (period$type == "date_range") {
    
    return(
      paste0(
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
  
}


#---------------------------------------------------------
# Calculate age
#---------------------------------------------------------

calculate_age <- function(
    years,
    months,
    weeks
) {
  
  dplyr::case_when(
    
    !is.na(years) &
      years > 0 ~ years,
    
    !is.na(months) ~ months / 12,
    
    !is.na(weeks) ~ weeks / 52,
    
    !is.na(years) ~ years,
    
    TRUE ~ NA_real_
    
  )
  
}


#---------------------------------------------------------
# Standardize visit type
#---------------------------------------------------------

clean_visit_type <- function(x) {
  
  x <- x |>
    as.character() |>
    stringr::str_to_lower() |>
    stringr::str_squish()
  
  
  dplyr::case_when(
    
    stringr::str_detect(
      x,
      "^new"
    ) ~ "NEW",
    
    stringr::str_detect(
      x,
      "re\\s*-?\\s*att|re\\s*att|repeat|review|revisit"
    ) ~ "RE-ATT",
    
    TRUE ~ NA_character_
    
  )
  
}


#---------------------------------------------------------
# Reporting age categories
#---------------------------------------------------------

get_age_category <- function(
    age,
    sex
) {
  
  dplyr::case_when(
    
    !is.na(age) &
      age >= 60 ~ "Over 60 years",
    
    !is.na(age) &
      age >= 5 &
      sex == "Male" ~ "Over 5 - Male",
    
    !is.na(age) &
      age >= 5 &
      sex == "Female" ~ "Over 5 - Female",
    
    !is.na(age) &
      age < 5 &
      sex == "Male" ~ "Children Under 5 - Male",
    
    !is.na(age) &
      age < 5 &
      sex == "Female" ~ "Children Under 5 - Female",
    
    TRUE ~ NA_character_
    
  )
  
}


#---------------------------------------------------------
# Age order
#---------------------------------------------------------

age_levels <- c(
  
  "Over 5 - Male",
  
  "Over 5 - Female",
  
  "Children Under 5 - Male",
  
  "Children Under 5 - Female",
  
  "Over 60 years"
  
)


#---------------------------------------------------------
# Prepare OPD data
#
# This function:
#   1. Determines the reporting period
#   2. Converts labelled variables
#   3. Calculates age
#   4. Standardizes visit type
#   5. Filters records to selected dates
#---------------------------------------------------------

prepare_opd_data <- function(
    data,
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
  # Prepare data
  #-------------------------------------------------------
  
  prepared_data <- data %>%
    
    dplyr::mutate(
      
      visit_date = as.Date(
        visit_date
      )
      
    ) %>%
    
    dplyr::mutate(
      
      dplyr::across(
        
        dplyr::any_of(
          
          c(
            
            "visit_type",
            
            "sex",
            
            "age_group",
            
            "diagnosis1",
            
            "diagnosis2",
            
            "referred_from",
            
            "sus_malaria"
            
          )
          
        ),
        
        to_label_chr
        
      )
      
    ) %>%
    
    dplyr::mutate(
      
      #-----------------------------------------------
      # Calculate age
      #-----------------------------------------------
      
      age = calculate_age(
        
        age_years,
        
        age_months,
        
        age_weeks
        
      ),
      
      
      #-----------------------------------------------
      # Reporting age category
      #-----------------------------------------------
      
      age_reports = get_age_category(
        
        age,
        
        sex
        
      ),
      
      
      #-----------------------------------------------
      # Clean visit type
      #-----------------------------------------------
      
      visit_type = clean_visit_type(
        
        visit_type
        
      ),
      
      
      #-----------------------------------------------
      # Clean diagnoses
      #-----------------------------------------------
      
      diagnosis1 = stringr::str_squish(
        
        as.character(
          diagnosis1
        )
        
      ),
      
      diagnosis2 = stringr::str_squish(
        
        as.character(
          diagnosis2
        )
        
      ),
      
      
      #-----------------------------------------------
      # Clean referral source
      #-----------------------------------------------
      
      referred_from = stringr::str_to_title(
        
        stringr::str_squish(
          
          as.character(
            referred_from
          )
          
        )
        
      ),
      
      
      #-----------------------------------------------
      # Clean suspected malaria
      #-----------------------------------------------
      
      sus_malaria = stringr::str_squish(
        
        as.character(
          sus_malaria
        )
        
      )
      
    ) %>%
    
    #---------------------------------------------------
  # Filter to reporting period
  #---------------------------------------------------
  
  dplyr::filter(
    
    !is.na(visit_date),
    
    visit_date >= period$start,
    
    visit_date <= period$end
    
  )
  
  
  return(
    prepared_data
  )
  
}


#---------------------------------------------------------
# Month label
#
# Retained for compatibility with other existing modules
#---------------------------------------------------------

month_label <- function(date) {
  
  format(
    
    as.Date(date),
    
    "%b %Y"
    
  )
  
}