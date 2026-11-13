queriesUI <- function(id) {
  
  ns <- NS(id)
  
  tabsetPanel(
    
    tabPanel(
      "Missing Socio Demographics",
      DTOutput(ns("missing_location"))
    ),
    
    tabPanel(
      "Age Variance",
      DTOutput(ns("age_var"))
    ),
    
    tabPanel(
      "Out-of-range Temp",
      DTOutput(ns("temp"))
    ),
    
    tabPanel(
      "Missing Temp",
      DTOutput(ns("missing_temp"))
    ),
    
    tabPanel(
      "Missing fever history/mal symptoms",
      DTOutput(ns("missing_mal_ind"))
    ),
    
    tabPanel(
      "Missed Testing opportunities",
      DTOutput(ns("missed_testing"))
    ),
    
    tabPanel("Assymptomatic Tested",
      DTOutput(ns("tested_assymptomatic"))
      ),
    
    tabPanel(
      "Missing Treatment",
      DTOutput(ns("missing_tx"))
    ),
    
    tabPanel(
      "Missing Test Indicator",
      DTOutput(ns("mal_test_Indicator_NA"))
    ),
    
    tabPanel(
      "Negative Treated",
      DTOutput(ns("negative_treated"))
    ),
    
    tabPanel(
      "Diagnosis Variances",
      DTOutput(ns("diag_dups"))
    ),
    
    tabPanel(
      "Positive w/o Diagnosis",
      DTOutput(ns("mal_diagnosis"))
    ),
    
    tabPanel(
      "Duplicates",
      DTOutput(ns("duplicates"))
    ),
    
    tabPanel(
      "OPD Check",
      DTOutput(ns("opd_check"))
    ),
    
    tabPanel(
      "OPD Compare",
      DTOutput(ns("opd_compare"))
    ),
    
  tabPanel(
    "Data Entered in the Incorrect Event",
    DTOutput(ns("incorrect_event"))
  ),
  
  tabPanel(
    "Repeat Visits Within the Month",
    DTOutput(ns("repeat_visits"))
  ))
}

#---------------------------------------
# SERVER
#---------------------------------------
queriesServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    dt_options <- list(
      pageLength = 10,
      scrollX = TRUE,
      autoWidth = TRUE
    )
    
    make_dt <- function(x, filename = "query") {
      datatable(
        x,
        extensions = "Buttons",
        rownames = FALSE,
        filter = "top",
        options = c(
          dt_options,
          list(
            dom = "Bfrtip",
            buttons = list(
              list(
                extend = "excel",
                filename = filename,
                title = NULL
              ),
              "copy",
              "csv",
              "print"
            )
          )
        )
      )
    }
    
    
    #---------------------------------------
    # Missing socio-demographics
    #---------------------------------------
    
    output$missing_location <- renderDT({
      
      data() |>
        select(
          county, subcounty, village,
          hf_county, health_facility,
          opd_no, visit_date, visit_type, age_group
        ) |>
        filter(
          is.na(county) |
            (
              county %in% c("Siaya", "Kakamega") &
                subcounty %in% c(
                  "Butere",
                  "Mumias West",
                  "4106 RARIEDA"
                ) &
                if_any(c(subcounty, village), is.na) &
                !is.na(opd_no) &
                !is.na(visit_date) &
                !is.na(age_group)
            )
        ) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Age and visit-date variance
    #---------------------------------------
    
    output$age_var <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, visit_date, opd_no,
          age_group, sex, age_years, age_months,
          age_weeks, age_days
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
        ) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Out-of-range temperature
    #---------------------------------------
    
    output$temp <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, opd_no,
          visit_date, age_group, temp,
          fever_48h, mal_symptoms
        ) |>
        filter(
          !is.na(temp),
          temp >= 43 | temp < 33
        ) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Missing malaria indicators
    #---------------------------------------
    
    output$missing_mal_ind <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, opd_no,
          county, subcounty, visit_date,
          age_group, temp, fever_48h, sus_malaria, mal_symptoms
        ) |>
        filter(
          if_any(
            c(fever_48h, sus_malaria),
            is.na
          )|
            is.na(mal_symptoms)& hf_county=="Siaya "
        ) |>
        make_dt()
    }, server=FALSE)
    
    
    #---------------------------------------
    # Missing temperature
    #---------------------------------------
    
    output$missing_temp <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, opd_no,
          county, subcounty, visit_date,
          age_group, temp, fever_48h, sus_malaria
        ) |>
        filter(is.na(temp)) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Missed testing opportunities
    #---------------------------------------
    
    output$missed_testing <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, age_group,
          opd_no, visit_date, fever_48h, temp,
          febrile, mal_symptoms, malaria_symptomatic,
          danger_signs, diagnosis1, diagnosis2, other_diagnosis1,
          tested
        ) |>
        filter(
          (malaria_symptomatic==1) & tested != 1
        ) |>
        make_dt()
    }, server=FALSE)
   
    
    #---------------------------------------
    # Asymptomatic but tested
    #---------------------------------------
    
    output$tested_assymptomatic <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, age_group,
          opd_no, visit_date, fever_48h, temp,
          febrile, mal_symptoms, malaria_symptomatic,danger_signs,
          sus_malaria, diagnosis1, diagnosis2, other_diagnosis1,
          tested
        ) |>
      filter(
        (malaria_symptomatic==0 & tested==1)
      ) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Missing malaria treatment
    #---------------------------------------
    
    output$missing_tx <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, opd_no,
          visit_date, age_group, temp, fever_48h,
          mal_symptoms, sus_malaria,
          diagnosis1, diagnosis2, other_diagnosis1,
          quinine_oral, quinine_paren, artesunate,
          dha_ppq, al, tx_none, other_tx, other_mal_tx
        ) |>
        filter(
          sus_malaria %in% c(
            "RDT Tested (+ve)",
            "Microscopy Tested (+ve)"
          ),
          
          if_all(
            c(
              quinine_oral, quinine_paren,
              artesunate, dha_ppq, al
            ),
            ~ .x %in% c(0, "No", NA)
          ),
          
          is.na(other_mal_tx) |
            other_mal_tx == "" |
            tx_none == "Yes"
        ) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Missing malaria test indicator
    #---------------------------------------
    
    output$mal_test_Indicator_NA <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, opd_no,
          visit_date, age_group, temp, fever_48h,
          mal_symptoms, sus_malaria,
          quinine_oral, quinine_paren, artesunate,
          dha_ppq, al, tx_none, tx_na, other_tx
        ) |>
        filter(
          if_all(
            c(
              quinine_oral, quinine_paren,
              artesunate, dha_ppq, al,
              tx_none, tx_na, other_tx
            ),
            ~ (.x %in% c(0, "0")) | is.na(.x)
          ),
          
          !sus_malaria %in% c(
            "Not tested",
            "NOT tested"
          ),
          
          mal_symptoms == "Yes"
        ) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Treated without positive malaria test
    #---------------------------------------
    
    output$negative_treated <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, opd_no,
          visit_date, age_group, temp, fever_48h,
          mal_symptoms,malaria_symptomatic,danger_signs, sus_malaria,
          quinine_oral, quinine_paren, artesunate,
          dha_ppq, al, tx_none, tx_na, other_tx,
          positive
        ) |>
        filter(
          if_any(
            c(
              quinine_oral, quinine_paren,
              artesunate, dha_ppq, al, other_tx
            ),
            ~ .x %in% c(1, "1")
          ),
          
          positive %in% c(0, "0", "")
        ) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Duplicate diagnosis
    #---------------------------------------
    
    output$diag_dups <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, visit_date,
          opd_no, visit_type, age_group,
          diagnosis1, diagnosis2,
          other_diagnosis1, other_diagnosis2
        ) |>
        filter(
          (
            diagnosis1 == diagnosis2 |
              other_diagnosis1 == other_diagnosis2
          ) &
            (diagnosis1 != "Other Disease (Specify)" &
            diagnosis2 != "Other Disease (Specify)"
          ) |
            (diagnosis1=="Malaria - Suspected" &
            diagnosis2==" Malaria - Confirmed"
        )|
            (diagnosis1=="Malaria - Confirmed" &
            diagnosis2 =="Malaria - Suspected")
            
        ) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Positive malaria case without diagnosis
    #---------------------------------------
    
    output$mal_diagnosis <- renderDT({
      
      malaria_dx <- c(
        "Malaria - Confirmed",
        "Malaria - in Pregnancy",
        "Malaria - Suspected"
      )
      
      data() |>
        select(
          hf_county, health_facility, opd_no,
          visit_date, age_group, sus_malaria,
          positive, diagnosis1, diagnosis2,
          other_diagnosis1, other_diagnosis2,
          totaltx
        ) |>
        filter(
          positive == 1,
          
          # No malaria diagnosis in either field
          if_all(
            c(diagnosis1, diagnosis2),
            ~ !.x %in% malaria_dx
          ) |
            
            # Suspected + Confirmed (either order)
            (
              diagnosis1 %in% c("Malaria - Suspected", "Malaria - Confirmed") &
                diagnosis2 %in% c("Malaria - Suspected", "Malaria - Confirmed") &
                diagnosis1 != diagnosis2
            )
        ) |>
        make_dt()
      
    })
    
    #---------------------------------------
    # Duplicate OPD records
    #---------------------------------------
    
    output$duplicates <- renderDT({
      
      data() |>
        select(
          hf_county, health_facility, opd_no,
          visit_date, 
          age_group,
          age_years,
          age_months, 
          sex, 
          visit_type,
          fever_48h, 
          temp
        ) |>
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
        filter(dupe_n > 1) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # Inconsistent OPD entries
    #---------------------------------------
    
    output$opd_check <- renderDT({
      
      data() |>
        select(
          health_facility, opd_no,
          opd_conf, visit_date, age_group
        ) |>
        filter(opd_no != opd_conf) |>
        make_dt()
    })
    
    
    #---------------------------------------
    # OPD compare
    #---------------------------------------
    
    output$opd_compare <- renderDT({
      
      data() |>
        mutate(visit_date = as.Date(visit_date)) |>
        group_by(hf_county, health_facility) |>
        summarise(
          records_in_filtered_data = n(),
          first_visit = min(visit_date, na.rm = TRUE),
          last_visit = max(visit_date, na.rm = TRUE),
          .groups = "drop"
        ) |>
        make_dt()
    })
  
    
    #---------------------------------------
    # Data entered in incorrect events
    #---------------------------------------
    
    output$incorrect_event <- renderDT({
      
      # Build month lookup dynamically based on min/max years in your data
      month_lookup <- tidyr::expand_grid(
        year = seq(
          lubridate::year(min(as.Date(data()$visit_date, na.rm = TRUE))),
          lubridate::year(max(as.Date(data()$visit_date, na.rm = TRUE)))
        ),
        month_num = 1:12
      ) |>
        mutate(
          redcap_event_name = paste(month.abb[month_num], year),
          # Adjust labels if REDCap uses full month names
          redcap_event_name = case_when(
            month_num == 6 ~ paste("June", year),
            month_num == 7 ~ paste("July", year),
            TRUE ~ redcap_event_name
          ),
          start_date = lubridate::make_date(year, month_num, 1),
          # FIX: use months() directly
          end_date   = start_date %m+% months(1)
        ) |>
        select(redcap_event_name, start_date, end_date)
      
      data() |>
        mutate(visit_date = as.Date(visit_date)) |>
        left_join(month_lookup, by = "redcap_event_name") |>
        mutate(
          error_reason = case_when(
            is.na(start_date) ~ "No matching event",
            is.na(visit_date) ~ "Missing visit date",
            visit_date < start_date ~ "Visit before event start",
            visit_date >= end_date ~ "Visit after event end",
            TRUE ~ NA_character_
          )
        ) |>
        filter(!is.na(error_reason)) |>
        select(
          hf_county,
          health_facility,
          redcap_event_name,
          redcap_repeat_instance,
          opd_no,
          visit_date,
          age_group,
          age_years,
          age_months,
          sex,
          visit_type,
          error_reason
        ) |>
        arrange(health_facility, redcap_event_name, opd_no) |>
        make_dt()
      
    }, server = FALSE)
    
    
    
    #---------------------------------------
    # Repeat visits within 5 days 
    #---------------------------------------
    
    output$repeat_visits <- renderDT({
      
      repeat_visits_df <- data() %>%
        
        mutate(
          visit_date = as.Date(visit_date),
          diagnosis = paste(diagnosis1, diagnosis2, sep = " | ")
        ) %>%
        
        filter(
          !is.na(opd_no),
          opd_no != "",
          !is.na(visit_date)
        ) %>%
        
        arrange(
          health_facility,
          opd_no,
          age_group,
          visit_date
        ) %>%
        
        group_by(
          health_facility,
          opd_no,
          age_group
        ) %>%
        
        mutate(
          previous_visit = lag(visit_date),
          previous_diagnosis = lag(diagnosis),
          
          days_between_visits = as.numeric(
            visit_date - previous_visit
          ),
          
          diagnosis_changed = case_when(
            is.na(previous_diagnosis) ~ NA_character_,
            previous_diagnosis == diagnosis ~ "No",
            TRUE ~ "Yes"
          )
        ) %>%
        
        filter(
          !is.na(days_between_visits),
          days_between_visits <= 5
        ) %>%
        
        select(
          hf_county,
          health_facility,
          opd_no,
          age_group,
          previous_visit,
          visit_date,
          days_between_visits,
          previous_diagnosis,
          diagnosis,
          diagnosis_changed
        ) %>%
        
        ungroup()
      
      DT::datatable(
        repeat_visits_df,
        rownames = FALSE,
        filter = "top",
        options = list(
          pageLength = 10,
          scrollX = TRUE
        )
      )
      
    }, server = FALSE)
  })
}
  
 