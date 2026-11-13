# TABLE MODULE

# UI
tableUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    tabBox(
      width = 12,
      
      # -------------------------------------------------------
      # TAB 1: Summary by Age Group
      # -------------------------------------------------------
      
      tabPanel(
        title = "Patient Characteristics",
        icon = icon("users"),
        
        br(),
        
        fluidRow(
          box(
            title = "Patient Characteristics by Age Group",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            gt::gt_output(
              ns("age_summary")
            )
          )
        )
      ),
      
      
      # -------------------------------------------------------
      # TAB 2: Summary by Health Facility
      # -------------------------------------------------------
      
      tabPanel(
        title = "By Health Facility",
        icon = icon("hospital"),
        
        br(),
        
        fluidRow(
          box(
            title = "Patient Characteristics by Health Facility",
            width = 12,
            status = "primary",
            solidHeader = TRUE,
            
            div(
              style = "overflow-x: auto;",
              
              gt::gt_output(
                ns("facility_summary")
              )
            )
          )
        )
      ),
      
      
      # -------------------------------------------------------
      # TAB 3: Raw Patient Data
      # -------------------------------------------------------
      
      # tabPanel(
      #   title = "Patient-Level Data",
      #   icon = icon("table"),
      #   
      #   br(),
      #   
      #   fluidRow(
      #     box(
      #       title = "Patient-Level Dataset",
      #       width = 12,
      #       status = "info",
      #       solidHeader = TRUE,
      #       
      #       DT::DTOutput(
      #         ns("datatable")
      #       )
      #     )
      #   )
      # )
      # 
    )
  )
}


# ============================================================
# TABLE MODULE SERVER
# Summary tables respond directly to dashboard filters
# ============================================================

tableServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
        # FUNCTION TO CLEAN CURRENT FILTERED DATA

    clean_report_data <- function(df) {
      
      df %>%
        dplyr::mutate(
          
          # -----------------------------------------------
          # Clean categorical variables as characters first
          # -----------------------------------------------
          dplyr::across(
            dplyr::any_of(
              c(
                "health_facility",
                "age_group",
                "fever_48h",
                "tested",
                "positive",
                "sus_malaria", 
                "febrile",
                "mal_result"
              )
            ),
            ~ {
              x <- stringr::str_squish(as.character(.x))
              x[x == ""] <- NA_character_
              x
            }
          ),
          
          
          # -----------------------------------------------
          # Malaria symptoms
          # Predefined levels allow all-NA subsets
          # -----------------------------------------------
          mal_symptoms = factor(
            as.character(mal_symptoms),
            levels = c("No", "Yes")
          ),
          
        # Febrile and Tested
        febrile = factor(febrile, 
                         levels = c("No", "Yes")),
        
        tested = factor(tested, 
                        levels = c("No", "Yes")
                        ),
        mal_result = factor(mal_result,
                            levels = c("Positive","Negative")
          
        ),
          # -----------------------------------------------
          # Malaria symptomatic
          # -----------------------------------------------
          malaria_symptomatic = dplyr::case_when(
            as.character(malaria_symptomatic) == "1" ~ "Yes",
            as.character(malaria_symptomatic) == "0" ~ "No",
            as.character(malaria_symptomatic) %in% c("Yes", "No") ~ 
              as.character(malaria_symptomatic),
            TRUE ~ NA_character_
          ),
          
          malaria_symptomatic = factor(
            malaria_symptomatic,
            levels = c("No", "Yes")
          )
          
        )
    }
    
    # ========================================================
    # TABLE 1
    # PATIENT CHARACTERISTICS BY AGE GROUP
    # ========================================================
    
    output$age_summary <- gt::render_gt({
      
      # Get the current filtered dashboard data
      df <- data()
      
      req(df)
      req(nrow(df) > 0)
      
      
      # Clean current filtered data
      df <- clean_report_data(df)
      
      
      # Check required variables
      required_columns <- c(
        "health_facility",
        "age_group",
        "age_years",
        "temp",
        "temp_cat",
        "fever_48h",
        "mal_symptoms",
        "febrile",
        # "tested",
        # "positive",
        "sus_malaria", 
        "mal_result"
      )
      
      
      validate(
        need(
          all(required_columns %in% names(df)),
          "Some variables required for the summary table are missing."
        )
      )
      
      
      # Build summary using the filtered data
      summary_table <- df %>%
        
        dplyr::select(
          health_facility,
          age_group,
          age_years,
          temp,
          temp_cat,
          fever_48h,
          mal_symptoms,
          # malaria_symptomatic,
          # tested,
          # positive,
          febrile,
          mal_result,
          sus_malaria
        ) %>%
        
      tbl_summary(
          
          by = age_group,
          
          type = list(
            c(
              health_facility,
              fever_48h,
              mal_symptoms,
              temp_cat,
              # malaria_symptomatic,
              # tested,
              # positive,
              febrile,
              sus_malaria,
              mal_result
            ) ~ "categorical",
            
            c(
              age_years,
              temp
            ) ~ "continuous"
          ),
          
          statistic = list(
            
            all_continuous() ~
              "{median} ({p25}, {p75})",
            
            all_categorical() ~
              "{n} ({p}%)"
            
          ),
          
          missing = "ifany",
          
          label = list(
            health_facility ~ "Health Facility",
            age_years ~ "Age in Years",
            temp ~ "Temperature",
            fever_48h ~ "Fever in Last 48 Hours",
            febrile ~ "Febrile (Temp >=37.5 or reported history of fever)",
            temp_cat ~ "Temperature (Grouped)",
            mal_symptoms ~ "Malaria Symptoms",
            mal_result ~ "Malaria test result",
            # malaria_symptomatic ~ "Malaria Symptomatic",
            # tested ~ "Malaria Tested",
            # positive ~ "Positive Malaria Test",
            sus_malaria ~ "Malaria results by Testing method"
          )
          
        ) %>%
        
        gtsummary::add_overall(
          last = TRUE
        ) %>%
        
        gtsummary::bold_labels()
      
      
      # Convert to GT
      summary_table %>%
        
        gtsummary::as_gt() %>%
        
        gt::tab_header(
          
          title = gt::md(
            "**Patient Characteristics by Age Group**"
          ),
          
          subtitle = paste(
            "Current filtered dataset:",
            scales::comma(nrow(df)),
            "patient records"
          )
          
        ) %>%
        
        gt::tab_options(
          table.width = gt::pct(100),
          heading.align = "left",
          column_labels.font.weight = "bold",
          row_group.font.weight = "bold"
        )
      
    })
    
    
    # ========================================================
    # TABLE 2
    # PATIENT CHARACTERISTICS BY HEALTH FACILITY
    # ========================================================
    
    output$facility_summary <- gt::render_gt({
      
      # IMPORTANT:
      # Get CURRENT filtered dashboard data
      df <- data()
      
      req(df)
      req(nrow(df) > 0)
      
      
      # Clean filtered data
      df <- clean_report_data(df)
      
      
      required_columns <- c(
        "health_facility",
        "age_group",
        "age_years",
        "temp",
        "temp_cat",
        "fever_48h",
        "mal_symptoms",
        "febrile",
        # "malaria_symptomatic",
        # "tested",
        # "positive",
        "sus_malaria",
        "mal_result"
        
      )
      
      
      validate(
        need(
          all(required_columns %in% names(df)),
          "Some variables required for the facility summary are missing."
        )
      )
      
      
      # Build summary using CURRENT filtered data
      facility_table <- df %>%
        
        select(
          health_facility,
          age_group,
          age_years,
          temp,
          temp_cat,
          fever_48h,
          mal_symptoms,
          febrile,
          mal_result,
          # malaria_symptomatic,
          # tested,
          # positive,
          sus_malaria,
        ) %>%
        
        gtsummary::tbl_summary(
          
          by = health_facility,
          
          # Explicitly define variable types
          type = list(
            
            c(
              age_group,
              fever_48h,
              temp_cat,
              mal_symptoms,
              febrile,
              # malaria_symptomatic,
              # tested,
              # positive,
              mal_result,
              sus_malaria
            ) ~ "categorical",
            
            c(
              age_years,
              temp
            ) ~ "continuous"
            
          ),
          
          statistic = list(
            
            gtsummary::all_continuous() ~
              "{median} ({p25}, {p75})",
            
            gtsummary::all_categorical() ~
              "{n} ({p}%)"
            
          ),
          
          missing = "ifany",
          
          label = list(
            age_group ~ "Age Group",
            age_years ~ "Age in Years",
            temp ~ "Temperature",
            temp_cat ~ "Temperature (Grouped)",
            fever_48h ~ "Fever in Last 48 Hours",
            mal_symptoms ~ "Malaria Symptoms",
            febrile ~ "Febrile (Temp >=37.5 or reported history of fever)",
            # malaria_symptomatic ~ "Malaria Symptomatic",
            # tested ~ "Malaria Tested",
            # positive ~ "Positive Malaria Test",
            mal_result ~ "Malaria test result",
            sus_malaria ~ "Malaria results by Testing method"
          )
          
        ) %>%
        
        gtsummary::add_overall(
          last = TRUE
        ) %>%
        
        gtsummary::bold_labels()
      
      # Convert to GT
      facility_table %>%
        
        gtsummary::as_gt() %>%
        
        gt::tab_header(
          
          title = gt::md(
            "**Patient Characteristics by Health Facility**"
          ),
          
          subtitle = paste(
            "Current filtered dataset:",
            scales::comma(nrow(df)),
            "patient records"
          )
          
        ) %>%
        
        gt::tab_options(
          table.width = gt::pct(100),
          heading.align = "left",
          column_labels.font.weight = "bold",
          row_group.font.weight = "bold"
        )
      
    })
    
    
    # ========================================================
    # TABLE 3
    # PATIENT LEVEL DATA
    # ========================================================
    # 
    # output$datatable <- DT::renderDT({
    #   
    #   # Get current filtered data
    #   df <- data()
    #   
    #   req(df)
    #   
    #   
    #   DT::datatable(
    #     
    #     df,
    #     
    #     rownames = FALSE,
    #     
    #     filter = "top",
    #     
    #     options = list(
    #       pageLength = 15,
    #       scrollX = TRUE,
    #       autoWidth = TRUE,
    #       searchHighlight = TRUE,
    #       lengthMenu = c(
    #         10,
    #         20,
    #         50,
    #         100
    #       )
    #     )
    #     
    #   )
    #   
    # })
    # 
    
  })
}