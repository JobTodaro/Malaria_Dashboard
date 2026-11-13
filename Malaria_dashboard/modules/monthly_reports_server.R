#=========================================================
# MONTHLY REPORTS SERVER
#=========================================================

monthlyReportsServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    
    #=====================================================
    # POPULATE FACILITY LIST
    #=====================================================
    
    observeEvent(data(), {
      
      updateSelectInput(
        
        session,
        
        "facility",
        
        choices = c(
          
          "All Facilities",
          
          sort(
            unique(
              data()$health_facility
            )
          )
          
        )
        
      )
      
    })
    
    
    #=====================================================
    # SELECTED REPORTING PERIOD
    #=====================================================
    
    selected_period <- reactive({
      
      req(
        input$report_period_type
      )
      
      
      #---------------------------------------------------
      # MONTHLY
      #---------------------------------------------------
      
      if (
        input$report_period_type == "monthly"
      ) {
        
        req(
          input$report_month
        )
        
        period <- get_reporting_period(
          
          report_month = input$report_month,
          
          period_type = "monthly"
          
        )
        
      }
      
      
      #---------------------------------------------------
      # WEEKLY
      #---------------------------------------------------
      
      else if (
        input$report_period_type == "weekly"
      ) {
        
        req(
          input$report_week_dates
        )
        
        period <- get_reporting_period(
          
          start_date = input$report_week_dates[1],
          
          end_date = input$report_week_dates[2],
          
          period_type = "date_range"
          
        )
        
      }
      
      
      #---------------------------------------------------
      # CUSTOM DATE RANGE
      #---------------------------------------------------
      
      else if (
        input$report_period_type == "date_range"
      ) {
        
        req(
          input$report_dates
        )
        
        period <- get_reporting_period(
          
          start_date = input$report_dates[1],
          
          end_date = input$report_dates[2],
          
          period_type = "date_range"
          
        )
        
      }
      
      
      #---------------------------------------------------
      # Return selected period
      #---------------------------------------------------
      
      period
      
    })
    
    
    #=====================================================
    # REPORTING PERIOD LABEL
    #=====================================================
    
    report_period_label <- reactive({
      
      period <- selected_period()
      
      get_reporting_period_label(
        period
      )
      
    })
    
    
    #=====================================================
    # PREPARED DATA FOR SELECTED PERIOD
    #
    # This is useful for malaria summaries and other
    # reports that need the already-filtered dataset.
    #=====================================================
    
    period_data <- reactive({
      
      period <- selected_period()
      
      prepare_opd_data(
        
        data = data(),
        
        start_date = period$start,
        
        end_date = period$end,
        
        period_type = "date_range"
        
      )
      
    })
    
    
    #=====================================================
    # OPD REPORT
    #=====================================================
    
    opd_report <- reactive({
      
      period <- selected_period()
      
      reports_opd(
        
        opd_data = data(),
        
        start_date = period$start,
        
        end_date = period$end,
        
        period_type = "date_range"
        
      )
      
    })
    
    
    #=====================================================
    # MOH 705 REPORT
    #=====================================================
    
    moh705_report <- reactive({
      
      period <- selected_period()
      
      reports_moh705(
        
        opd_data = data(),
        
        start_date = period$start,
        
        end_date = period$end,
        
        period_type = "date_range"
        
      )
      
    })
    
    
    #=====================================================
    # MOH 711 REPORT
    #=====================================================
    
    moh711_report <- reactive({
      
      period <- selected_period()
      
      tryCatch({
        
        reports_moh711(
          
          opd_data = data(),
          
          start_date = period$start,
          
          end_date = period$end,
          
          period_type = "date_range"
          
        )
        
      }, error = function(e) {
        
        print(e)
        
        NULL
        
      })
      
    })
    
    
    #=====================================================
    # OPD REPORT PREVIEW
    #=====================================================
    
    output$preview_opd <- renderUI({
      
      req(
        input$facility
      )
      
      ft <- create_opd_flextable(
        
        report = opd_report(),
        
        facility = input$facility
        
      )
      
      flextable::htmltools_value(
        ft
      )
      
    })
    
    
    #=====================================================
    # OPD REPORT DOWNLOAD
    #=====================================================
    
    output$download_opd <- downloadHandler(
      
      filename = function() {
        
        paste0(
          
          "OPD_Report_",
          
          gsub(
            " ",
            "_",
            report_period_label()
          ),
          
          ".docx"
          
        )
        
      },
      
      content = function(file) {
        
        export_opd_report(
          
          report = opd_report(),
          
          facility = input$facility,
          
          file = file
          
        )
        
      }
      
    )
    
    
    #=====================================================
    # MOH 705 PREVIEW
    #=====================================================
    
    output$preview_705 <- DT::renderDT({
      DT::datatable(
        preview_moh705(
          report = moh705_report(),
          facility = input$facility
        ),
        options = list(
          scrollX = TRUE,
          pageLength = 20
        ),
        rownames = FALSE
      )
    })
    
    
    #=====================================================
    # MOH 705 DOWNLOAD
    #=====================================================
    
    output$download_705 <- downloadHandler(
      
      filename = function() {
        
        paste0(
          
          "MOH_705_",
          
          gsub(
            " ",
            "_",
            report_period_label()
          ),
          
          ".xlsx"
          
        )
        
      },
      
      content = function(file) {
        
        export_moh705(
          
          report = moh705_report(),
          
          facility = input$facility,
          
          file = file
          
        )
        
      }
      
    )
    
    
    #=====================================================
    # MOH 711 PREVIEW
    #=====================================================
    
    output$preview_711 <- DT::renderDT({
      DT::datatable(
        preview_moh711(
          report = moh711_report(),
          facility = input$facility
        ),
        options = list(
          scrollX = TRUE,
          pageLength = 20
        ),
        rownames = FALSE
      )
    })
    
    
    #=====================================================
    # MOH 711 DOWNLOAD
    #=====================================================
    
    output$download_711 <- downloadHandler(
      
      filename = function() {
        
        paste0(
          
          "MOH_711_",
          
          gsub(
            " ",
            "_",
            report_period_label()
          ),
          
          ".xlsx"
          
        )
        
      },
      
      content = function(file) {
        
        export_moh711(
          
          report = moh711_report(),
          
          facility = input$facility,
          
          file = file
          
        )
        
      }
      
    )
    
    
    #=====================================================
    # MALARIA TESTING SUMMARY
    #=====================================================
    
    malaria_testing_summary <- reactive({
      
      req(
        input$facility
      )
      
      #-----------------------------------------------
      # Use data already filtered to selected period
      #-----------------------------------------------
      
      df <- period_data()
      
      
      #-----------------------------------------------
      # Filter facility
      #-----------------------------------------------
      
      if (
        input$facility != "All Facilities"
      ) {
        
        df <- df %>%
          
          dplyr::filter(
            
            health_facility ==
              input$facility
            
          )
        
      }
      
      
      #-----------------------------------------------
      # Malaria testing summary
      #-----------------------------------------------
      
      result <- df %>%
        
        dplyr::filter(
          
          !is.na(sus_malaria),
          
          sus_malaria != "",
          
          !sus_malaria %in% c(
            
            "NOT tested",
            
            "Not done",
            
            "Unknown"
            
          )
          
        ) %>%
        
        dplyr::count(
          
          sus_malaria,
          
          name = "Cases"
          
        )
      
      
      #-----------------------------------------------
      # Add percentages
      #-----------------------------------------------
      
      if (
        nrow(result) > 0
      ) {
        
        result <- result %>%
          
          dplyr::mutate(
            
            Percent = round(
              
              100 * Cases /
                sum(Cases),
              
              1
              
            )
            
          )
        
      } else {
        
        result <- tibble::tibble(
          
          sus_malaria = character(),
          
          Cases = numeric(),
          
          Percent = numeric()
          
        )
        
      }
      
      
      result
      
    })
    
    
    #=====================================================
    # MALARIA TREATMENT SUMMARY
    #=====================================================
    
    malaria_treatment_summary <- reactive({
      
      req(
        input$facility
      )
      
      
      #-----------------------------------------------
      # Use data already filtered to selected period
      #-----------------------------------------------
      
      df <- period_data()
      
      
      #-----------------------------------------------
      # Filter facility
      #-----------------------------------------------
      
      if (
        input$facility != "All Facilities"
      ) {
        
        df <- df %>%
          
          dplyr::filter(
            
            health_facility ==
              input$facility
            
          )
        
      }
      
      
      #-----------------------------------------------
      # Treatment summary
      #-----------------------------------------------
      
      result <- tibble::tibble(
        
        Regimen = c(
          
          "AL",
          
          "DHA-PPQ",
          
          "Artesunate",
          
          "Quinine (Oral)",
          
          "Quinine (Parenteral)",
          
          "Other"
          
        ),
        
        Cases = c(
          
          sum(
            df$al == 1,
            na.rm = TRUE
          ),
          
          sum(
            df$dha_ppq == 1,
            na.rm = TRUE
          ),
          
          sum(
            df$artesunate == 1,
            na.rm = TRUE
          ),
          
          sum(
            df$quinine_oral == 1,
            na.rm = TRUE
          ),
          
          sum(
            df$quinine_paren == 1,
            na.rm = TRUE
          ),
          
          sum(
            df$other_tx == 1,
            na.rm = TRUE
          )
          
        )
        
      )
      
      
      #-----------------------------------------------
      # Add percentages
      #-----------------------------------------------
      
      total_cases <- sum(
        result$Cases,
        na.rm = TRUE
      )
      
      
      if (
        total_cases > 0
      ) {
        
        result <- result %>%
          
          dplyr::mutate(
            
            Percent = round(
              
              100 * Cases /
                total_cases,
              
              1
              
            )
            
          )
        
      } else {
        
        result <- result %>%
          
          dplyr::mutate(
            
            Percent = 0
            
          )
        
      }
      
      
      result
      
    })
    
    
    #=====================================================
    # MALARIA TESTING TABLE
    #=====================================================
    
    output$malaria_testing_table <- DT::renderDT({
      
      DT::datatable(
        
        malaria_testing_summary(),
        
        rownames = FALSE,
        
        options = list(
          
          pageLength = 10,
          
          dom = "tip"
          
        )
        
      )
      
    })
    
    
    #=====================================================
    # MALARIA TREATMENT TABLE
    #=====================================================
    
    output$malaria_treatment_table <- DT::renderDT({
      
      DT::datatable(
        
        malaria_treatment_summary(),
        
        rownames = FALSE,
        
        options = list(
          
          pageLength = 10,
          
          dom = "tip"
          
        )
        
      )
      
    })
    
    
    #=====================================================
    # MALARIA TESTING DOWNLOAD
    #=====================================================
    
    output$download_malaria_testing <- downloadHandler(
      
      filename = function() {
        
        paste0(
          
          "Malaria_Testing_",
          
          gsub(
            " ",
            "_",
            report_period_label()
          ),
          
          ".csv"
          
        )
        
      },
      
      content = function(file) {
        
        write.csv(
          
          malaria_testing_summary(),
          
          file,
          
          row.names = FALSE
          
        )
        
      }
      
    )
    
    
    #=====================================================
    # MALARIA TREATMENT DOWNLOAD
    #=====================================================
    
    output$download_malaria_treatment <- downloadHandler(
      
      filename = function() {
        
        paste0(
          
          "Malaria_Treatment_",
          
          gsub(
            " ",
            "_",
            report_period_label()
          ),
          
          ".csv"
          
        )
        
      },
      
      content = function(file) {
        
        write.csv(
          
          malaria_treatment_summary(),
          
          file,
          
          row.names = FALSE
          
        )
        
      }
      
    )
    
    
  })
  
}