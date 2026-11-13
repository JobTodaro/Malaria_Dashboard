#=========================================================
# MONTHLY REPORTS UI
#=========================================================

monthlyReportsUI <- function(id) {
  
  ns <- NS(id)
  
  fluidPage(
    
    #=====================================================
    # Page title
    #=====================================================
    
    h3("Monthly Reports"),
    
    
    #=====================================================
    # Facility + Reporting Period
    #=====================================================
    
    fluidRow(
      
      #---------------------------------------------------
      # Facility
      #---------------------------------------------------
      
      column(
        
        6,
        
        selectInput(
          
          ns("facility"),
          
          "Select Facility",
          
          choices = NULL
          
        )
        
      ),
      
      
      #---------------------------------------------------
      # Reporting period type
      #---------------------------------------------------
      
      column(
        
        6,
        
        radioButtons(
          
          ns("report_period_type"),
          
          "Reporting Period",
          
          choices = c(
            
            "Monthly" = "monthly",
            
            "Weekly" = "weekly",
            
            "Custom Date Range" = "date_range"
            
          ),
          
          selected = "monthly",
          
          inline = TRUE
          
        ),
        
        
        #-----------------------------------------------
        # Monthly selector
        #-----------------------------------------------
        
        conditionalPanel(
          
          condition = sprintf(
            "input['%s'] == 'monthly'",
            ns("report_period_type")
          ),
          
          dateInput(
            
            ns("report_month"),
            
            "Select Month",
            
            value = Sys.Date(),
            
            format = "yyyy-mm"
            
          )
          
        ),
        
        
        #-----------------------------------------------
        # Weekly selector
        #-----------------------------------------------
        
        conditionalPanel(
          
          condition = sprintf(
            "input['%s'] == 'weekly'",
            ns("report_period_type")
          ),
          
          dateRangeInput(
            
            ns("report_week_dates"),
            
            "Select Week",
            
            start = Sys.Date() -
              lubridate::days(
                lubridate::wday(
                  Sys.Date(),
                  week_start = 1
                ) - 1
              ),
            
            end = Sys.Date() -
              lubridate::days(
                lubridate::wday(
                  Sys.Date(),
                  week_start = 1
                ) - 1
              ) +
              lubridate::days(6),
            
            format = "dd/mm/yyyy"
            
          )
          
        ),
        
        
        #-----------------------------------------------
        # Custom date range
        #-----------------------------------------------
        
        conditionalPanel(
          
          condition = sprintf(
            "input['%s'] == 'date_range'",
            ns("report_period_type")
          ),
          
          dateRangeInput(
            
            ns("report_dates"),
            
            "Select Date Range",
            
            start = Sys.Date() - 6,
            
            end = Sys.Date(),
            
            format = "dd/mm/yyyy"
            
          )
          
        )
        
      )
      
    ),
    
    
    #=====================================================
    # Reports
    #=====================================================
    
    tabsetPanel(
      
      
      #---------------------------------------------------
      # OPD Workload
      #---------------------------------------------------
      
      tabPanel(
        
        "OPD Workload Report",
        
        downloadButton(
          
          ns("download_opd"),
          
          "Download OPD Report"
          
        ),
        
        uiOutput(
          
          ns("preview_opd")
          
        )
        
      ),
      
      
      #---------------------------------------------------
      # MOH 705
      #---------------------------------------------------
      
      tabPanel(
        
        "MOH 705 A & B",
        
        downloadButton(
          
          ns("download_705"),
          
          "Download MOH 705 Report"
          
        ),
        
        DT::dataTableOutput(
          
          ns("preview_705")
          
        )
        
      ),
      
      
      #---------------------------------------------------
      # MOH 711
      #---------------------------------------------------
      
      tabPanel(
        
        "MOH 711 A & B",
        
        downloadButton(
          
          ns("download_711"),
          
          "Download MOH 711 Report"
          
        ),
        
        DT::dataTableOutput(
          
          ns("preview_711")
          
        )
        
      ),
      
      
      #---------------------------------------------------
      # Malaria Testing
      #---------------------------------------------------
      
      tabPanel(
        
        "Malaria Testing",
        
        DTOutput(
          
          ns("malaria_testing_table")
          
        ),
        
        br(),
        
        downloadButton(
          
          ns("download_malaria_testing"),
          
          "Download"
          
        )
        
      ),
      
      
      #---------------------------------------------------
      # Malaria Treatment
      #---------------------------------------------------
      
      tabPanel(
        
        "Malaria Treatment",
        
        DTOutput(
          
          ns("malaria_treatment_table")
          
        ),
        
        br(),
        
        downloadButton(
          
          ns("download_malaria_treatment"),
          
          "Download"
          
        )
        
      )
      
    )
    
  )
  
}