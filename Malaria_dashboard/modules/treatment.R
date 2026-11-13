treatmentUI <- function(id) {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Malaria Treatment Regimens",
      width = 4,
      solidHeader = TRUE,
      status = "primary",
      plotlyOutput(
        ns("malaria_tx"),
        height = "350px"
      )
    )
  )
}

treatmentServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    
    #===========================================
    # SAFE DATA
    #===========================================
    
    safe_data <- reactive({
      
      req(data())
      
      df <- data()
      
      validate(
        need(is.data.frame(df), "Data is not available."),
        need(nrow(df) > 0, "No records match the selected filters.")
      )
      
      if ("visit_date" %in% names(df)) {
        df$visit_date <- suppressWarnings(as.Date(df$visit_date))
      }
      
      indicator_vars <- intersect(
        c(
          "malaria_symptomatic",
          "tested",
          "positive",
          "test_rdt",
          "test_bs",
          "al",
          "dha_ppq",
          "artesunate",
          "quinine_oral",
          "quinine_paren",
          "other_tx"
        ),
        names(df)
      )
      
      df <- df %>%
        mutate(across(all_of(indicator_vars), ~ suppressWarnings(as.numeric(.))))
      
      df
      
    })
    #treatment regimens
    output$malaria_tx <- renderPlotly({
      
      df <- safe_data()
      
      tx <- tibble(
        
        Regimen = c(
          "AL",
          "DHA-PPQ",
          "Artesunate",
          "Quinine Oral",
          "Quinine Injectable",
          "Other"
        ),
        
        Cases = c(
          sum(df$al == 1, na.rm = TRUE),
          sum(df$dha_ppq == 1, na.rm = TRUE),
          sum(df$artesunate == 1, na.rm = TRUE),
          sum(df$quinine_oral == 1, na.rm = TRUE),
          sum(df$quinine_paren == 1, na.rm = TRUE),
          sum(df$other_tx == 1, na.rm = TRUE)
        )
        
      )
      
      plot_ly(
        tx,
        labels = ~Regimen,
        values = ~Cases,
        type = "pie",
        textinfo = "label+percent"
      )
      
    })  
    })
}
