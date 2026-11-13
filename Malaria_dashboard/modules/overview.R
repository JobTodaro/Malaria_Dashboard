#---------------------------------------
# UI
#---------------------------------------

overviewUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow(
      
      column(
        width = 6,
        valueBoxOutput(ns("total_visits"))
      ),
      
      column(
        width = 6,
        valueBoxOutput(ns("suspected_cases"))
      ),
      
      column(
        width = 6,
        valueBoxOutput(ns("tested"))
      ),
      
      column(
        width = 6,
        valueBoxOutput(ns("positive"))
      ),
      
      column(
        width = 6,
        valueBoxOutput(ns("testing_rate"))
      ),
      
      column(
        width = 6,
        valueBoxOutput(ns("positivity_rate"))
      )
    ),
    
    box(
      title = "Cumulative OPD Caseload by Facility",
      width = 6,
      status = "warning",
      solidHeader = TRUE,
      plotlyOutput(
        ns("facility_plot"),
        height = "350px"
      )
    ),
    
    
    fluidRow(
      box(
        title = "Monthly OPD Caseload Trends",
        width = 6,
        status = "warning",
        solidHeader = TRUE,
        plotlyOutput(
          ns("trend_plot"),
          height = "350px"
        )
      )
    
    ),

    fluidRow(
      
      box(
        title = "Malaria RDT Positivity Rate",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        plotlyOutput(
          ns("rdt_malaria_test"),
          height = "350px"
        )
      ),
      
      box(
        title = "Malaria Microscopy Positivity Rates",
        width = 6,
        solidHeader = TRUE,
        status = "primary",
        plotlyOutput(
          ns("bs_malaria_test"),
          height = "350px"
        )
      )
    )
  )
}


#---------------------------------------
# SERVER
#---------------------------------------
overviewServer <- function(id, data) {
  
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
    
    #===========================================
    # VALUE BOXES
    #===========================================
    
    output$total_visits <- renderValueBox({
      
      df <- safe_data()
      
      valueBox(
        value = scales::comma(nrow(df)),
        subtitle = "Total Visits",
        icon = icon("users"),
        color = "blue"
      )
      
    })
    
    output$suspected_cases <- renderValueBox({
      
      df <- safe_data()
      
      valueBox(
        value = scales::comma(sum(df$malaria_symptomatic == 1, na.rm = TRUE)),
        subtitle = "Suspected Malaria",
        icon = icon("bug"),
        color = "red"
      )
      
    })
    
    output$tested <- renderValueBox({
      
      df <- safe_data()
      
      valueBox(
        value = scales::comma(sum(df$tested == 1, na.rm = TRUE)),
        subtitle = "Tested",
        icon = icon("vial"),
        color = "green"
      )
      
    })
    
    output$positive <- renderValueBox({
      
      df <- safe_data()
      
      valueBox(
        value = scales::comma(sum(df$positive == 1, na.rm = TRUE)),
        subtitle = "Positive Cases",
        icon = icon("plus-circle"),
        color = "orange"
      )
      
    })
    
    output$testing_rate <- renderValueBox({
      
      df <- safe_data()
      
      suspected <- sum(df$malaria_symptomatic == 1, na.rm = TRUE)
      tested <- sum(df$tested == 1, na.rm = TRUE)
      
      rate <- ifelse(suspected == 0, 0, round(100 * tested / suspected, 1))
      
      valueBox(
        paste0(rate, "%"),
        "Testing Coverage",
        icon = icon("chart-line"),
        color = "purple"
      )
      
    })
    
    output$positivity_rate <- renderValueBox({
      
      df <- safe_data()
      
      tested <- sum(df$tested == 1, na.rm = TRUE)
      positive <- sum(df$positive == 1, na.rm = TRUE)
      
      rate <- ifelse(tested == 0, 0, round(100 * positive / tested, 1))
      
      valueBox(
        paste0(rate, "%"),
        "Positivity Rate",
        icon = icon("chart-line"),
        color = "yellow"
      )
      
    })
    
    #===========================================
    # MONTHLY OPD TREND
    #===========================================
    
    output$trend_plot <- renderPlotly({
      
      df <- safe_data()
      
      validate(
        need("visit_date" %in% names(df), "visit_date column missing."),
        need("health_facility" %in% names(df), "health_facility column missing.")
      )
      
      trend <- df %>%
        filter(!is.na(visit_date)) %>%
        mutate(month = floor_date(visit_date, "month")) %>%
        group_by(month, health_facility) %>%
        summarise(
          opd_visits = n(),
          .groups = "drop"
        )
      
      validate(
        need(nrow(trend) > 0, "No monthly data available.")
      )
      
      p <- ggplot(
        trend,
        aes(
          x = month,
          y = opd_visits,
          colour = health_facility,
          group = health_facility
        )
      ) +
        geom_line(linewidth = 1) +
        geom_point(size = 2) +
        scale_x_date(date_labels = "%b\n%Y") +
        labs(
          x = "Month",
          y = "OPD Visits",
          colour = "Facility"
        ) +
        theme_bw()
      
      ggplotly(p, tooltip = c("x", "y", "colour"))
      
    })
    
    #===========================================
    # FACILITY CASELOAD
    #===========================================
    
    output$facility_plot <- renderPlotly({
      
      df <- safe_data()
      
      facility <- df %>%
        group_by(health_facility) %>%
        summarise(
          opd_visits = n(),
          .groups = "drop"
        ) %>%
        arrange(desc(opd_visits))
      
      validate(
        need(nrow(facility) > 0, "No facility data available.")
      )
      
      p <- ggplot(
        facility,
        aes(
          reorder(health_facility, opd_visits),
          opd_visits
        )
      ) +
        geom_col(fill = "#2C7FB8") +
        geom_text(
          aes(label = opd_visits),
          hjust = -0.2,
          size = 3.5
        ) +
        coord_flip() +
        expand_limits(y = max(facility$opd_visits) * 1.15) +
        labs(
          x = "Health Facility",
          y = "OPD Visits"
        ) +
        theme_bw()+
        theme(
          legend.position = "none"
        )
      
      ggplotly(p, tooltip = c("x", "y"))
      
    })
    
    #===========================================
    # MALARIA TESTING
    #===========================================
    
    output$malaria_test <- renderPlotly({
      
      df <- safe_data()
      
      validate(
        need("sus_malaria" %in% names(df), "sus_malaria column missing.")
      )
      
      pie <- df %>%
        filter(
          !is.na(sus_malaria),
          sus_malaria != "",
          !sus_malaria %in% c("NOT tested", "Not done", "Unknown")
        ) %>%
        count(sus_malaria)
      
      validate(
        need(nrow(pie) > 0, "No malaria testing data.")
      )
      
      plot_ly(
        pie,
        labels = ~sus_malaria,
        values = ~n,
        type = "pie",
        textinfo = "label+percent"
      )
      
    })
    
    #RDT
    output$rdt_malaria_test <- renderPlotly({
      
      df <- safe_data()
      
      validate(
        need("sus_malaria" %in% names(df), "sus_malaria column missing.")
      )
      
      pie <- df %>%
        filter(
          !is.na(sus_malaria),
          sus_malaria != "",
          !sus_malaria %in% c("NOT tested", "Not done", "Unknown"),
          test_rdt==1
        ) %>%
        count(sus_malaria)
      
      validate(
        need(nrow(pie) > 0, "No malaria testing data.")
      )
      
      plot_ly(
        pie,
        labels = ~sus_malaria,
        values = ~n,
        type = "pie",
        textinfo = "label+percent"
      )
      
    })
    
    #BS
    
    output$bs_malaria_test <- renderPlotly({
      
      df <- safe_data()
      
      validate(
        need("sus_malaria" %in% names(df), "sus_malaria column missing.")
      )
      
      pie <- df %>%
        filter(
          !is.na(sus_malaria),
          sus_malaria != "",
          !sus_malaria %in% c("NOT tested", "Not done", "Unknown"),
          test_bs==1
        ) %>%
        count(sus_malaria)
      
      validate(
        need(nrow(pie) > 0, "No malaria testing data.")
      )
      
      plot_ly(
        pie,
        labels = ~sus_malaria,
        values = ~n,
        type = "pie",
        textinfo = "label+percent"
      )
      
    })
    

    
    # #===========================================
    # # MALARIA TREATMENT
    # #===========================================
    # 
    # output$malaria_tx <- renderPlotly({
    #   
    #   df <- safe_data()
    #   
    #   tx <- tibble(
    #     
    #     Regimen = c(
    #       "AL",
    #       "DHA-PPQ",
    #       "Artesunate",
    #       "Quinine Oral",
    #       "Quinine Injectable",
    #       "Other"
    #     ),
    #     
    #     Cases = c(
    #       sum(df$al == 1, na.rm = TRUE),
    #       sum(df$dha_ppq == 1, na.rm = TRUE),
    #       sum(df$artesunate == 1, na.rm = TRUE),
    #       sum(df$quinine_oral == 1, na.rm = TRUE),
    #       sum(df$quinine_paren == 1, na.rm = TRUE),
    #       sum(df$other_tx == 1, na.rm = TRUE)
    #     )
    #     
    #   )
    #   
    #   plot_ly(
    #     tx,
    #     labels = ~Regimen,
    #     values = ~Cases,
    #     type = "pie",
    #     textinfo = "label+percent"
    #   )
    #   
    # })
    
  })
  
}