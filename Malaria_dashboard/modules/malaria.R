malariaUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    
  fluidRow(
    box(title = "Malaria Cascade by Facility", 
        width = 12, 
        solidHeader = TRUE,
        status = "primary",
        plotlyOutput(ns("cascade_plot"), 
                     height = "850px"))
  ),
    
    fluidRow(
      # box(title = "Malaria Positivity by Facility", 
      #     width = 6, 
      #     solidHeader = TRUE,
      #     status = "danger",
      #     plotlyOutput(ns("positivity_plot"))),
      
      box(title = "Monthly Positivity Trends", 
          width = 12, 
          status = "info",
          solidHeader = TRUE,
          plotlyOutput(ns("positivity_trend")))
      
    ),
      
      fluidRow(
      box(title = "Missed Opportunities in Malaria Case Management",
          width = 12, 
          solidHeader = TRUE,
          status = "warning",
          plotlyOutput(ns("missed_opportunities"), 
                       height = "850px"))
      )
  )   
}

malariaServer <- function(id, data) {
  
  moduleServer(id, function(input, output, session) {
    
    #---------------------------------------
    # CASCADE PLOT
    #---------------------------------------
    
    output$cascade_plot <- renderPlotly({
      
      fd <- data()
      
      cascade_facility_total <- fd %>%
        group_by(health_facility) %>%
        summarise(
          opd_visits = n(),
          # symptomatic = sum(malaria_symptomatic == 1, na.rm = TRUE),
          febrile = sum(febrile =="Yes", na.rm = TRUE),
          tested = sum(tested == 1, na.rm = TRUE),
          positive = sum(positive == 1, na.rm = TRUE),
          treated = sum(treatment_category == 1, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          # prop_symptomatic = ifelse(opd_visits > 0, symptomatic / opd_visits, NA_real_),
          prop_febrile = ifelse(opd_visits>0, febrile / opd_visits, NA_real_),
          prop_tested = ifelse(febrile > 0, tested / febrile, NA_real_),
          prop_positive = ifelse(tested > 0, positive / tested, NA_real_),
          prop_treated = ifelse(positive > 0, treated / positive, NA_real_)
        )
      
      cascade_long <- cascade_facility_total %>%
        pivot_longer(
          cols = c(opd_visits, febrile, tested, positive, treated),
          names_to = "stage",
          values_to = "count"
        ) %>%
        mutate(
          stage = factor(
            stage,
            levels = c(
              "opd_visits",
              "febrile",
              "tested",
              "positive",
              "treated"
            ),
            labels = c(
              "Total Attendees",
              "febrile",
              "Tested",
              "Positive",
              "Treated"
            )
          ),
          prop = dplyr::case_when(
            stage == "febrile" ~ prop_febrile,
            stage == "Tested" ~ prop_tested,
            stage == "Positive" ~ prop_positive,
            stage == "Treated" ~ prop_treated,
            TRUE ~ NA_real_
          ),
          label = dplyr::if_else(
            is.na(prop),
            scales::comma(count),
            paste0(
              scales::comma(count),
              "\n(",
              scales::percent(prop, accuracy = 0.1),
              ")"
            )
          )
        )
      
      p <- ggplot(
          cascade_long,
          aes(
            stage,
            count,
            fill = stage,
            text = paste0(
              "Facility: ", health_facility,
              "<br>Stage: ", stage,
              "<br>Count: ", scales::comma(count),
              ifelse(
                is.na(prop),
                "",
                paste0("<br>Percent: ", scales::percent(prop, accuracy = 0.1))
              )
            )
          )
        ) +
          geom_col() +
        geom_text(
          aes(label = label),
          vjust = -0.3,
          size = 3
        ) +
        facet_wrap(~health_facility, ncol = 3) +
        theme_bw() +
        theme(
          strip.text = element_text(face = "bold"),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none"
        ) +
        labs(x = "", y = "Count")
      
      ggplotly(p, tooltip = "text")
    })
    
    
    #---------------------------------------
    # MISSED OPPORTUNITIES
    #---------------------------------------
    
    output$missed_opportunities <- renderPlotly({
      
      #--------------------------------------------------
      # 1. CALCULATE MONTHLY METRICS
      #--------------------------------------------------
      
      missed_monthly <- data() |> 
        mutate(
          opd_date = as.Date(visit_date),
          month_date = floor_date(opd_date, "month"),
          month_lab = format(month_date, "%b %Y")
        ) |> 
        
        group_by(
          health_facility,
          month_date,
          month_lab
        ) |> 
        
        summarise(
          
          #-----------------------------
          # DENOMINATORS
          #-----------------------------
          
          febrile_den = sum(
            febrile == "Yes",
            na.rm = TRUE
          ),
          
          positive_den = sum(
            positive == 1,
            na.rm = TRUE
          ),
          
          not_positive_den = sum(
            positive == 0,
            na.rm = TRUE
          ),
          
          
          #-----------------------------
          # NUMERATORS
          #-----------------------------
          
          febrile_not_tested = sum(
            febrile == "Yes" & tested == 0,
            na.rm = TRUE
          ),
          
          positive_not_treated = sum(
            positive == 1 & totaltx == 0,
            na.rm = TRUE
          ),
          
          treated_not_positive = sum(
            positive == 0 & treatment_category == 4,
            na.rm = TRUE
          ),
          
          .groups = "drop"
        ) |> 
        
        #------------------------------------------------
      # 2. CALCULATE PERCENTAGES
      #------------------------------------------------
      
      mutate(
        
        febrile_not_tested_pct = ifelse(
          febrile_den > 0,
          100 * febrile_not_tested / febrile_den,
          NA_real_
        ),
        
        positive_not_treated_pct = ifelse(
          positive_den > 0,
          100 * positive_not_treated / positive_den,
          NA_real_
        ),
        
        treated_not_positive_pct = ifelse(
          not_positive_den > 0,
          100 * treated_not_positive / not_positive_den,
          NA_real_
        )
      )
      
      
      #--------------------------------------------------
      # 3. LONG FORMAT
      #--------------------------------------------------
      
      missed_long <- missed_monthly |> 
        
        pivot_longer(
          
          cols = c(
            febrile_not_tested_pct,
            positive_not_treated_pct,
            treated_not_positive_pct
          ),
          
          names_to = "metric",
          values_to = "percentage"
        ) |> 
        
        mutate(
          
          metric_lbl = recode(
            
            metric,
            
            febrile_not_tested_pct =
              "Febrile not tested",
            
            positive_not_treated_pct =
              "Positive not treated",
            
            treated_not_positive_pct =
              "Treated but test negative"
          ),
          
          
          #-----------------------------
          # NUMERATOR
          #-----------------------------
          
          numerator = case_when(
            
            metric == "febrile_not_tested_pct" ~
              febrile_not_tested,
            
            metric == "positive_not_treated_pct" ~
              positive_not_treated,
            
            metric == "treated_not_positive_pct" ~
              treated_not_positive
          ),
          
          
          #-----------------------------
          # DENOMINATOR
          #-----------------------------
          
          denominator = case_when(
            
            metric == "febrile_not_tested_pct" ~
              febrile_den,
            
            metric == "positive_not_treated_pct" ~
              positive_den,
            
            metric == "treated_not_positive_pct" ~
              not_positive_den
          ),
          
          
          #-----------------------------
          # TOOLTIP
          #-----------------------------
          
          tooltip_text = paste0(
            "<b>Facility:</b> ",
            health_facility,
            
            "<br><b>Month:</b> ",
            month_lab,
            
            "<br><b>Metric:</b> ",
            metric_lbl,
            
            "<br><b>Numerator:</b> ",
            scales::comma(numerator),
            
            "<br><b>Denominator:</b> ",
            scales::comma(denominator),
            
            "<br><b>Percentage:</b> ",
            round(percentage, 1),
            "%"
          )
        )
      
      
      #--------------------------------------------------
      # 4. ORDER METRICS
      #--------------------------------------------------
      
      missed_long <- missed_long |> 
        mutate(
          metric_lbl = factor(
            metric_lbl,
            levels = c(
              "Febrile not tested",
              "Positive not treated",
              "Treated but test negative"
            )
          )
        )
      
      #--------------------------------------------------
      # 5. PLOT
      #--------------------------------------------------
      #--------------------------------------------------
      # 5. STACKED BAR PLOT
      #--------------------------------------------------
      
      p <- ggplot(
        missed_long,
        aes(
          x = month_date,
          y = percentage,
          fill = metric_lbl,
          text = tooltip_text
        )
      ) +
        
        # STACKED BARS
        geom_col(
          width = 20
        ) +
        
        # LABEL ONLY NON-ZERO SEGMENTS
        geom_text(
          data = missed_long |> 
            filter(numerator > 0),
          aes(
            label = paste0(
              numerator,
              " (",
              round(percentage, 1),
              "%)"
            )
          ),
          position = position_stack(vjust = 0.5),
          size = 2.8,
          fontface = "bold",
          colour = "black"
        ) +
        
        # ONE PANEL PER FACILITY
        facet_wrap(
          ~health_facility,
          ncol = 2
        ) +
        
        # X AXIS
        scale_x_date(
          date_breaks = "1 month",
          date_labels = "%b\n%Y",
          expand = expansion(
            mult = c(0.02, 0.02)
          )
        ) +
        
        # Y AXIS
        scale_y_continuous(
          breaks = seq(0, 100, 25),
          labels = function(x) {
            paste0(x, "%")
          },
          expand = expansion(
            mult = c(0, 0.08)
          )
        ) +
        
        # COLOURS
        scale_fill_manual(
          values = c(
            "Febrile not tested" = "#2C7FB8",
            "Positive not treated" = "#F03B20",
            "Treated but test negative" = "#7FCDBB"
          )
        ) +
        
        labs(
          x = "Month",
          y = "Percentage",
          fill = "Metric"
        ) +
        
        theme_bw() +
        
        theme(
          strip.text = element_text(
            face = "bold",
            size = 9
          ),
          
          axis.text.x = element_text(
            hjust = 1,
            size = 8
          ),
          
          axis.text.y = element_text(
            size = 8
          ),
          
          axis.title = element_text(
            face = "bold"
          ),
          
          panel.grid.major.x = element_blank(),
          
          panel.grid.minor = element_blank(),
          
          legend.position = "bottom"
        )
      
      
      #--------------------------------------------------
      # 6. PLOTLY
      #--------------------------------------------------
      
      ggplotly(
        p,
        tooltip = "text"
      ) |>
        layout(
          legend = list(
            orientation = "h",
            x = 0.5,
            xanchor = "center"
          )
        )
    })
      
    #---------------------------------------
    # POSITIVITY BY FACILITY
    #---------------------------------------
    
    # output$positivity_plot <- renderPlotly({
    #   
    #   positivity_facility <- data() %>%
    #     group_by(health_facility) %>%
    #     summarise(
    #       tested = sum(tested == 1, na.rm = TRUE),
    #       positive = sum(positive == 1, na.rm = TRUE),
    #       positivity = 100 * positive / tested,
    #       .groups = "drop"
    #     ) %>%
    #     filter(tested > 0) %>%
    #     arrange(desc(positivity))
    #   
    #   p <- ggplot(
    #     positivity_facility,
    #     aes(
    #       x = reorder(health_facility, positivity),
    #       y = positivity,
    #       fill = positivity,
    #       text = paste0(
    #         "<b>", health_facility, "</b>",
    #         "<br>Tested: ", tested,
    #         "<br>Positive: ", positive,
    #         "<br>Positivity: ", round(positivity,1), "%"
    #       )
    #     )
    #   ) +
    #     geom_col(width = 0.75) +
    #     geom_text(
    #       aes(label = sprintf("%.1f%%", positivity)),
    #       hjust = -0.1,
    #       size = 3
    #     ) +
    #     coord_flip() +
    #     scale_fill_gradient(
    #       low = "#9ECAE1",
    #       high = "#CB181D",
    #       name = "%"
    #     ) +
    #     expand_limits(y = max(positivity_facility$positivity, na.rm = TRUE) + 10) +
    #     labs(
    #       x = NULL,
    #       y = "Test Positivity (%)"
    #     ) +
    #     theme_bw() +
    #     theme(
    #       legend.position = "none",
    #       panel.grid.major.y = element_blank(),
    #       axis.text.y = element_text(size = 10)
    #     )
    #   
    #   ggplotly(p, tooltip = "text")
    # })
    # 
  #Monthly positivity Trends
    output$positivity_trend <- renderPlotly({
      
      pos_trend <- data() %>%
        mutate(
          opd_date = as.Date(visit_date),
          month_date = floor_date(opd_date, "month"),
          month_lab = format(month_date, "%b %Y")
        ) %>%
        
        group_by(
          health_facility,
          month_date,
          month_lab
        ) %>%
        
        summarise(
          tested = sum(tested == 1, na.rm = TRUE),
          positive = sum(positive == 1, na.rm = TRUE),
          positivity = ifelse(
            tested > 0,
            100 * positive / tested,
            NA_real_
          ),
          .groups = "drop"
        ) %>%
        
        filter(tested > 0) %>%
        
        arrange(
          health_facility,
          month_date
        )
      
      
      p <- ggplot(
        pos_trend,
        aes(
          x = month_date,
          y = positivity,
          group = health_facility,
          text = paste0(
            "<b>Facility:</b> ", health_facility,
            "<br><b>Month:</b> ", format(month_date, "%b %Y"),
            "<br><b>Positivity:</b> ", round(positivity, 1), "%",
            "<br><b>Tested:</b> ", tested,
            "<br><b>Positive:</b> ", positive
          )
        )
      ) +
        
        geom_line(
          colour = "#2C7FB8",
          linewidth = 1.2
        ) +
        
        geom_point(
          colour = "red",
          size = 2.5
        ) +
        
        facet_wrap(
          ~health_facility, ncol = 3
        ) +
        
        scale_x_date(
          date_breaks = "2 months",
          date_labels = "%b\n%Y",
          expand = expansion(mult = c(0.02, 0.02))
        ) +
        
        scale_y_continuous(
          labels = function(x) paste0(x, "%"),
          limits = c(0, 100),
          expand = expansion(mult = c(0, 0.05))
        ) +
        
        labs(
          x = NULL,
          y = "Positivity (%)"
        ) +
        
        theme_bw() +
        
        theme(
          
          strip.text = element_text(
            face = "bold"
          ),
          
          axis.text.x = element_text(
            # angle = 90,
            hjust = 1
          ),
          
          panel.grid.minor = element_blank()
        )
      
      
      ggplotly(
        p,
        tooltip = "text"
      )
    })
  })
}


