#------------------------------------------------
# DIAGNOSIS MODULE UI
#------------------------------------------------

diagnosisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      box(
        title = "Top 5 Diagnoses (Under 5 Years)",
        width = 12,
        status = "primary",
        solidHeader = TRUE,
        plotlyOutput(ns("diag_under5_plot"), height = "600px")
      )
    ),
    
    fluidRow(
      box(
        title = "Top 5 Diagnoses (Over 5 Years)",
        width = 12,
        status = "success",
        solidHeader = TRUE,
        plotlyOutput(ns("diag_over5_plot"), height = "600px")
      )
    )
    
    # fluidRow(
    #   box(
    #     title = "Referral Pattern by Facility",
    #     width = 12,
    #     status = "warning",
    #     solidHeader = TRUE,
    #     plotlyOutput(ns("referral_plot"), height = "550px")
    #   )
    # )
  )
}

#------------------------------------------------
# DIAGNOSIS MODULE SERVER
#------------------------------------------------

diagnosisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Prepare top 5 diagnoses per facility
    prepare_top_diagnoses <- function(df, selected_age_group) {
      validate(
        need("age_group" %in% names(df), "Column 'age_group' is missing."),
        need("health_facility" %in% names(df), "Column 'health_facility' is missing."),
        need("diagnosis1" %in% names(df), "Column 'diagnosis1' is missing.")
      )
      
      diag_data <- df |>
        dplyr::filter(age_group == selected_age_group) |>
        dplyr::mutate(
          diagnosis = stringr::str_squish(stringr::str_to_title(as.character(diagnosis1))),
          diagnosis = dplyr::na_if(diagnosis, ""),
          diagnosis = dplyr::na_if(diagnosis, "Na"),
          diagnosis = dplyr::na_if(diagnosis, "N/A")
        ) |>
        dplyr::filter(!is.na(diagnosis)) |>
        dplyr::count(health_facility, diagnosis, name = "n")
      
      totals <- diag_data |>
        dplyr::group_by(health_facility) |>
        dplyr::summarise(total_all = sum(n), .groups = "drop")
      
      top5 <- diag_data |>
        dplyr::left_join(totals, by = "health_facility") |>
        dplyr::mutate(pct = n / total_all) |>
        dplyr::group_by(health_facility) |>
        dplyr::slice_max(order_by = pct, n = 5, with_ties = FALSE) |>
        dplyr::ungroup() |>
        dplyr::mutate(
          diagnosis_ordered = tidytext::reorder_within(diagnosis, pct, health_facility),
          label_pct = scales::percent(pct, accuracy = 0.1)
        )
      
      top5
    }
    
    # Create diagnosis plot
    make_diagnosis_plot <- function(df, selected_age_group) {
      top5 <- prepare_top_diagnoses(df, selected_age_group)
      
      validate(
        need(nrow(top5) > 0, paste0("No diagnosis records available for ", selected_age_group))
      )
      
      p <- ggplot2::ggplot(
        top5,
        ggplot2::aes(
          x = diagnosis_ordered,
          y = pct,
          fill = diagnosis,
          text = paste0(
            "Facility: ", health_facility,
            "<br>Diagnosis: ", diagnosis,
            "<br>Cases: ", scales::comma(n),
            "<br>Total diagnoses: ", scales::comma(total_all),
            "<br>Percentage: ", scales::percent(pct, accuracy = 0.1)
          )
        )
      ) +
        ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
        ggplot2::geom_text(ggplot2::aes(label = label_pct), hjust = -0.1, size = 3.5) +
        ggplot2::coord_flip(clip = "off") +
        ggplot2::facet_wrap(~health_facility, scales = "free_y") +
        tidytext::scale_x_reordered() +
        ggplot2::scale_y_continuous(labels = scales::percent_format(), expand = ggplot2::expansion(mult = c(0, 0.15))) +
        ggplot2::labs(x = "Diagnosis", y = "Percentage of Diagnoses") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          strip.text = ggplot2::element_text(face = "bold"),
          axis.title = ggplot2::element_text(face = "bold"),
          axis.text = ggplot2::element_text(face = "bold"),
          plot.margin = ggplot2::margin(t = 5, r = 35, b = 5, l = 5)
        )
      
      plotly::ggplotly(p, tooltip = "text")|>
      plotly::layout(showlegend = FALSE)
    }
    
    # Under 5 plot
    output$diag_under5_plot <- plotly::renderPlotly({
      make_diagnosis_plot(df = data(), selected_age_group = "Under 5")
    })
    
    # Over 5 plot
    output$diag_over5_plot <- plotly::renderPlotly({
      make_diagnosis_plot(df = data(), selected_age_group = "Over 5")
    })
    
    # Referral pattern plot
    # output$referral_plot <- plotly::renderPlotly({
    #   df <- data()
    #   
    #   referral_candidates <- c("referred","referral","referral_status","referred_out","referred_to","referral_facility")
    #   referral_column <- referral_candidates[referral_candidates %in% names(df)][1]
    #   
    #   validate(need(!is.na(referral_column), "No referral column found in dataset."))
    #   
    #   referral_data <- df |>
    #     dplyr::mutate(
    #       referral_status = dplyr::case_when(
    #         as.character(.data[[referral_column]]) %in% c("1","Yes","YES","Referred","Referred Out","Referred out") ~ "Referred",
    #         as.character(.data[[referral_column]]) %in% c("0","No","NO","Not Referred","Not referred","") ~ "Not Referred",
    #         is.na(.data[[referral_column]]) ~ "Missing",
    #         TRUE ~ "Other"
    #       )
    #     ) |>
    #     dplyr::count(health_facility, referral_status, name = "n") |>
    #     dplyr::group_by(health_facility) |>
    #     dplyr::mutate(pct = n / sum(n)) |>
    #     dplyr::ungroup()
    #   
    #   validate(need(nrow(referral_data) > 0, "No referral records available."))
    #   
    #   p <- ggplot2::ggplot(
    #     referral_data,
    #     ggplot2::aes(
    #       x = health_facility,
    #       y = pct,
    #       fill = referral_status,
    #       text = paste0(
    #         "Facility: ", health_facility,
    #         "<br>Referral status: ", referral_status,
    #         "<br>Records: ", scales::comma(n),
    #         "<br>Percentage: ", scales::percent(pct, accuracy = 0.1)
    #       )
    #     )
    #   ) +
    #     ggplot2::geom_col() +
    #     ggplot2::coord_flip() +
    #     ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    #     ggplot2::labs(x = "Health Facility", y = "Percentage of OPD Visits", fill = "Referral Status") +
    #     ggplot2::theme_bw()
    #   
    #   plotly::ggplotly(p, tooltip = "text")
    # })
  })
}
