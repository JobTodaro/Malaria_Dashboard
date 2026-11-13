filtersUI <- function(id, data) {
  
  ns <- NS(id)
  
  tagList(
    
    dateRangeInput(
      ns("date"),
      "Select Date Range",
      start = min(as.Date(data$visit_date), na.rm = TRUE),
      end = max(as.Date(data$visit_date), na.rm = TRUE)
    ),
    
    selectizeInput(
      ns("county"),
      "County",
      choices = sort(unique(data$hf_county)),
      selected = sort(unique(data$hf_county)),
      multiple = TRUE
    ),
    
    selectizeInput(
      ns("facility"),
      "Health Facility",
      choices = sort(unique(data$health_facility)),
      selected = sort(unique(data$health_facility)),
      multiple = TRUE
    ),
    
    selectizeInput(
      ns("age"),
      "Age Group",
      choices = sort(unique(data$age_group)),
      selected = sort(unique(data$age_group)),
      multiple = TRUE
    )
  )
}


filtersServer <- function(id,
                          data,
                          role,
                          county,
                          facility){
  
  moduleServer(id, function(input, output, session){
    
    #---------------------------------------------------
    # Lock county/facility selectors according to login
    #---------------------------------------------------
    
    observe({
      
      req(role())
      
      ## County users
      if (identical(role(), "County")) {
        
        updateSelectizeInput(
          session,
          "county",
          choices = county(),
          selected = county(),
          server = TRUE
        )
        
      }
      
      ## Facility users
      if (identical(role(), "Facility")) {
        
        updateSelectizeInput(
          session,
          "county",
          choices = county(),
          selected = county(),
          server = TRUE
        )
        
        updateSelectizeInput(
          session,
          "facility",
          choices = facility(),
          selected = facility(),
          server = TRUE
        )
        
      }
      
    })
    
    #---------------------------------------------------
    # Update facilities after county selection
    #---------------------------------------------------
    
    observeEvent(input$county, {
      
      req(input$county)
      
      facilities <- data |>
        dplyr::filter(hf_county %in% input$county) |>
        dplyr::pull(health_facility) |>
        unique() |>
        sort()
      
      ## Facility users always remain on their facility
      if (identical(role(), "Facility")) {
        
        facilities <- facility()
        
      }
      
      updateSelectizeInput(
        session,
        "facility",
        choices = facilities,
        selected = facilities,
        server = TRUE
      )
      
    }, ignoreInit = FALSE)
    
    #---------------------------------------------------
    # Filtered dataset
    #---------------------------------------------------
    
    reactive({
      
      req(input$date)
      
      df <- data
      
      ## Date filter
      df <- df |>
        dplyr::filter(
          visit_date >= input$date[1],
          visit_date <= input$date[2]
        )
      
      ##---------------------------------------
      ## SECURITY (cannot be bypassed)
      ##---------------------------------------
      
      if (identical(role(), "County")) {
        
        df <- df |>
          dplyr::filter(
            hf_county == county()
          )
        
      }
      
      if (identical(role(), "Facility")) {
        
        df <- df |>
          dplyr::filter(
            hf_county == county(),
            health_facility == facility()
          )
        
      }
      
      ##---------------------------------------
      ## Dashboard filters
      ##---------------------------------------
      
      if (!is.null(input$county) && length(input$county) > 0) {
        
        df <- df |>
          dplyr::filter(
            hf_county %in% input$county
          )
        
      }
      
      if (!is.null(input$facility) && length(input$facility) > 0) {
        
        df <- df |>
          dplyr::filter(
            health_facility %in% input$facility
          )
        
      }
      
      if (!is.null(input$age) && length(input$age) > 0) {
        
        df <- df |>
          dplyr::filter(
            age_group %in% input$age
          )
        
      }
      
      df
      
    })
    
  })
  
}