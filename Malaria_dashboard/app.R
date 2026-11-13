source("global.R")
source("load_credentials.R")
source("modules/filters.R",local = TRUE)
source("modules/overview.R", local = TRUE)
source("modules/malaria.R", local = TRUE)
source("modules/diagnosis.R", local = TRUE)
source("modules/treatment.R", local = TRUE)
source("modules/table.R", local = TRUE)
source("modules/queries.R", local = TRUE)
# source("modules/daily_summary.R", local = TRUE)
source("modules/reports_helpers.R", local = TRUE)
source("modules/reports_opd.R", local = TRUE)
source("modules/reports_moh705.R", local = TRUE)
source("modules/reports_moh711.R", local = TRUE)
source("modules/monthly_reports_ui.R", local = TRUE)
source("modules/monthly_reports_server.R", local = TRUE)
  
ui <- secure_app(
  dashboardPage(
  
  dashboardHeader(
    title = "Malaria Surveillance Dashboard",
    tags$li(
      class = "dropdown",
      textOutput("logged_user")
    )
  ), 
  
  dashboardSidebar(
    
    sidebarMenu(
      menuItem(
        "Overview",
        tabName = "overview",
        icon = icon("dashboard")
      ),
      
      # menuItem(
      #   "Daily Summary",
      #   tabName = "daily_summary",
      #   icon = icon("calendar-days")
      # ),
      # 
      menuItem(
        "Malaria",
        tabName = "malaria",
        icon = icon("mosquito")
      ),
      
      menuItem(
        "Diagnosis",
        tabName = "diagnosis",
        icon = icon("stethoscope")
      ),
      
      menuItem(
        "Treatment",
        tabName = "treatment",
        icon = icon("pills")
      ),
      
      menuItem(
        "Data Table",
        tabName = "table",
        icon = icon("table")
      ),
      
      menuItem(
        "Data Quality Queries",
        tabName = "queries",
        icon = icon("circle-question")
      ),
      menuItem(
        "Monthly Reports ", 
        tabName = "monthly_reports",
        icon = icon("chart-column")
      )
    ),
    
    hr(),
    
  # Filters appear once only, inside the sidebar
    filtersUI(
      id = "filters",
      data = opd_merged
    )
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(
        tabName = "overview",
        overviewUI("overview")
      ),
      
      # tabItem(
      #   tabName = "daily_summary",
      #   dailySummaryUI("daily_summary")
      # ),
      # 
      tabItem(
        tabName = "malaria",
        malariaUI("malaria")
      ),
      
      tabItem(
        tabName = "diagnosis",
        diagnosisUI("diagnosis")
      ),
      
      tabItem(
        tabName = "treatment",
        treatmentUI("treatment")
      ),
      
      tabItem(
        tabName = "table",
        tableUI("table")
      ),
      
      tabItem(
        tabName = "queries",
        queriesUI("queries")
      ),
      
      tabItem(
        tabName = "monthly_reports",
        monthlyReportsUI("monthly_reports")
      )
    )
  )
)
)

server <- function(input, output, session) {
  
  #---------------------------------------
  # User authentication
  #---------------------------------------
  
  auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  #---------------------------------------
  # Logged in user information
  #---------------------------------------
  
  current_user <- reactive({
    auth$user
  })
  
  current_credentials <- reactive({
    
    req(auth$user)
    
    credentials |>
      dplyr::filter(user == auth$user)
    
  })
  
  current_role <- reactive({
    
    current_credentials()$role[[1]]
    
  })
  
  current_county <- reactive({
    
    current_credentials()$county[[1]]
    
  })
  
  current_facility <- reactive({
    
    current_credentials()$facility[[1]]
    
  })

  #---------------------------------------
  # set filtered OPD dataset
  #---------------------------------------
  
  filtered <- filtersServer(
    id = "filters",
    data = opd_merged,
    role = current_role,
    county = current_county,
    facility = current_facility
  )
  
  # #---------------------------------------
  # # Daily summary dataset
  # #---------------------------------------
  # 
  # daily_summary_data <- reactive({
  #   daily_summary
  # })
  # 
  # 
  #---------------------------------------
  # Dashboard modules
  #---------------------------------------
  
  overviewServer(
    id = "overview",
    data = filtered
  )
  
  # dailySummaryServer(
  #   id = "daily_summary",
  #   daily_data = daily_summary_data
  # )
  # 
  malariaServer(
    id = "malaria",
    data = filtered
  )
  
  diagnosisServer(
    id = "diagnosis",
    data = filtered
  )
  
  treatmentServer(
    id = "treatment",
    data = filtered
  )
  
  tableServer(
    id = "table",
    data = filtered
  )
  
  queriesServer(
    id = "queries",
    data = filtered
  )
  monthlyReportsServer(
    id = "monthly_reports",
    data = filtered
  )
  
  output$logged_user <- renderText({
    
    paste(
      "Logged in as:",
      current_user(),
      "|",
      current_role()
    )
    
  })
  
}

shinyApp(ui, server)