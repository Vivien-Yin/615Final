library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(WDI)
library(ggplot2)
library(osmdata)
library(dplyr)
library(tidyr)
comparative_data <- reactive({
  tryCatch({
    WDI(
      country = c("CU", "BS", "HT"),
      indicator = c(
        "NY.GDP.MKTP.CD",    # GDP
        "EG.USE.ELEC.KH.PC", # Electricity consumption
        "SP.POP.TOTL",       # Population
        "SH.ALC.PCAP.LI"     # Alcohol consumption
      ),
      start = 2000,
      end = 2024
    ) %>%
      rename(Country = iso2c)
  }, error = function(e) {
    stop("Error: Could not fetch comparative data")
  })
})

ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bd/Flag_of_Cuba.svg/1280px-Flag_of_Cuba.svg.png", 
               height = "30px", style = "margin-right: 10px;"),
      "Cuba"
    )
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Description", tabName = "description", icon = icon("info-circle")),
      menuItem("Key Variables", tabName = "key_variables", icon = icon("list")),
      menuItem("Comparative Analysis", tabName = "comparative_analysis", icon = icon("chart-bar")),
      menuItem("SWOT Analysis", tabName = "swot", icon = icon("chart-line")),
      menuItem("References", tabName = "references", icon = icon("book"))
    )
  ),
  dashboardBody(
    tabItems(
      # Description Tab
      tabItem(
        tabName = "description",
        h2("Cuba Description"),
        tabsetPanel(
          tabPanel(
            "Global Location",
            leafletOutput("map_global", height = 400)
          ),
          tabPanel(
            "Map of Cuba",
            leafletOutput("map_cuba", height = 400)
          ),
          tabPanel(
            "General Description",
            h3("General Description"),
            p("Cuba is a socialist republic governed by the Communist Party of Cuba, with a one-party system and leadership centralized under the President and Prime Minister. Its economy is a mixed socialist model, dominated by state-controlled industries such as tourism, agriculture, and biotechnology, while facing challenges like the U.S. embargo and inefficiencies in state-run sectors. The population, approximately 11 million, reflects a mix of Afro-Cuban, European, and Mestizo heritage, with Spanish as the official language. Cuba boasts high literacy rates and universal healthcare, contributing to one of the highest life expectancies in the region. Geographically, it is the largest island in the Caribbean, featuring tropical climates and diverse ecosystems, though it is vulnerable to hurricanes and coastal erosion."),
            h4("Highlights:"),
            p("Below are two iconic locations in Cuba that showcase the country's natural beauty and cultural heritage:"),
            imageOutput("Cuba/Avenida_Malecón.jpg", height = "250px"),
            p(
              "Avenida Malecón: A famous waterfront promenade in Havana, symbolizing Cuba's cultural and historical vibrancy.",
              style = "text-align: center; font-style: italic;"
            ),
            imageOutput("Cuba/Viñales_Valley.jpg", height = "250px"),
            p(
              "Viñales Valley: A stunning UNESCO World Heritage Site, known for its distinctive limestone hills (mogotes) and traditional tobacco farming.",
              style = "text-align: center; font-style: italic;"
            )
          )
        )
      ),
      # Key Variables Tab
      tabItem(
        tabName = "key_variables",
        h2("Key Variables & Prediction"),
        selectInput(
          "indicator",
          "Choose an Indicator:",
          choices = indicator_choices,
          selected = "NY.GDP.MKTP.CD"
        ),
        plotOutput("key_variable_plot"),
        h3("Time Series Prediction"),
        p("This section forecasts the selected variable for the next 2 years using time series models."),
        plotOutput("variable_forecast_plot"),
        uiOutput("prediction_explanation")
      ),
      # Comparative Analysis Tab
      tabItem(
        tabName = "comparative_analysis",
        h2("Comparative Analysis"),
        tabsetPanel(
          # Neighbors of Cuba tab
          tabPanel(
            "Neighbors of Cuba",
            h3("Neighbors of Cuba"),
            p("Cuba's closest neighbors are the Bahamas to the north and Haiti to the east. Both nations share the Caribbean Sea's cultural and geographical proximity with Cuba."),
            leafletOutput("neighbors_map", height = 400),
            p("The map above highlights the locations of Cuba, the Bahamas, and Haiti within the Caribbean region.")
          ),
          # Comparison of Key Variables tab
          tabPanel(
            "Comparison of Key Variables",
            h3("Comparison of Key Variables"),
            selectInput(
              "comparison_indicator",
              "Choose an Indicator:",
              choices = list(
                "GDP (Current US$)" = "NY.GDP.MKTP.CD",
                "Electricity Consumption (kWh per Capita)" = "EG.USE.ELEC.KH.PC",
                "Population (Total)" = "SP.POP.TOTL",
                "Alcohol Consumption (Liters per Capita)" = "SH.ALC.PCAP.LI"
              ),
              selected = "NY.GDP.MKTP.CD"
            ),
            plotOutput("comparative_plot", height = "500px")
          )
        )
      ),
      # SWOT Analysis Tab
      tabItem(
        tabName = "swot",
        h2("SWOT Analysis"),
        tabsetPanel(
          # Strengths Tab
          tabPanel(
            "Strengths",
            h3("Strengths: Healthcare"),
            p("Cuba is globally recognized for its healthcare system, particularly its high life expectancy at birth."),
            plotOutput("healthcare_plot", height = "400px"),
            p("The above chart compares Cuba's life expectancy at birth with the global average over the years.")
          ),
          # Weaknesses Tab
          tabPanel(
            "Weaknesses",
            h3("Weaknesses: Economic Challenges"),
            p("Cuba's GDP growth has been relatively constrained compared to the global average."),
            plotOutput("gdp_plot"),
            p("The plot compares Cuba's GDP (current US$) with the world average over the past two decades. It looks like a flat line of Cuba's GDP which is due to a mismatch in the scale of the GDP values for Cuba and the World. Cuba’s GDP values are much smaller compared to the World’s GDP values, so they appear almost like a straight line at the bottom when plotted on the same y-axis.")
          ),
          # Opportunities
          tabPanel(
            "Opportunities",
            h3("Opportunities: Growth in Tourism"),
            p("Cuba's tourism industry has shown significant growth over the past 20 years."),
            plotOutput("tourism_plot"),
            p("The plot illustrates the trend in international tourist arrivals in Cuba over the past two decades.")
          ),
          # Threats
          tabPanel(
            "Threats",
            h3("Threats: Political Instability"),
            p("Cuba's political challenges and instability have had substantial impacts on its economic and social development."),
            p("Political instability in Cuba has affected foreign investments and economic growth. Despite these challenges, efforts are ongoing to improve stability and attract global partnerships.")
          )
        )
      ),
      # References Tab
      tabItem(
        tabName = "references",
        h2("References"),
        p("1. World Bank: Cuba Economic Indicators."),
        p("2. CIA World Factbook: Cuba."),
        p("3. UNESCO: Cultural Heritage Sites in Cuba."),
        p("4. Open AI Chat GPT")
      )
    )
  )
)