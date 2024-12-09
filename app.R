
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
            img("Avenida_Malecón.jpg", height = "250px"),
            p(
              "Avenida Malecón: A famous waterfront promenade in Havana, symbolizing Cuba's cultural and historical vibrancy.",
              style = "text-align: center; font-style: italic;"
            ),
            img("Viñales_Valley.jpg", height = "250px"),
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
server <- function(input, output, session) {
  # Global Map
  output$map_global <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        data = land,
        fillColor = "lightgray",
        color = "white",
        weight = 0.5,
        fillOpacity = 0.7,
        popup = ~"World Map"
      ) %>%
      addPolygons(
        data = cuba,
        fillColor = "red",
        color = "black",
        weight = 1,
        fillOpacity = 0.8,
        popup = ~"Cuba"
      ) %>%
      addCircleMarkers(
        lng = -80,
        lat = 21.5,
        radius = 8,
        color = "blue",
        fillColor = "blue",
        fillOpacity = 1,
        popup = "Cuba Center"
      )
  })
  
  # Map of Cuba
  output$map_cuba <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.5, lat = 21.5, zoom = 7)
  })
  
  key_data <- reactive({
    WDI(
      country = "CU",
      indicator = names(indicator_choices),
      start = 2000,
      end = 2024
    )
  })
  key_data <- reactive({
    tryCatch({
      WDI(
        country = "CU",
        indicator = names(indicator_choices),
        start = 2000,
        end = 2024
      )
    }, error = function(e) {
      if (file.exists("cuba_wdi_data.csv")) {
        read.csv("cuba_wdi_data.csv")
      } else {
        stop("Error: Unable to fetch data from WDI and the local file 'cuba_wdi_data.csv' is missing.")
      }
    })
  })
  # Reactive data loading
  key_data <- reactive({
    tryCatch({
      WDI(
        country = "CU",
        indicator = names(indicator_choices),
        start = 2000,
        end = 2024
      )
    }, error = function(e) {
      if (file.exists("cuba_wdi_data.csv")) {
        read.csv("cuba_wdi_data.csv")
      } else {
        stop("Error: Unable to fetch data from WDI and the local file 'cuba_wdi_data.csv' is missing.")
      }
    })
  })
  # Render the Avenida Malecón image
  output$malecon_image <- renderImage({
    list(
      src = "Cuba/Avenida_Malecon.jpg",  # Adjust the path based on your directory structure
      contentType = "image/jpeg",
      height = "250px",
      alt = "Avenida Malecón"
    )
  }, deleteFile = FALSE)
  
  # Render the Viñales Valley image
  output$vinales_image <- renderImage({
    list(
      src = "Cuba/Vinales_Valley.jpg",  # Adjust the path based on your directory structure
      contentType = "image/jpeg",
      height = "250px",
      alt = "Viñales Valley"
    )
  }, deleteFile = FALSE)
  # Render key variable plot
  output$key_variable_plot <- renderPlot({
    data <- key_data()
    ggplot(data, aes(x = year, y = !!sym(input$indicator))) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(
        title = paste("Trend of", names(indicator_choices[which(indicator_choices == input$indicator)])),
        x = "Year",
        y = names(indicator_choices[which(indicator_choices == input$indicator)])
      ) +
      theme_minimal()
  })
  
  # Time series prediction
  output$variable_forecast_plot <- renderPlot({
    data <- key_data()
    indicator <- input$indicator
    indicator_data <- data[, c("year", indicator)]
    
    # Filter non-NA values and prepare time series
    ts_data <- ts(indicator_data[, indicator], start = min(data$year, na.rm = TRUE), frequency = 1)
    ts_data <- na.omit(ts_data)
    
    # Fit ARIMA model
    library(forecast)
    fit <- auto.arima(ts_data)
    forecast_data <- forecast(fit, h = 2)
    
    # Plot forecast
    plot(forecast_data, main = paste("Forecast for", names(indicator_choices[which(indicator_choices == indicator)])), ylab = "Value", xlab = "Year")
  })
  
  # Render explanation for time series prediction
  output$prediction_explanation <- renderUI({
    indicator_name <- names(indicator_choices[which(indicator_choices == input$indicator)])
    
    tagList(
      h4("Time Series Model Explanation"),
      p("The time series forecasting is performed using the ARIMA (AutoRegressive Integrated Moving Average) model. 
        This model is widely used for forecasting time series data and identifies the best parameters (p, d, q) automatically."),
      p("Key steps involved:"),
      tags$ul(
        tags$li("Data preprocessing: Handling missing values and converting data to a time series object."),
        tags$li("Model selection: Using the `auto.arima` function to determine the optimal parameters."),
        tags$li("Forecasting: Generating predictions for the next 2 years based on historical trends.")
      ),
      p("The formula for the ARIMA model is as follows:"),
      tags$code("ARIMA(p, d, q)"),
      p("where:"),
      tags$ul(
        tags$li("p: Number of lag observations included in the model (AutoRegressive part)."),
        tags$li("d: Number of times the raw observations are differenced to make the data stationary."),
        tags$li("q: Size of the moving average window.")
      ),
      p(paste("For the selected variable", indicator_name, ", the forecast for the next 2 years is shown in the plot."))
    )
  })
  
  
  
  
  
  # Fetch WDI data for multiple countries
  comparative_data <- reactive({
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
      rename(Country = iso2c) %>%
      pivot_longer(cols = starts_with("NY.") | starts_with("EG.") | starts_with("SP.") | starts_with("SH."),
                   names_to = "Indicator", values_to = "Value")
  })
  
  # Render plots for comparative analysis
  output$comparative_plot <- renderPlot({
    data <- comparative_data()
    
    # Validate data
    req(data)
    
    # Get selected indicator
    selected_indicator <- input$comparison_indicator
    
    # Filter data for the selected indicator
    filtered_data <- data %>%
      filter(Indicator == selected_indicator)
    
    # Cuba vs Bahamas
    cuba_bahamas <- ggplot(filtered_data %>% filter(Country %in% c("CU", "BS")),
                           aes(x = year, y = Value, color = Country)) +
      geom_line(size = 1.2) +
      scale_color_manual(
        values = c("CU" = "blue", "BS" = "red"),
        labels = c("CU" = "Cuba", "BS" = "Bahamas")
      ) +
      labs(
        title = paste("Cuba vs Bahamas -", selected_indicator),
        x = "Year",
        y = "Value",
        color = "Country"
      ) +
      theme_minimal()
    
    # Cuba vs Haiti
    cuba_haiti <- ggplot(filtered_data %>% filter(Country %in% c("CU", "HT")),
                         aes(x = year, y = Value, color = Country)) +
      geom_line(size = 1.2) +
      scale_color_manual(
        values = c("CU" = "blue", "HT" = "green"),
        labels = c("CU" = "Cuba", "HT" = "Haiti")
      ) +
      labs(
        title = paste("Cuba vs Haiti -", selected_indicator),
        x = "Year",
        y = "Value",
        color = "Country"
      ) +
      theme_minimal()
    
    # Arrange plots side by side
    gridExtra::grid.arrange(cuba_bahamas, cuba_haiti, ncol = 2)
  })
  
  # Render Neighbors Map
  output$neighbors_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = c(-77.7812, -72.2852, -76.3159),
        lat = c(21.5218, 18.9712, 24.3963),
        radius = 8,
        color = c("blue", "green", "red"),
        fillColor = c("blue", "green", "red"),
        fillOpacity = 1,
        label = c("Cuba", "Haiti", "Bahamas"),
        popup = c(
          "<b>Cuba:</b> Largest island in the Caribbean with a socialist republic.",
          "<b>Haiti:</b> Located on the island of Hispaniola, east of Cuba.",
          "<b>Bahamas:</b> An archipelago located north of Cuba."
        ),
        labelOptions = labelOptions(noHide = TRUE, direction = "top", textOnly = TRUE)
      ) %>%
      setView(lng = -77.5, lat = 21.0, zoom = 5)
  })
  # Fetch healthcare data for Cuba and World averages
  healthcare_data <- reactive({
    tryCatch({
      WDI(
        country = c("CU", "WLD"),  # Fetch data for Cuba and World
        indicator = c("SP.DYN.LE00.IN"), # Life Expectancy
        start = 2000,
        end = 2024
      ) %>%
        rename(Country = iso2c)
    }, error = function(e) {
      stop("Error: Could not fetch healthcare data")
    })
  })
  
  # Render healthcare plot
  output$healthcare_plot <- renderPlot({
    req(healthcare_data())  # Ensure data is available
    
    # Prepare data for plotting
    data <- healthcare_data()
    
    ggplot(data, aes(x = year, y = SP.DYN.LE00.IN, color = Country)) +
      geom_line(size = 1) +
      scale_color_manual(
        values = c("CU" = "blue", "WLD" = "grey"),
        labels = c("CU" = "Cuba", "WLD" = "World Average")
      ) +
      labs(
        title = "Life Expectancy at Birth: Cuba vs World",
        x = "Year",
        y = "Life Expectancy (Years)",
        color = "Country"
      ) +
      theme_minimal()
  })
  output$gdp_plot <- renderPlot({
    # Fetch GDP data for Cuba and the world
    gdp_data <- WDI(
      country = c("CU", "WLD"),
      indicator = "NY.GDP.MKTP.CD",
      start = 2000,
      end = 2024
    ) 
    
    # Plot the GDP trends
    ggplot(gdp_data, aes(x = year, y = NY.GDP.MKTP.CD, color = country)) +
      geom_line(size = 1.2) +
      scale_y_continuous(labels = scales::comma) +
      scale_color_manual(
        values = c("Cuba" = "blue", "World" = "gray"),
        labels = c("CU" = "Cuba", "1W" = "World")
      ) +
      labs(
        title = "Cuba vs World: GDP (Current US$)",
        x = "Year",
        y = "GDP (Current US$)",
        color = "Country"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold"),
        legend.position = "bottom"
      )
  })
  tourism_data <- reactive({
    WDI(
      country = "CU",
      indicator = "ST.INT.ARVL", # International tourist arrivals
      start = 2000,
      end = 2018
    )
  })
  
  output$tourism_plot <- renderPlot({
    data <- tourism_data()
    
    ggplot(data, aes(x = year, y = ST.INT.ARVL)) +
      geom_line(color = "blue", size = 1) +
      labs(
        title = "Growth in International Tourist Arrivals in Cuba",
        x = "Year",
        y = "Tourist Arrivals"
      ) +
      theme_minimal()
  })
}
shinyApp(ui = ui, server = server)
