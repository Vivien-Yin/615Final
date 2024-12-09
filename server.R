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