# Install required packages (run these lines only once)
# install.packages("shiny")
# install.packages("plotly")
# install.packages("tidyverse")
# install.packages("leaflet")
# install.packages("leaflet.extras")

# Load the libraries
library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)
library(leaflet)
library(leaflet.extras)

# Load the dataset
terrorism_data <- read.csv("globalterrorismdb_0718dist.csv")
population_data <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_344555.csv", skip = 4) # Adjust as needed
population_data <- population_data %>% 
  select(Country.Name, Country.Code, starts_with("X")) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year", names_prefix = "X", values_to = "population") %>%
  mutate(year = as.numeric(year))

# Ensure the year column is numeric
terrorism_data$iyear <- as.numeric(terrorism_data$iyear)

# Merge terrorism data with population data
terrorism_data <- left_join(terrorism_data, population_data, by = c("country_txt" = "Country.Name", "iyear" = "year"))

# Define the UI
ui <- fluidPage(
  titlePanel("Global Terrorism Data Explorer"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("year", "Select Year:", min = min(terrorism_data$iyear, na.rm = TRUE), max = max(terrorism_data$iyear, na.rm = TRUE), value = c(2000, 2017)),
      selectInput("country", "Select Country:", choices = c("All", unique(terrorism_data$country_txt)), selected = "All", multiple = TRUE),
      selectInput("attack_type", "Select Attack Type:", choices = c("All", unique(terrorism_data$attacktype1_txt)), selected = "All", multiple = TRUE)
    ),
    mainPanel(
      plotlyOutput("trendPlot"),
      plotlyOutput("countryPlot"),
      plotlyOutput("attackTypePlot"),
      plotlyOutput("targetPlot"),
      plotlyOutput("casualtyPlot"),
      plotlyOutput("regionPlot"),
      plotlyOutput("ratioPlot"),  # Add ratio plot output
      leafletOutput("geoMap"),
      verbatimTextOutput("summaryStats")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  # Reactive expression to filter data based on inputs
  filtered_data <- reactive({
    req(input$year)  # Ensure input$year is available
    
    data <- terrorism_data %>%
      filter(iyear >= input$year[1], iyear <= input$year[2])
    
    if (!("All" %in% input$country)) {
      data <- data %>% filter(country_txt %in% input$country)
    }
    
    if (!("All" %in% input$attack_type)) {
      data <- data %>% filter(attacktype1_txt %in% input$attack_type)
    }
    
    return(data)
  })
  
  # Render the trend plot
  output$trendPlot <- renderPlotly({
    trend_data <- filtered_data() %>%
      count(iyear)
    
    p <- ggplot(trend_data, aes(x = iyear, y = n)) +
      geom_line() +
      geom_smooth(method = "loess") +
      ggtitle("Global Terrorism Incidents Over the Years") +
      xlab("Year") +
      ylab("Number of Incidents")
    ggplotly(p)
  })
  
  # Render the country plot
  output$countryPlot <- renderPlotly({
    top_countries <- filtered_data() %>%
      count(country_txt) %>%
      top_n(10, n) %>%
      arrange(desc(n))
    
    p <- ggplot(top_countries, aes(x = reorder(country_txt, n), y = n)) +
      geom_bar(stat = "identity") +
      ggtitle("Top 10 Countries with Highest Frequency of Attacks") +
      xlab("Country") +
      ylab("Number of Attacks") +
      coord_flip()
    ggplotly(p)
  })
  
  # Render the attack type plot
  output$attackTypePlot <- renderPlotly({
    attack_type_data <- filtered_data() %>%
      count(attacktype1_txt) %>%
      arrange(desc(n))
    
    p <- ggplot(attack_type_data, aes(x = reorder(attacktype1_txt, n), y = n)) +
      geom_bar(stat = "identity") +
      ggtitle("Distribution of Attack Types") +
      xlab("Attack Type") +
      ylab("Count") +
      coord_flip()
    ggplotly(p)
  })
  
  # Render the target plot
  output$targetPlot <- renderPlotly({
    target_data <- filtered_data() %>%
      count(iyear, targtype1_txt)
    
    p <- ggplot(target_data, aes(x = iyear, y = targtype1_txt, fill = n)) +
      geom_tile() +
      scale_fill_gradient(low = "white", high = "red") +
      ggtitle("Changes in Targets Over Time") +
      xlab("Year") +
      ylab("Target Type")
    ggplotly(p)
  })
  
  # Render the casualty plot
  output$casualtyPlot <- renderPlotly({
    casualty_data <- filtered_data() %>%
      group_by(iyear) %>%
      summarise(total_killed = sum(nkill, na.rm = TRUE), total_wounded = sum(nwound, na.rm = TRUE))
    
    p <- ggplot(casualty_data, aes(x = iyear)) +
      geom_line(aes(y = total_killed, color = "Killed")) +
      geom_line(aes(y = total_wounded, color = "Wounded")) +
      ggtitle("Number of Casualties Over the Years") +
      xlab("Year") +
      ylab("Number of Casualties") +
      scale_color_manual(values = c("Killed" = "red", "Wounded" = "blue"), name = "Casualty Type")
    ggplotly(p)
  })
  
  # Render the region plot
  output$regionPlot <- renderPlotly({
    region_data <- filtered_data() %>%
      count(region_txt) %>%
      arrange(desc(n))
    
    p <- ggplot(region_data, aes(x = reorder(region_txt, n), y = n)) +
      geom_bar(stat = "identity") +
      ggtitle("Number of Attacks by Region") +
      xlab("Region") +
      ylab("Number of Attacks") +
      coord_flip()
    ggplotly(p)
  })
  
  # Render the ratio plot
  output$ratioPlot <- renderPlotly({
    ratio_data <- filtered_data() %>%
      filter(!is.na(population)) %>%
      group_by(country_txt, iyear) %>%
      summarise(attacks = n(), population = mean(population)) %>%
      mutate(ratio = (attacks / population) * 100000)
    
    top_5_countries <- filtered_data() %>%
      count(country_txt) %>%
      top_n(5, n) %>%
      pull(country_txt)
    
    ratio_data <- ratio_data %>%
      filter(country_txt %in% top_5_countries)
    
    p <- ggplot(ratio_data, aes(x = iyear, y = ratio, color = country_txt)) +
      geom_line() +
      ggtitle("Ratio of Terrorist Attacks to Population Over the Years") +
      xlab("Year") +
      ylab("Attacks per 100,000 People") +
      scale_color_discrete(name = "Country")
    ggplotly(p)
  })
  
  # Render the geographic heatmap
  output$geoMap <- renderLeaflet({
    data <- filtered_data() %>%
      filter(!is.na(latitude) & !is.na(longitude))  # Filter out missing lat/lon
    
    leaflet(data) %>%
      addTiles() %>%
      addHeatmap(
        lng = ~longitude,
        lat = ~latitude,
        intensity = ~1,
        blur = 20,
        max = 0.05,
        radius = 15
      ) %>%
      addLegend(
        "bottomright", 
        pal = colorNumeric("YlOrRd", domain = NULL),
        values = ~1,
        title = "Heatmap Intensity",
        opacity = 1
      )
  })
  
  # Render the summary statistics
  output$summaryStats <- renderPrint({
    data <- filtered_data()
    cat("Summary Statistics\n")
    cat("==================\n")
    cat("Total Number of Attacks:", nrow(data), "\n")
    cat("Average Number of Attacks per Year:", round(mean(table(data$iyear)), 2), "\n")
    cat("Top 5 Countries with Most Attacks:\n")
    top_countries <- data %>%
      count(country_txt) %>%
      top_n(5, n) %>%
      arrange(desc(n))
    print(top_countries %>% rename(Country = country_txt, `Number of Attacks` = n))
    cat("\nMost Common Attack Types:\n")
    common_attacks <- data %>%
      count(attacktype1_txt) %>%
      top_n(5, n) %>%
      arrange(desc(n))
    print(common_attacks %>% rename(`Attack Type` = attacktype1_txt, `Number of Attacks` = n))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
