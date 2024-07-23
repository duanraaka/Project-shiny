# Toulouse Business School MSc AIBA
# Authors: Arnaud AKAFFOu
# Course: Advanced R programming for Business by Prof. VIKLUND Jonas, 
# Version: v1.0



library(shiny)
library(ggplot2)
library(data.table)
library(readxl)
library(tidyverse)
library(spData) # For getting spatial data
library(sf) # For preserving spatial data
library(leaflet) # For making maps


# load the data 
df <- read_excel("Sample - Superstore update.-2.xls")
setDT(df)
#load the us map
mapData <- us_states

mainPlot = function(Discount_range) {
  # Discount_range is a vector with two elements e.g. c(0,0.1)
  # returns a plot 
  p = ggplot(data = df[Discount %in% Discount_range], 
             aes(x=Sales, y = Profit)) + 
    geom_point(aes(colour = Discount))
  return(p)
}


ui <- fluidPage(
  
  # Application title
  titlePanel("Discount vs Profit vs Sales"),

  sliderInput(inputId = "disc",label = "Discount range", value = c(0, 0.1), min = 0, max = 0.8),
  
  #scatterplot 
  plotOutput("scatterPlot"),
  
  titlePanel("Sales per Categories"),
  #Category Menu
  selectInput("Category", "Category", c("Office Supplies",
                                        "Furniture",
                                        "Technology") %>%
                append("All")),
  
  #States Menu
  selectInput("State", "State", levels(as.factor(df$State)) %>% 
                append("All") %>% # Add "All" option
                sort()), # Sort options alphabetically
  
  # Bar Chart
  plotOutput("SUBCATBar"),
  
  # Map
  leafletOutput("map"),
  
  # Table
  dataTableOutput("table")
  
)



# Define server logic
server <- function(input, output) {
  
  # call the convenience function mainPlot we created for the plot:
  output$scatterPlot <- renderPlot(mainPlot(input$disc))


# Create bar chart of brands
  output$SUBCATBar <- renderPlot({
    
    # Filter data based on selected Category
    if (input$Category != "All") {
      df <- filter(df, Category == input$Category)
    }
    
    if (input$State != "All") {
      df <- filter(df, State == input$State)
    }
    
    # Bar chart
    Subs <- group_by(df,`Sub-Category`) %>% 
      summarise(totsales = sum(Sales)) %>% 
      arrange(desc(totsales)) %>% 
      top_n(10)
    
    ggplot(Subs, aes(reorder(`Sub-Category`, totsales))) +
      geom_bar(aes(weight = totsales), fill = "tomato3") + 
      coord_flip() +
      ggtitle("Top 10 Sub Category") +
      xlab("Sub-Category") +
      ylab("Sales") +
      theme_bw(base_size = 16)

    
    
  })

#Create MAP   
  output$map <- renderLeaflet({
    
    # Filter data based on selected Category
    if (input$Category != "All") {
      df <- filter(df, Category == input$Category)
    }
    
    if (input$State != "All") {
      df <- filter(df, State == input$State)
    }
    
    # Get the total sales per states
    states <- group_by(df, State) %>% 
      summarise(totsales = sum(Sales))
    
    # Add spatial data to states dataframe
    states <- left_join(states, mapData, c("State" = "NAME"))
    
    # Create color palette for map
    pal <- colorNumeric(palette = "YlOrRd", domain = states$totsales)
    
    
    
    #Create label text for map
    map_labels <- paste("Sales from",
                        states$State, 
                        "generate", 
                        round(states$totsales, 1))
    
    # Generate basemap
    map <- leaflet() %>%
      addTiles() %>% 
      setView(-93.65, 42.0285, zoom = 4)
    
    
    
    map %>% addPolygons(data = states$geometry,
                        fillColor = pal(states$totsales),
                        fillOpacity = .7,
                        color = "grey",
                        weight = 1,
                        label = map_labels,
                        labelOptions = labelOptions(textsize = "12px")) %>%
      addLegend(pal = pal, 
                values = states$totsales,
                position = "bottomleft")
    
  })
  
  # Create data table
  output$table <- renderDataTable({
    
    # Filter data based on selected Category
    if (input$Category != "All") {
      df <- filter(df, Category == input$Category)
    }
    
    if (input$State != "All") {
      df <- filter(df, State == input$State)
    }
    
    df[, c(10, 11, 13, 15, 16, 14, 18, 19, 20, 21)]
    
  })
}

shinyApp(ui, server)



