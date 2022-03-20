library (readr)
library (ggmap)
library (ggplot2)
library(shiny)



crime_d <- read.csv("crime_data.csv")
crime_d$date <- as.Date(crime_d$date, "%d/%m/%Y")


input <- fluidPage(
  
  sidebarLayout(
    
    sidebarPanel(
      
      checkboxGroupInput("offs", label = "offense",
                         choices = list("murder" = 'murder',
                                        "robbery" = 'robbery',
                                        "aggravated assault" = 'aggravated assault',
                                        "auto theft" = 'auto theft',
                                        "theft" =  'theft')
                         ,selected = "murder")
      
    ),
    
    mainPanel(
      
      plotOutput('myplot'))
    
  )
  
)

output <- function(input, output) {
  
  output$myplot <- renderPlot({
    
    bbox = c(left=-95.8, bottom=29.4, right=-95.0, top=30.0)
    map <- get_stamenmap(bbox, zoom = 10, source="stamen")
    
    ggmap(map) + stat_density2d(data = subset(big_s, offense == input$offs),
                                aes(x = lon, y = lat, fill = ..level.., alpha = ..level..), geom = 'polygon') +
      labs(color = '') + 
      facet_wrap(~ offense) 
    
  })
  
  
}


shinyApp(input, output)