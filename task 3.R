#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



big_s <- read.csv("big_stock_data.csv")
big_s$date <- as.Date(big_s$date, "%d/%m/%Y")


input <- fluidPage(
    
    sidebarLayout(
        
        sidebarPanel(
            
            checkboxGroupInput("comp", label = " Company",
                               choices = list("Apple" = 'Apple',
                                              "Google" = 'Google',
                                              "Microsoft" = 'Microsoft',
                                              "Facebook" = 'Facebook',
                                              "Amazon" =  'Amazon',
                                              "Alibaba" = 'Alibaba',
                                              "Intel"  =  'Intel',
                                              "SAP" ='SAP'),selected = "Apple"),
            
            radioButtons("perf", 
                         label = "Select one option", 
                         choices = list("Closing Price" = 'close_price',
                                        "Volume" = 'volume'),selected = 'close_price'),           
            
        ),
        
        mainPanel(
            
            plotOutput('myplot'))
        
    )
    
)

output <- function(input, output) {
    
    output$myplot <- renderPlot({
      big_s1 <- big_s %>% filter(company %in% input$comp)
        ggplot(data = big_s1) +
            geom_line(
                aes_string(x="date", y= input$perf, group = "company", col = "company")) 
        
        
    })
    
    
}


shinyApp(input, output)
