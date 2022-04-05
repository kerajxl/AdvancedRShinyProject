
library(shiny)
library(WBreader)
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)
if (!require("WDI")) install.packages("WDI")
library(WDI)
# Define UI for application that draws a histogram

#indicators_list <- read.delim2('indicators.txt', header = T, skipNul = T, sep = ",")
#countries_list <-  read.delim2('countries.txt', header = T, skipNul = T)
indicators = as.data.frame(WDIsearch(field = 'name'))
dat = WDI(indicator=indicators$indicator[1:15], country='PL', start=1960, end=2012)
unique(dat$country)
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("indicators",
                        "Select indicators you want to compar",
                        choices = indicators$name)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("firstPlot")
        )
    )
)


server <- function(input, output) {
    
    
    output$firstPlot <- renderPlot({
      
      
      #a quick plot with legend, title and lable
      
      ggplot(dat, aes(year, input$indicator, color=country)) + geom_line() 
      +     xlab('Year') + ylab('GDI per capita (Atlas Method USD)') 
      +     labs(title = "GNI Per Capita ($USD Atlas Method)")
       
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
