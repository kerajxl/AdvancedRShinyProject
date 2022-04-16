
library(shiny)
library(shinyWidgets)
library(readxl)
library(devtools)
library(tidyverse)
install_github('https://github.com/kerajxl/WBreader') 
#install.packages('gapminder')
library(WBreader)
library(rlang)
library(gganimate)
library(DT)
library(tweenr)
library(magick)
library(gifski)
library(magrittr)
library(countrycode)
library(grid)
devtools::install_github("ellisp/ggflags")
library(ggflags)
library(gapminder)


setwd("C:/Users/leski/OneDrive/Pulpit/DS/APR/Advanced_R_project")

data <- WBreader::wb_read_excel(path = 'Data_Extract_From_World_Development_Indicators.xlsx', extension = 'xlsx', keep_NA = FALSE, view = FALSE)
codes <- read_excel('Dictrionary_codes.xlsx')
data$value <- as.integer(data$value)
data$year <- as.integer(data$year)

ui <- fluidPage(
  
  # App title ----
  titlePanel("World Bank Project"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      
      pickerInput(
        inputId = "country",
        label = "Select countries:", 
        choices = unique(data$`Country Name`),
        options = list(
          `actions-box` = TRUE), 
        multiple = TRUE
      ),
      
      br(),
      
      pickerInput('indicator', 'Select Indicator', choices = unique(data$`Series Name`) ),
      
      br(),
      # Input: Slider for the number of observations to generate ----
      sliderTextInput(
        inputId = "years",
        label = "Choose a range:", 
        choices = unique(data$year),
        selected = c(2005, 2015)
      )
    
     
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  tabPanel("Plot", 
                           plotOutput("plot")),
                  
                  tabPanel("Summary",
                           verbatimTextOutput("summary")),
                  tabPanel("Table", 
                           DTOutput("table")),
                  tabPanel("Bar Race", 
                           sliderInput('yearAnime', '', min = min(data$year), max = max(data$year), value = 2001, step = 1, sep = "",width = 400,
                                       animate = animationOptions(
                             interval = 1000,
                             loop = TRUE,
                             playButton = NULL,
                             pauseButton = NULL
                           )),
                           
                           plotOutput("bar_race"))
      )
    )
    )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  dataInput <- reactive({
    plot_df <- data %>% 
      filter(year >= input$years[1] & year <= input$years[2] & `Country Name` %in% input$country & `Series Name` == input$indicator) %>% 
      select(`Country Name`, year, value) %>% 
      arrange(`Country Name`, desc(year))
  })
  
  barRaceCalc <- reactive({
    data %>% 
      left_join(codes, by = c("Country Code" = "three_code" )) %>% 
      filter(year == input$yearAnime & `Series Name` == input$indicator) %>% 
      group_by(year) %>% 
      arrange(year,desc(value)) %>% 
      mutate(rank = row_number()*120) %>% 
      select(year, `Country Name`, two_code, value, rank) 
  })
    
    output$bar_race <- renderPlot({
      
      
      
      
        ggplot(barRaceCalc()) + #, aes(x = Rank, group = Country, country = as.factor(Code)
        geom_col(aes(x = rank, y = value), width = 100, fill = "magenta", color = "black") + # Columns
        coord_flip(clip = "off", expand = FALSE) + # Flip
        labs(title='{closest_state}', x = "", y = "Value") + # Labels
        theme_minimal() + # Theme
        #geom_text(aes(x = rank, y = -1000, label = `Country Name`), hjust = 1) + # Names
        geom_text(aes(x = rank, y = value + 200, label = as.character(value)), hjust = 0, color = "black") + # Values  
        geom_flag(aes(x = rank, y = -300,  country = two_code), size = 10) + # Flags
        scale_y_continuous(labels = scales::comma) + # Format y-axis values
        scale_x_reverse() + # Highest values on top
        #transition_states(year, transition_length = 4, state_length = 1) + # Animate
        ggtitle((as.character(input$yearAnime)), subtitle = input$indicator)+
        theme(
          plot.margin = margin(0,2,0,3,"cm"),
          axis.text.y  = element_blank()
          )  
      })
      
 
    
    output$table <- renderDT(
      DT::datatable(
       dataInput() )
      
      
     
      
    )
    
    
    
  }
  
 

# Run the application 
shinyApp(ui = ui, server = server)
