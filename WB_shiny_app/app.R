#library(tidyverse)
library(devtools)
install_github('https://github.com/ellisp/ggflags')
install_github('https://github.com/kerajxl/WBreader')



packages <- c("ggplot2", "devtools", "gifski", "tidyr", "gganimate", "countrycode", 'shiny', 'shinyWidgets',
              'tidyverse', 'rlang', 'gganimate', 'DT', 'tweenr', 'magick', 'magrittr', 'countrycode',
              'grid', 'ggflags', 'WBreader', 'bslib', 'thematic', 'shinydashboard', 'readxl')


installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))
setwd("C:/Users/leski/OneDrive/Pulpit/DS/APR/Advanced_R_project")


  
#data <- WBreader::wb_read_excel(path = 'Data_Extract_From_World_Development_Indicators.xlsx', extension = 'xlsx', keep_NA = FALSE)


#Adjusts all plots in shiny to the current theme
thematic_shiny(
  bg = 'auto',
  fg = 'auto',
  accent =  'auto',
  font =  'auto')


ui <- fluidPage(
  
  #Theme
  theme = bs_theme(),
  
  # App title ----
  titlePanel("World Bank Project"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      fileInput('file1', 'Choose xlsx file',
                accept = c(".xlsx", ".csv")
      ),
      
      
      uiOutput("country"),
      
      br(),
      uiOutput("indicator"),
      uiOutput("indicator_y"),
      
      
      
      br(),
      # Input: Slider for the number of observations to generate ----
      uiOutput("years")
      
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    
    mainPanel(
      
      # Output: Tabset w/ plot, summary, and table ----
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Summary",
                           verbatimTextOutput("summary")),
                  tabPanel("Table", 
                           DTOutput("table")),
                  tabPanel("Static Plot", 
                           
                           prettyRadioButtons(
                             inputId = "selPlot",
                             label = "Choose a graph :", 
                             choices = c("bar", "line", "pie"),
                             icon = icon("fa-thin fa-chart-line"),
                             animation = "jelly",
                             inline = TRUE, shape = 'curve'
                           ),
                           
                           plotOutput('staticPlot')
                           ),
                  
                  tabPanel("Bar Race", uiOutput("slider1"),
   
                           plotOutput("bar_race")),
                  
                  tabPanel("Scatter Plot",
                           
                           #display dynamic UI
                           uiOutput("slider2"),
                           
                           plotOutput("scatter_plot"),
                           
                           numericInput('flag_size', label = 'Adjust the size of flags: ', 10,
                                        1, 30, 1),
                           actionButton('render_gif', 'Export to gif'),
                           imageOutput("plot1")
                  )
                  
                  
                  
      )
    )
  )
) 


# Define server logic required to draw a histogram
server <- function(input, output) {
  bs_themer()
  
  #reactive dataset 
  dataset <- reactive({
      if(is.null(input$file1)){
        setwd("C:/Users/leski/OneDrive/Pulpit/DS/APR/Advanced_R_project")
        data <- WBreader::wb_read_excel(path = 'Data_Extract_From_World_Development_Indicators.xlsx', extension = 'xlsx', keep_NA = FALSE)
      } else {
        file <- input$file1
        ext <- tools::file_ext(file$name)
        file.rename(file$datapath,
                    paste(file$datapath, ext, sep="."))
        data <-wb_read_excel(path = (paste(file$datapath, ext, sep=".")),extension = ext, keep_NA = FALSE )
      }
    })
  
  #UIs 
  output$country <- renderUI({
    data <- dataset()
    pickerInput(
      inputId = "country",
      label = "Select countries:", 
      choices = unique(data$`Country Name`),
      options = list(
        `actions-box` = TRUE), 
      multiple = TRUE
    )
                
  })
  
  output$indicator <- renderUI({
    data <- dataset()
    pickerInput('indicator', 'Select Indicator x', choices = unique(data$`Series Name`),options = list(
      `actions-box` = TRUE) )
  })
  
  output$indicator_y <- renderUI({
    data <- dataset()
    pickerInput('indicator_y', 'Select Indicator y', choices = unique(data$`Series Name`), options = list(
      `actions-box` = TRUE) )
  })
  
  output$years <- renderUI({
    data <- dataset()
    sliderTextInput(
      inputId = "years",
      label = "Choose a range:", 
      choices = unique(data$year),
      selected = c(min(data$year), max(data$year))
    )
  })

  
  
  
  
  
  
  
               
  dataInput <- reactive({
    data <- dataset()
    plot_df <- data %>% 
      filter(year >= input$years[1] & year <= input$years[2] & `Country Name` %in% input$country & `Series Name` == input$indicator) %>% 
      select(`Country Name`, year, value) %>% 
      arrange(`Country Name`, desc(year))
  })
  
  barRaceCalc <- reactive({
    data <- dataset()
    data %>% 
      #left_join(codes, by = c("Country Code" = "three_code" )) %>% 
      filter(year == input$inSlider1 & `Series Name` == input$indicator & `Country Name` %in% input$country ) %>% 
      group_by(year) %>% 
      arrange(year,desc(value)) %>% 
      mutate(rank = row_number()*120) %>% 
      select(year, `Country Name`, `Country Code`, value, prettyValue, rank)
  })
  
  
 
  
  output$slider1 <- renderUI({
    data <- dataset()
    sliderInput("inSlider1", "Run the animation", min=input$years[1], max=input$years[2], value=input$years[1], step = 1, width = 400,sep = "", 
                animate = animationOptions(
                  interval = 1500, 
                  loop = TRUE,
                  playButton = NULL,
                  pauseButton = NULL
                ))
  })
  
 
  
  
  output$bar_race <- renderPlot({
    
    
    
    data_bar <- barRaceCalc()
    ggplot(data_bar) +
      geom_col(aes(x = rank, y = value), width = 70) + # Columns
      coord_flip(clip = "off", expand = FALSE) + # Flip
      labs( x = "", y = "Value") + # Labels
      theme_minimal() + # Theme
      geom_text(aes(x = rank, y = value + (max(value)- min(value))*0.005, label = as.character(prettyValue)), hjust = 0) + # Values  
      geom_flag(aes(x = rank, y = -(max(value)- min(value))*0.03,  country = unique(data_bar$'Country Code')), size = 10) + # Flags
      scale_y_continuous(labels = scales::comma) + # Format y-axis values
      scale_x_reverse() + # Highest values on top
      ggtitle((as.character(input$inSlider1)), subtitle = input$indicator)+
      theme(
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 15),
        text = element_text(colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0,2,0,3,"cm"),
        axis.ticks.y  = element_blank()
        
        
      )   
  }, height = 600)
  
 
  staticPlotCalc <- reactive({
    data <- dataset()
    data %>% 
      #left_join(codes, by = c("Country Code" = "three_code" )) %>% 
      filter(year >= input$years[1] & year <= input$years[2] & `Series Name` == input$indicator & `Country Name` %in% input$country ) %>% 
      select(year, `Country Name`, `Country Code`, value)
  })
 
  output$staticPlot <- renderPlot({
    
    data_static <- staticPlotCalc()
    if(input$selPlot == 'bar'){
      ggplot(data=data_static, aes(x=`Country Name`, y=value)) +
      geom_bar(stat="identity", fill="steelblue")+
      theme_minimal()
    } else if(input$selPlot == 'line'){
      
    } else {
      
    }
    
    
  })
   
  
  
  output$table <- renderDT(
    DT::datatable(
      dataInput() )
    
    
    
    
  )
  
  
  #wprowadzam swój plot
  
  scatterCalc <- reactive({
    data <- dataset()
    data %>%
      filter(input$years[1] & year <= input$years[2], `Country Name`%in% input$country,
             `Series Name` %in% c(input$indicator, input$indicator_y)) %>% 
      spread(key = `Series Name`, value = value) %>% 
      filter(year == input$inSlider) %>%
      mutate(x = input$indicator, y = input$indicator_y)
    #group_by(year) %>% 
    #arrange(year,desc(year)) #%>% 
    #select(year,`Country Name`,  `Country Code`, input$indicator, input$indicator_y) 
  })
  
  
  output$scatter_plot <- renderPlot({
    p_data <- scatterCalc()
    
    ggplot(p_data) +
      aes(x = get(input$indicator), y = get(input$indicator_y)) +
      geom_flag(aes( country=unique(`Country Code`)),size = input$flag_size, show.legend=FALSE) + 
      labs(x = input$indicator,
           y = input$indicator_y) +
      #theme_minimal() +
      ggtitle((as.character(input$inSlider)))
    
    
    
  })
  
  output$slider2 <- renderUI({
    data <- dataset()
    sliderInput("inSlider", "Run the animation", min=input$years[1], max=input$years[2], value=input$years[1], step = 1, width = 400,sep = "", 
                animate = animationOptions(
                  interval = 1000, 
                  loop = TRUE,
                  playButton = NULL,
                  pauseButton = NULL
                ))
  })
  
  
  gif_data <- reactive({
    data <- dataset()
    data %>%
      filter(input$years[1] & year <= input$years[2], `Country Name`%in% input$country,
             `Series Name` %in% c(input$indicator, input$indicator_y)) %>% 
      spread(key = `Series Name`, value = value) %>% 
      group_by(year) %>% 
      arrange(year,desc(year))
    
  })
  
  
  observeEvent(input$render_gif, {
    data <- dataset()
    output$plot1 <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      # now make the animation
      p = ggplot(gif_data(), aes(x = get(input$indicator), y = get(input$indicator_y))) +
        geom_flag(aes( country=unique(`Country Code`)),size = input$flag_size, show.legend=FALSE) + 
        labs(x = input$indicator,
             y = input$indicator_y) +
        transition_time(year) # New
      
      anim_save("outfile.gif", animate(p)) # New
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif',
           width = 400,
           height = 300,
           alt = "This is alternate text"
      )}, deleteFile = TRUE)
  })
  
  
  
  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

