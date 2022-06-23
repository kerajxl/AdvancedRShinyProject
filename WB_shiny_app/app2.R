#Load the first 2 necessary packages to avoid problems with masked functions.
library(tidyverse)
library(devtools)

#Install our WBReader package and ggflags for nice animations.
install_github('https://github.com/ellisp/ggflags')
install_github('https://github.com/kerajxl/WBreader')


#Installing packages if necessary.
packages <- c("ggplot2", "devtools", "gifski", "tidyr", "gganimate", "countrycode",'shinyjs', 'shiny', 'shinyWidgets',
              'tidyverse', 'rlang', 'gganimate', 'DT', 'tweenr', 'magick', 'magrittr', 'countrycode',
              'grid', 'ggflags', 'WBreader', 'bslib', 'thematic', 'shinydashboard', 'readxl', 'wbstats', 'plotly')


installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))


#Set working directory 

#Jarek
#setwd("C:/Users/leski/OneDrive/Pulpit/DS/APR/Advanced_R_project")

#Daniel
setwd('C:/Users/Admin/Desktop/DATA SCIENCE STUDIA/2 SEM DSBA/ADVANCED R/Projekcik z KerajemXL/Advanced_R_project')





#Adjusts all plots in shiny to the current theme
thematic_shiny(
  bg = 'auto',
  fg = 'auto',
  accent =  'auto',
  font =  NA)

#Part reposinsible for user interface
ui <- fluidPage(
  useShinyjs(),
  useSweetAlert(),
  #Theme
  theme = bs_theme(),
  
  # App title ----
  titlePanel("The World Bank Project"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      fileInput('file1', 'Choose xlsx file',
                accept = c(".xlsx", ".csv")),
      
      inputPanel(
        div(style="display:inline-block;vertical-align:top;",
                    fluidRow(
                      column(10, materialSwitch(
                inputId = "API",
                label = "Switch to API", 
                value = FALSE,
                status = "success"
              )),
              column(2, actionButton(
                inputId = "success",
                label = "",
                icon = icon("question"))
              ))
        )),
 
      
  
      
      
     
      uiOutput("country"),
      
      br(),
      uiOutput("indicator"),
      
      #This indicator will be only visible after clicking the Scatter Plot panel.
      conditionalPanel(
        "output.scatter_plot",
        uiOutput("indicator_y")
      ),
      
      
      
      br(),
      # Input: Slider for the number of observations to generate ----
      uiOutput("years"),
      
      # The download button
      downloadButton('download',"Download the data in long format")
      
      
      
      
    ),
    
    # Main panel for displaying outputs -
    
    mainPanel(
      
      # Output: Tabset with plots, instructions, and table
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Instructions",
                           strong("Welcome to the World Bank Project!"),
                           p("Step 1 - Data: By default a well selected dataset is loaded into the app. If you want to 
                             load your own data, please click the 'Browse...' button on the left-hand side and 
                             upload your data. Our app works only with data downloaded from the official World Bank
                             site. You may also connect to the WB Api and generate a data set with 15 randomly selected
                             indicators. Some plots may not render well due to a lot of missing values in some indicators.
                             We recommend to use our data but you may try using Api as well."),
                           p("Step 2 - Theme: Choose a theme that suits you best by clicking 'Theme customizer' on the right-hand
                             side."),
                           p("Step 3 - Countries: Select the countries that you are interested in."),
                           p("Step 4 - Indicators: Select the indicators that you are interested in. If you don't select any coutnries,
                             plots will not load regardless of indicators."),
                           p("Step 5 - Years: Select the range of years you are interested in."),
                           p("Step 6 - Plots: Have fun with various plots :). Majority of plots may be animated by clicking the play button.
                           Clicking the Scatter Plot tab will activate an 
                             additional option to choose indicator y, as it is the only 2-dimensional plot."),
                           p("Step 7 - Export to csv in long format: You may export the data set to csv in long format by clicking
                             the button 'Download the data in long format' in the left-down corner. Choose a directory to which
                             the csv will be saved and provide the name for the file. You don't have to write '.csv' in the name of your file.
                             Saved data set will have countries and years that you have selected."),
                           verbatimTextOutput("instructions")),
                  tabPanel("Table", 
                           plotlyOutput("plotlyTable")),
                  tabPanel("Static Plot", 
                           
                           prettyRadioButtons(
                             inputId = "selPlot",
                             label = "Choose a graph :", 
                             choices = c("bar", "line", "pie"),
                             icon = icon("fa-thin fa-chart-line"),
                             animation = "jelly",
                             inline = TRUE, shape = 'curve'
                           ),
                           
                           plotlyOutput('staticPlot')
                           ),
                  
                  tabPanel("Bar Race", uiOutput("slider1"),
   
                           plotOutput("bar_race")),
                  
                  tabPanel("Scatter Plot",
                           
                           #display dynamic UI
                           uiOutput("slider2"),
                           
                           plotOutput("scatter_plot"),
                           
                           numericInput('flag_size', label = 'Adjust the size of flags: ', 10,
                                        1, 30, 1),
                           imageOutput("plot1")
                  )
      )
                  
                  
      )
    )
)



#Server responsible for generating outpus
server <- function(input, output, session) {
  bs_themer()
  observeEvent(input$success, {
    sendSweetAlert(
      session = getDefaultReactiveDomain(),
      title = "Let's connect to the API!",
      text = "This option allows you to connect to the WorldBank API. 
            Due to the number of available indicators (over 10 thousand) and the fact that the application was prepared for the purpose
            of presentation of skills in R, we limited the number of indicators to 15 randomly selected. Connecting may take up to a minute.",
      type = "info"
    )})   
  
  #reactive data set 
  dataset <- reactive({
      if(!is.null(input$file1)){
        file <- input$file1
        ext <- tools::file_ext(file$name)
        file.rename(file$datapath,
                    paste(file$datapath, ext, sep="."))
        data <-wb_read_excel(path = (paste(file$datapath, ext, sep=".")),extension = ext, keep_NA = FALSE )
       
      } else if(input$API == TRUE){

        y = wbindicators()
        y %>% filter(source == 'World Development Indicators') -> y
        y <- sample(y$indicatorID,15)
       
        #yyyy <- c("NY.GDP.MKTP.CD" , 	"NY.GNS.ICTR.CD")
        

        indicators = y
        countries = c("AT", "BE", "BG", "CY" ,"HR", "CZ", "DK", "EE", "FI", "FR", "DE", "GR" ,"HU", "IE", "IT",
                      "LV", "LU", "LT", "MT", "NL", "PL", "PT",
                       "RO", "SI", "SK", "ES", "SE", "BR", "CA", "CH",
                      "CN","JP", "QA", "US", "AU")
        
        data1 <- wb(country = countries, indicator = y)  #= 'NY.GDP.PCAP.CD')
        data1$iso2c <- tolower(data1$iso2c)
        data1$date <- as.integer(data1$date)
        data1 %>% 
          mutate(prettyValue = ifelse(grepl('%',indicator, fixed = TRUE)==TRUE, paste0(round(value,2),'%'),
                                      format(round(value,0),nsmall = 0, big.mark="'"))) %>% 
          select(country,iso2c, date, indicator, value, prettyValue) -> data1 
        colnames(data1) <- c("Country Name","Country Code" ,"year","Series Name","value","prettyValue" )
            
          
        
        
        
        data <- data1
       
         
        
      } else {
        setwd('C:/Users/Admin/Desktop/DATA SCIENCE STUDIA/2 SEM DSBA/ADVANCED R/Projekcik z KerajemXL/Advanced_R_project')
        
        data <- WBreader::wb_read_excel(path = 'Data_Extract_From_World_Development_Indicators.xlsx', extension = 'xlsx', keep_NA = FALSE)
        
      }
    })
  
  ###Export data to csv in long format. User may download data for countries and years she/he is interested in.
  
  thedata <- reactive({
    data <- dataset()
    data %>%
      filter(year >= min(c(input$years[1], input$years[2])) & year <= max(c(input$years[1], input$years[2])), `Country Name`%in% input$country)
    })
  
  #Function for downloading the data
  output$download <- downloadHandler(
    filename = function(){"thename.csv"}, 
    content = function(fname){
      write.csv(thedata(), fname)
    }
  ) 
  
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
      filter(year >= input$years[1] & year <= input$years[2] & `Series Name` == input$indicator & `Country Name` %in% input$country ) %>% 
      select(year, `Country Name`, `Country Code`, value, prettyValue) 
  })

  output$staticPlot <- renderPlotly({
    
    data_static <- staticPlotCalc()
    if(input$selPlot == 'bar'){
      
      plot1 <- plot_ly(
      x = data_static$`Country Name`,
      y = data_static$value,
      frame = data_static$year,
      type = 'bar',
      mode = 'markers')
      plot1 <- plot1 %>%  layout(title = paste0(input$indicator," over time")) %>% animation_slider(
        currentvalue = list(prefix = "Year ", font = list(color="red")))  
      plot1 <- plot1 %>% animation_opts(
          frame = 1000000, easing = "elastic", redraw = FALSE
        )
      plot1 <- plot1 %>% 
        layout(xaxis = list(categoryorder = "total descending"))
      
      
      } else if(input$selPlot == 'line'){
      plot2 <- plot_ly(
        x = data_static$year,
        y = data_static$value,
        split = data_static$`Country Name`,
        type = 'scatter',
        mode = 'lines+markers',
        )
      plot2 <- plot2 %>%  layout(title = paste0(input$indicator," over time"))
    } else {
      
      plot3 <- plot_ly(
        labels = data_static$`Country Name`,
        values = data_static$value,
        type = 'pie',
      )
      plot3 <- plot3 %>% layout(title = input$indicator, xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                                         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
      }
    
    
  })
   
  
  
  output$table <- renderDT(
    DT::datatable(
      dataInput() ))
    
 output$plotlyTable <- renderPlotly({
   data <- dataset()
   table1 <- plot_ly(
     type = 'table',
     header = list(values = c("#", names(data)),
     align = c('left', rep('center', ncol(mtcars))),
     line = list(width = 1, color = 'black'),
     fill = list(color = 'rgb(235, 100, 230)'),
     font = list(family = "Arial", size = 14, color = "white")
   ),
   cells = list(
     values = rbind(
       rownames(data), 
       t(as.matrix(unname(data)))
     ),
   align = c('left', rep('center', ncol(data))),
   line = list(color = "black", width = 1),
   fill = list(color = c('rgb(235, 193, 238)', 'rgba(228, 222, 249, 0.65)')),
   font = list(family = "Arial", size = 12, color = c("black"))   
     
     
   )
   )
 })   
    
    
  
  
  
  #scatter plot
  #Filter only necessary data according to the selected range of years and indicators
  scatterCalc <- reactive({
    data <- dataset()
    data %>%
      filter(year >= min(c(input$years[1], input$years[2])) & year <= max(c(input$years[1], input$years[2])), `Country Name`%in% input$country,
             `Series Name` %in% c(input$indicator, input$indicator_y)) %>% 
      #Spread the Series Name column in order to create two columns for plotting the scatter plot.
      spread(key = `Series Name`, value = prettyValue) %>% 
      filter(year == input$inSlider) %>%
      #Remove unnecesary value column and merge the rows with summarise function.
      select(-value) %>% group_by(year, `Country Name`, `Country Code`) %>%
      summarise_all(na.omit)

  })
  
  
  output$scatter_plot <- renderPlot({
    p_data <- scatterCalc()
    
    ggplot(p_data) +
      aes(x = get(input$indicator), y = get(input$indicator_y)) +
      geom_flag(aes( country=unique(`Country Code`)),size = input$flag_size, show.legend=FALSE) + 
      labs(x = input$indicator,
           y = input$indicator_y) +
      ggtitle((as.character(input$inSlider))) #Current year for the animation
    
    
    
  })
  #Slider for running the animation
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
  

  

  
  
  
}



# Run the application 
shinyApp(ui = ui, server = server)

