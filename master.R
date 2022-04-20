library(devtools)
install_github('https://github.com/ellisp/ggflags')
install_github('https://github.com/kerajxl/WBreader')



packages <- c("ggplot2", "devtools", "gifski", "tidyr", "gganimate", "countrycode", 'shiny', 'shinyWidgets',
              'tidyverse', 'rlang', 'gganimate', 'DT', 'tweenr', 'magick', 'magrittr', 'countrycode',
              'grid', 'ggflags', 'WBreader', 'bslib', 'thematic')


installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

invisible(lapply(packages, library, character.only = TRUE))

data <- WBreader::wb_read_excel(path = 'Data_Extract_From_World_Development_Indicators.xlsx', extension = 'xlsx', keep_NA = FALSE)

data$year <- as.integer(data$year)

data$value <- as.numeric(data$value)

summary(data)
#codes
data$'Country Code' <- countrycode(data$'Country Name', "country.name", "iso2c")
data$'Country Code' <- tolower(data$'Country Code')


#Adjusts all plots in shiny to the current theme
thematic_shiny(
  bg = 'auto',
  fg = 'auto',
  accent =  'auto',
  font =  'auto')


ui <- fluidPage(
  
  #Theme
  theme = bs_theme(bootswatch = 'solar'),
  
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
      
      pickerInput('indicator', 'Select Indicator x', choices = unique(data$`Series Name`) ),
      
      pickerInput('indicator_y', 'Select Indicator y', choices = unique(data$`Series Name`) ),
      
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
                           
                           plotOutput("bar_race")),
                  
                  tabPanel("Scatter Plot",
                           
                           #display dynamic UI
                           uiOutput("slider"),
                           
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
  
  dataInput <- reactive({
    plot_df <- data %>% 
      filter(year >= input$years[1] & year <= input$years[2] & `Country Name` %in% input$country & `Series Name` == input$indicator) %>% 
      select(`Country Name`, year, value) %>% 
      arrange(`Country Name`, desc(year))
  })
  
  barRaceCalc <- reactive({
    data %>% 
      #left_join(codes, by = c("Country Code" = "three_code" )) %>% 
      filter(year == input$yearAnime & `Series Name` == input$indicator) %>% 
      group_by(year) %>% 
      arrange(year,desc(value)) %>% 
      mutate(rank = row_number()*120) %>% 
      select(year, `Country Name`, `Country Code`, value, rank) 
  })
  
  output$bar_race <- renderPlot({
    
    
    
    
    ggplot(barRaceCalc()) +
      geom_col(aes(x = rank, y = value), width = 100) + # Columns
      coord_flip(clip = "off", expand = FALSE) + # Flip
      labs(title='{closest_state}', x = "", y = "Value") + # Labels
      theme_minimal() + # Theme
      geom_text(aes(x = rank, y = value + 200, label = as.character(value)), hjust = 0) + # Values  
      geom_flag(aes(x = rank, y = -300,  country = unique(data$'Country Code')), size = 10) + # Flags
      scale_y_continuous(labels = scales::comma) + # Format y-axis values
      scale_x_reverse() + # Highest values on top
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
  
  
  #wprowadzam swój plot
  
  scatterCalc <- reactive({
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
  
  output$slider <- renderUI({
    sliderInput("inSlider", "Run the animation", min=input$years[1], max=input$years[2], value=input$years[1], step = 1, width = 400,
                animate = animationOptions(
                  interval = 2000,
                  loop = TRUE,
                  playButton = NULL,
                  pauseButton = NULL
                ))
  })
  
  
  gif_data <- reactive({
    data %>%
      filter(input$years[1] & year <= input$years[2], `Country Name`%in% input$country,
             `Series Name` %in% c(input$indicator, input$indicator_y)) %>% 
      spread(key = `Series Name`, value = value) %>% 
      group_by(year) %>% 
      arrange(year,desc(year))
    
  })
  
  
  observeEvent(input$render_gif, {
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