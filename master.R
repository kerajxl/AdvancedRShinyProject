library(ggplot2)
install.packages('gganimate')
library(gganimate)

install.packages('devtools')
library(devtools)

install.packages('gifski')
library(gifski)
getwd()
setwd("C:/Users/Admin/Desktop/DATA SCIENCE STUDIA/2 SEM DSBA/ADVANCED R/Projekcik z KerajemXL/Advanced_R_project/projekcik")

install_github('https://github.com/kerajxl/WBreader') 
library(WBreader)
data <- WBreader::wb_read_excel(path = 'Data_Extract_From_World_Development_Indicators.xlsx', extension = 'xlsx', keep_NA = TRUE)

data$year <- as.numeric(data$year)
data$`GDP (current US$)`<- as.numeric(data$`GDP (current US$)`)
data$`Life expectancy at birth, total (years)` <- as.numeric(data$`Life expectancy at birth, total (years)`)

p <- ggplot(
  data, 
  aes(x = data$'GDP (current US$)', y=data$`Life expectancy at birth, total (years)` , size = data$'Population, total', colour = data$'Country Name')
) +
  geom_point(show.legend = TRUE) +
  scale_color_viridis_d() +
  labs(x = "GDP", y = "Life expectancy")
  #geom_text(aes(label=data$'Country Name'),hjust=0, vjust=0)
  #p
  
  summary(data)

animation <- p + transition_time(data$year) +labs(title = "Year: {frame_time}")

animate(animation, renderer=gifski_renderer())


