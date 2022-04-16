packages <- c("ggplot2", "devtools", "gifski", "tidyr", "gganimate", "countrycode")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

install_github('https://github.com/ellisp/ggflags')
library(ggflags)

install_github('https://github.com/kerajxl/WBreader') 
library(WBreader)
data <- WBreader::wb_read_excel(path = 'Data_Extract_From_World_Development_Indicators.xlsx', extension = 'xlsx', keep_NA = TRUE)

data$year <- as.numeric(data$year)
data$`GDP (current US$)`<- as.numeric(data$`GDP (current US$)`)
data$`Life expectancy at birth, total (years)` <- as.numeric(data$`Life expectancy at birth, total (years)`)


#codes
data$iso2c <- countrycode(data$'Country Name', "country.name", "iso2c")
data$iso2c <- tolower(data$iso2c)


data$`Death rate, crude (per 1,000 people)` <- as.numeric(data$`Death rate, crude (per 1,000 people)`)

p<- ggplot(data, aes(x=data$`GDP (current US$)`, y=data$`Life expectancy at birth, total (years)`)) +
  geom_flag(aes(size=data$`Population, total`, country=data$iso2c), show.legend=FALSE) + 
  #scale_fill_brewer(palette = "Set1") +    
  #expand_limits(y=0) +  
  labs(x = "GDP",
       y = "Death rate per 1000 people") +
  scale_x_log10(breaks=c(1000000000, 10000000000, 100000000000, 1000000000000, 10000000000000), label=scales::comma) +
  theme_minimal() +
  scale_y_log10(breaks = c(60, 70, 80, 90))

animation <- p + transition_time(year) +labs(title = "Year: {frame_time}")

animate(animation)
# renderer=gifski_renderer()
