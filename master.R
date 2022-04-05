library(readxl)
library(devtools)
library(tidyverse)
install_github('https://github.com/kerajxl/WBreader') 
library(WBreader)
data <- WBreader::wb_read_excel(path = 'Data_Extract_From_World_Development_Indicators.xlsx', extension = 'xlsx', keep_NA = FALSE)

library(WDI)


dict <- WDIsearch('gdp')


gdp <- WDI(country="all", indicator = '6.0.GDP_growth')
