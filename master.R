library(readxl)
library(devtools)
library(tidyverse)
install_github('https://github.com/kerajxl/WBreader') 
library(WBreader)
data <- WBreader::wb_read_excel('Data_Extract_From_World_Development_Indicators.xlsx', extension = 'xlsx', keep_NA = FALSE)


