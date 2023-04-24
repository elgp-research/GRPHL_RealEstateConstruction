##--libraries-------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(readxl)

##--data import-----------------------------------------------------------------
ces_employees <- read_excel("Data/ces_employees.xlsx")
ces_pctchange <- read_excel("Data/ces_pctchange.xlsx")

abs_data <- read_csv("Data/abs_data.csv")

##--CES data clean-----------------------------------------------------------------
# define a cleaning function
cleanData <- function(data) {
  data <- data[-c(1,2,3,4,5,6,7,8,9,10,11),] # removing empty rows in the data
  colnames(data) <- as.character(unlist(data[1, ])) # set column names based on first row values
  data <- data[-1,]
  return(data)
}




