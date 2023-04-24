##--libraries-------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(readxl)

##--1a. CES data import-----------------------------------------------------------------

ces_employees <- read_excel("Data/ces_employees.xlsx")
ces_pctchange <- read_excel("Data/ces_pctchange.xlsx")

# store both datasets in a list
cesDataList <- list(ces_employees, ces_pctchange)

##--1b. CES data clean-----------------------------------------------------------------

# define a cleaning function
ces_clean <- function(data, index) {
  data <- data[-c(1,2,3,4,5,6,7,8,9,10,11),] # removing empty rows in the data
  colnames(data) <- as.character(unlist(data[1, ])) # set column names based on first row values
  data <- data[-1,]
  data <- data %>% 
    gather(month, values, Jan:Dec) %>% 
    mutate(values = as.numeric(values),
           date = as.Date(paste(Year, month, "01", sep = "-"), format = "%Y-%b-%d")
           ) %>% 
    select(Year, date, values)
  # create a new variable with a different value depending on the index of the data frame
  if (index == 1) {
    data$type <- "No. of employees (in thousands)"
  } else if (index == 2) {
    data$type <- "12-Month Percentage change"
  }
  # arranging by date 
  data <- data %>% 
    arrange(date)
  return(data)
}

# apply the cleaning function to each dataset in the list
cleanCESdata <- lapply(seq_along(cesDataList), function(i) ces_clean(cesDataList[[i]], i))
ces_employees <- cleanCESdata[[1]]
ces_pctchange <- cleanCESdata[[2]]

##--2a. ABS data import-----------------------------------------------------------------

abs_data <- read_csv("Data/abs_data.csv")
abs_data <- abs_data[-1,]
