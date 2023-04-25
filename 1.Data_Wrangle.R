##--libraries-------------------------------------------------------------------
library(tidyverse)
library(tidycensus)
library(readxl)
options(scipen=999)

# Looking at the codebook 
varlist19 <- load_variables(year = 2019, dataset = "acs5")

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

##--3a. IPUMS data----------------------------------------------------------------------


##--4a. ACS data------------------------------------------------------------------------

# Looking at the codebook 
varlist21 <- load_variables(year = 2021, dataset = "acs1")

# Construction Workers variable list from ACS
varlist <- c(total_employees = "B24060_031", # Total Construction Workers
  
             total_male = B24010_117, # Male 
             total_female = B24010_268, # Female 
             
             fulltime_male = B24020_117, # Full time Male
             fulltime_female = B24020_268, # Full time Female
             
             
             B24010A_032, # White Male 
             B24010A_068, # White Female 
             B24010B_032, # Black Male 
             B24010B_068, # Black Female 
             B24010C_032, # Native Male
             B24010C_068, # Native Female
             B24010D_032, # Asian Male
             B24010D_068, # Asian Female
             B24010E_032, # Hawaii Male
             B24010E_068, # Hawaii Female
             B24010F_032, # Other race Male
             B24010F_068, # Other race Female
             B24010G_032, # 2 or more races Male
             B24010G_068, # 2 or more races Female
             B24010H_032, # White alone, Not Hispanic Male
             B24010H_068, # White alone, Not Hispanic Female
             B24010I_032, # Hispanic Male
             B24010I_068 # Hispanic Female
             )

# setting years 
years <- lst(2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 
             2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2021)








