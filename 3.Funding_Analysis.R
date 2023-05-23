##--libraries----
library(tidyverse)

read_and_clean_data <- function(folder_path) {
  # Get a list of all CSV files in the folder
  csv_files <- list.files(folder_path, pattern = "\\.csv$")
  
  # Initialize a list to store the cleaned dataframes
  cleaned_data <- list()
  
  # Loop over each CSV file
  for (i in seq_along(csv_files)) {
    # Read in the data
    df <- read.csv(file.path(folder_path, csv_files[i]))
    
    # Get the name of the current CSV file (without the .csv extension)
    csv_name <- tools::file_path_sans_ext(csv_files[i])
    
    # Extract the year from the CSV file name
    year <- as.numeric(sub(".*FY(\\d+).*", "\\1", csv_name))
    
    # Add a year column
    df$year <- year
    
    # Your cleaning code here
    
    ##--filtering for counties in Greater Philadelphia----
    df <- df %>% 
      filter((recipient_county_name == "MONTGOMERY" & recipient_state_name == "PENNSYLVANIA") |
               (recipient_county_name == "BUCKS" & recipient_state_name == "PENNSYLVANIA") |
               (recipient_county_name == "CHESTER" & recipient_state_name == "PENNSYLVANIA") |
               (recipient_county_name == "DELAWARE"  & recipient_state_name == "PENNSYLVANIA") |
               (recipient_county_name == "BURLINGTON" & recipient_state_name == "NEW JERSEY") |
               (recipient_county_name == "CAMDEN" & recipient_state_name == "NEW JERSEY") |
               (recipient_county_name == "GLOUCESTER" & recipient_state_name == "NEW JERSEY") |
               (recipient_county_name == "NEW CASTLE" & recipient_state_name == "DELAWARE") |
               (recipient_county_name == "CECIL" & recipient_state_name == "MARYLAND") | 
               (recipient_county_name == "SALEM" & recipient_state_name == "NEW JERSEY") |
               (recipient_county_name == "PHILADELPHIA" & recipient_state_name == "PENNSYLVANIA")
      ) 
    
    ##--filtering for infrastructure related government departments----
    df <- df %>% 
      filter(awarding_agency_name == "Department of Transportation" |
               awarding_agency_name == "Department of Energy" |
               awarding_agency_name == "Department of Housing and Urban Development" |
               awarding_agency_name == "Department of Defense" |
               awarding_agency_name == "Environmental Protection Agency" 
      )
    
    ##--filtering relevant variables----
    df <- df %>% 
      select(recipient_state_name, recipient_county_name, recipient_name, recipient_address_line_1, 
             recipient_city_code, awarding_agency_name, federal_action_obligation, 
             total_obligated_amount, cfda_title, assistance_type_description, 
             business_types_description, year)
    
    ##--summing ammounts by data filters
    df <- df %>% 
      mutate(region = ifelse(recipient_county_name == "PHILADELPHIA", "Philadelphia", "Greater Philadelphia")) 
    
    # Add the cleaned dataframe to the list
    cleaned_data[[i]] <- df
  }
  
  return(cleaned_data)
}

# Assuming you have a folder named "data" containing CSV files with names like "data_FY2019.csv"
cleaned_data <- read_and_clean_data("Data/Federal_Assistance")

# Assuming you have a list of cleaned dataframes named cleaned_data
names(cleaned_data) <- paste0("df", seq_along(cleaned_data), "_clean")
list2env(cleaned_data, envir = .GlobalEnv)


grants_data1 <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_1.csv")
grants_data2 <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_2.csv")
grants_data3 <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_3.csv")
grants_data4 <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_4.csv")

grants_data_2023 <- rbind(grants_data1, grants_data2, grants_data3, grants_data4)

##--contracts data----
# contracts_data1 <- read.csv("Data/FY2023_All_Contracts_Full_20230508/FY2023_All_Contracts_Full_20230510_1.csv")
# contracts_data2 <- read.csv("Data/FY2023_All_Contracts_Full_20230508/FY2023_All_Contracts_Full_20230510_2.csv")
# contracts_data3 <- read.csv("Data/FY2023_All_Contracts_Full_20230508/FY2023_All_Contracts_Full_20230510_3.csv")
#
# contracts_data_2023 <- rbind(contracts_data1, contracts_data2, contracts_data3)

##--filtering for counties in Greater Philadelphia----
df_grphl <- grants_data_2023 %>%
  filter((recipient_county_name == "MONTGOMERY" & recipient_state_name == "PENNSYLVANIA") |
           (recipient_county_name == "BUCKS" & recipient_state_name == "PENNSYLVANIA") |
           (recipient_county_name == "CHESTER" & recipient_state_name == "PENNSYLVANIA") |
           (recipient_county_name == "DELAWARE"  & recipient_state_name == "PENNSYLVANIA") |
           (recipient_county_name == "BURLINGTON" & recipient_state_name == "NEW JERSEY") |
           (recipient_county_name == "CAMDEN" & recipient_state_name == "NEW JERSEY") |
           (recipient_county_name == "GLOUCESTER" & recipient_state_name == "NEW JERSEY") |
           (recipient_county_name == "NEW CASTLE" & recipient_state_name == "DELAWARE") |
           (recipient_county_name == "CECIL" & recipient_state_name == "MARYLAND") |
           (recipient_county_name == "SALEM" & recipient_state_name == "NEW JERSEY") |
           (recipient_county_name == "PHILADELPHIA" & recipient_state_name == "PENNSYLVANIA")
  )

##--filtering for infrastructure related government departments----
df_grphl <- df_grphl %>%
  filter(awarding_agency_name == "Department of Transportation" |
           awarding_agency_name == "Department of Energy" |
           awarding_agency_name == "Department of Housing and Urban Development" |
           awarding_agency_name == "Environmental Protection Agency"
  )

##--filtering relevant variables----
df_grphl <- df_grphl %>%
  select(recipient_state_name, recipient_county_name, awarding_agency_name, federal_action_obligation,
         total_obligated_amount, cfda_title, assistance_type_description,
         business_types_description)

##--summing ammounts by data filters
df_grphl <- df_grphl %>%
  mutate(region = ifelse(recipient_county_name == "PHILADELPHIA", "Philadelphia", "Greater Philadelphia"),
         year = 2023)


# ##--saving data set---- 
# #write.csv(df_grphl, "Data/clean_fed_funds.csv")
# 
df7_clean$year <- 2021 
