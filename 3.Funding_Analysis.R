options(scipen = 999)
##--libraries----
library(tidyverse)

##--Creating Function for reading and cleaning data----
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
    # df <- df %>% 
    #   filter(awarding_agency_name == "Department of Transportation" |
    #            awarding_agency_name == "Department of Energy" |
    #            awarding_agency_name == "Department of Housing and Urban Development" |
    #            awarding_agency_name == "Department of Defense" |
    #            awarding_agency_name == "Environmental Protection Agency" 
    #   )
    
    ##--filtering relevant variables----
    # df <- df %>% 
    #   select(recipient_state_name, recipient_county_name, recipient_name, recipient_address_line_1, 
    #          period_of_performance_start_date, period_of_performance_current_end_date,
    #          awarding_office_name, funding_office_name, recipient_city_code, awarding_agency_name, federal_action_obligation, 
    #          total_obligated_amount, cfda_title, assistance_type_description, 
    #          business_types_description, year)
    
    ##--summing ammounts by data filters
    df <- df %>% 
      mutate(region = ifelse(recipient_county_name == "PHILADELPHIA", "Philadelphia", "Greater Philadelphia")) 
    
    ##--calculating duration of grants 
    df$period_of_performance_start_date <- as.Date(df$period_of_performance_start_date)
    df$period_of_performance_current_end_date <- as.Date(df$period_of_performance_current_end_date)
    
    df$grant_duration <- as.numeric(difftime(df$period_of_performance_current_end_date, df$period_of_performance_start_date, units = "weeks")) 
    
    ##--calculating time left in funding
    current_date <- Sys.Date()
    df$months_left <- as.numeric(difftime(df$period_of_performance_current_end_date, current_date, units = "weeks")) 
    
    
    # Add the cleaned dataframe to the list
    cleaned_data[[i]] <- df
  }
  
  return(cleaned_data)
}

##--Running the function on all the datasets----
cleaned_data <- read_and_clean_data("Data")

##--Putting dataframe in R's global environment----
names(cleaned_data) <- paste0("df", seq_along(cleaned_data), "_clean")
list2env(cleaned_data, envir = .GlobalEnv)

##--Binding datasets together---- 

# Initialize an empty dataframe to store the combined data
df_grphl <- data.frame()

# Loop over the names of dataframes
for (i in 1:3) {
  # Get the name of the current dataframe
  df_name <- paste0("df", i, "_clean")
  
  # Retrieve the current dataframe from R environment
  df <- get(df_name)
  
  # Bind the current dataframe to the combined data
  df_grphl <- rbind(df_grphl, df)
}

# creating dataset for businesses by ethnicity 
# df_grphl_eth <- df_grphl %>% 
#   select(contract_transaction_unique_key, contract_award_unique_key,
#          award_id_piid, modification_number, parent_award_agency_id, 
#          federal_action_obligation, total_dollars_obligated, 
#          period_of_performance_start_date, period_of_performance_current_end_date,
#          region, awarding_agency_name, recipient_name, recipient_address_line_1, recipient_city_name,
#          alaskan_native_corporation_owned_firm, american_indian_owned_business, 
#          tribally_owned_firm, minority_owned_business, asian_pacific_american_owned_business, 
#          black_american_owned_business, hispanic_american_owned_business)

# reshaping data to long to create ethnicity variable 
# df_grphl_eth <- df_grphl_eth %>% 
#   gather(ethnicity, indicator, alaskan_native_corporation_owned_firm:hispanic_american_owned_business)

df_temp <- df_grphl %>% 
  distinct(product_or_service_code_description)

df_grphl <- read.csv("Data/clean_fed_funds_contracts.csv")

construction_list <- c(
"ARCHITECT AND ENGINEERING- CONSTRUCTION: CONFERENCE SPACE AND FACILITIES",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: DAMS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: HEATING AND COOLING PLANTS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: HIGHWAYS, ROADS, STREETS, BRIDGES, AND RAILWAYS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: HOSPITALS AND INFIRMARIES",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: LABORATORIES AND CLINICS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: MAINTENANCE BUILDINGS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: MISCELLANEOUS BUILDINGS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: MUSEUMS AND EXHIBITION BUILDINGS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: OFFICE BUILDINGS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: OTHER ADMINISTRATIVE FACILITIES/SERVICE BUILDINGS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: OTHER HOSPITAL BUILDINGS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: OTHER INDUSTRIAL BUILDINGS",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: PARKING FACILITIES",
"ARCHITECT AND ENGINEERING- CONSTRUCTION: RESTORATION OF REAL PROPERTY (PUBLIC OR PRIVATE)",
"ARCHITECT AND ENGINEERING- GENERAL: ENGINEERING DRAFTING, NOT CAD/CAM",
"ARCHITECT AND ENGINEERING- GENERAL: INSPECTION (NON-CONSTRUCTION)",
"ARCHITECT AND ENGINEERING- GENERAL: LANDSCAPING, INTERIOR LAYOUT, AND DESIGNING",
"ARCHITECT AND ENGINEERING- GENERAL: OTHER",
"BUILDING COMPONENTS, PREFABRICATED",
"CABINETS, LOCKERS, BINS, AND SHELVING",
"CABLE, CORD, AND WIRE ASSEMBLIES: COMMUNICATION EQUIPMENT",
"CIRCUIT BREAKERS",
"CLEAN WORK STATIONS, CONTROLLED ENVIRONMENT, AND RELATED EQUIPMENT",
"CLEANING AND POLISHING COMPOUNDS AND PREPARATIONS",
"CONNECTORS, ELECTRICAL",
"CONSTRUCTION OF AIRPORT RUNWAYS AND TAXIWAYS",
"CONSTRUCTION OF DAMS",
"CONSTRUCTION OF DINING FACILITIES",
"CONSTRUCTION OF EXHIBIT DESIGN (NON-BUILDING)",
"CONSTRUCTION OF FUEL STORAGE BUILDINGS",
"CONSTRUCTION OF HIGHWAYS, ROADS, STREETS, BRIDGES, AND RAILWAYS",
"CONSTRUCTION OF HOSPITALS AND INFIRMARIES",
"CONSTRUCTION OF LABORATORIES AND CLINICS",
"CONSTRUCTION OF MAINTENANCE BUILDINGS",
"CONSTRUCTION OF MISCELLANEOUS BUILDINGS",
"CONSTRUCTION OF OFFICE BUILDINGS",
"CONSTRUCTION OF OTHER ADMINISTRATIVE FACILITIES AND SERVICE BUILDINGS",
"CONSTRUCTION OF OTHER AIRFIELD STRUCTURES",
"CONSTRUCTION OF OTHER HOSPITAL BUILDINGS",
"CONSTRUCTION OF OTHER INDUSTRIAL BUILDINGS",
"CONSTRUCTION OF OTHER NON-BUILDING FACILITIES",
"CONSTRUCTION OF OTHER UTILITIES",
"CONSTRUCTION OF PARKING FACILITIES",
"CONSTRUCTION OF RECREATION FACILITIES (NON-BUILDING)",
"CONSTRUCTION OF RESTORATION OF REAL PROPERTY (PUBLIC OR PRIVATE)",
"CONSTRUCTION OF SCHOOLS",
"CONSTRUCTION OF SEWAGE AND WASTE FACILITIES",
"HOUSEKEEPING- CARPET LAYING/CLEANING",
"HOUSEKEEPING- CUSTODIAL JANITORIAL",
"HOUSEKEEPING- FACILITIES OPERATIONS SUPPORT",
"HOUSEKEEPING- INTERIOR PLANTSCAPING",
"HOUSEKEEPING- LANDSCAPING/GROUNDSKEEPING",
"HOUSEKEEPING- WAREHOUSING/STORAGE",
"INDOOR AND OUTDOOR ELECTRIC LIGHTING FIXTURES",
"INDUSTRIAL ASSEMBLY MACHINES",
"INDUSTRIAL BOILERS",
"INSPECTION- ALARM, SIGNAL, AND SECURITY DETECTION SYSTEMS",
"INSPECTION- ELECTRICAL AND ELECTRONIC EQUIPMENT COMPONENTS",
"INSPECTION GAGES AND PRECISION LAYOUT TOOLS",
"INSPECTION- INSTRUMENTS AND LABORATORY EQUIPMENT",
"INSPECTION- PLUMBING, HEATING, AND WASTE DISPOSAL EQUIPMENT",
"INSPECTION- PREFABRICATED STRUCTURES AND SCAFFOLDING",
"INSPECTION- VALVES",
"INSTALLATION OF EQUIPMENT- ELECTRICAL AND ELECTRONIC EQUIPMENT COMPONENTS",
"INSTALLATION OF EQUIPMENT- FIBER OPTICS MATERIALS, COMPONENTS, ASSEMBLIES, AND ACCESSORIES",
"INSTALLATION OF EQUIPMENT- FIRE CONTROL EQUIPMENT",
"INSTALLATION OF EQUIPMENT- FURNITURE",
"INSTALLATION OF EQUIPMENT- HOUSEHOLD AND COMMERCIAL FURNISHINGS AND APPLIANCES",
"INSTALLATION OF EQUIPMENT- INSTRUMENTS AND LABORATORY EQUIPMENT",
"INSTALLATION OF EQUIPMENT- MEDICAL, DENTAL, AND VETERINARY EQUIPMENT AND SUPPLIES",
"INSTALLATION OF EQUIPMENT- OFFICE SUPPLIES AND DEVICES",
"INSTALLATION OF EQUIPMENT- PIPE, TUBING, HOSE, AND FITTINGS",
"INSTALLATION OF EQUIPMENT- PLUMBING, HEATING, AND WASTE DISPOSAL EQUIPMENT",
"INSTALLATION OF EQUIPMENT- REFRIGERATION, AIR CONDITIONING, AND AIR CIRCULATING EQUIPMENT",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- CLEANING EQUIPMENT AND SUPPLIES",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- CONSTRUCTION AND BUILDING MATERIALS",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- CONSTRUCTION/MINING/EXCAVATING/HIGHWAY MAINTENANCE EQUIPMENT",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- HOUSEHOLD AND COMMERCIAL FURNISHINGS AND APPLIANCES",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- INSTRUMENTS AND LABORATORY EQUIPMENT",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- LIGHTING FIXTURES AND LAMPS",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- PLUMBING, HEATING, AND WASTE DISPOSAL EQUIPMENT",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- PREFABRICATED STRUCTURES  AND SCAFFOLDING",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- PUMPS AND COMPRESSORS",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- REFRIGERATION, AIR CONDITIONING, AND AIR CIRCULATING EQUIPMENT",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- VALVES",
"MAINT/REPAIR/REBUILD OF EQUIPMENT- WATER PURIFICATION AND SEWAGE TREATMENT EQUIPMENT",
"MAINTENANCE OF DAMS",
"REPAIR OR ALTERATION OF DAMS",
"REPAIR OR ALTERATION OF FAMILY HOUSING FACILITIES",
"REPAIR OR ALTERATION OF FUEL SUPPLY FACILITIES",
"REPAIR OR ALTERATION OF GOVERNMENT-OWNED GOVERNMENT-OPERATED (GOGO) R&D FACILITIES",
"REPAIR OR ALTERATION OF HEATING AND COOLING PLANTS",
"REPAIR OR ALTERATION OF HIGHWAYS/ROADS/STREETS/BRIDGES/RAILWAYS",
"REPAIR OR ALTERATION OF HOSPITALS AND INFIRMARIES",
"REPAIR OR ALTERATION OF LABORATORIES AND CLINICS",
"REPAIR OR ALTERATION OF MISCELLANEOUS BUILDINGS",
"REPAIR OR ALTERATION OF MUSEUMS AND EXHIBITION BUILDINGS",
"REPAIR OR ALTERATION OF OFFICE BUILDINGS",
"REPAIR OR ALTERATION OF OPEN STORAGE FACILITIES",
"REPAIR OR ALTERATION OF OTHER ADMINISTRATIVE FACILITIES AND SERVICE BUILDINGS",
"REPAIR OR ALTERATION OF OTHER INDUSTRIAL BUILDINGS",
"REPAIR OR ALTERATION OF OTHER NON-BUILDING FACILITIES",
"REPAIR OR ALTERATION OF OTHER RESIDENTIAL BUILDINGS",
"REPAIR OR ALTERATION OF OTHER UTILITIES",
"REPAIR OR ALTERATION OF OTHER WAREHOUSE BUILDINGS",
"REPAIR OR ALTERATION OF PARKING FACILITIES",
"REPAIR OR ALTERATION OF PRODUCTION BUILDINGS",
"REPAIR OR ALTERATION OF RECREATION FACILITIES (NON-BUILDING)",
"REPAIR OR ALTERATION OF RESTORATION OF REAL PROPERTY (PUBLIC OR PRIVATE)",
"REPAIR OR ALTERATION OF SEWAGE AND WASTE FACILITIES",
"REPAIR OR ALTERATION OF TROOP HOUSING FACILITIES",
"REPAIR OR ALTERATION OF WATER SUPPLY FACILITIES")


df_grphl <- df_grphl %>%
  mutate(construction_indicator = ifelse(product_or_service_code_description %in% construction_list, "Construction", "Non-Construction"))

##--calculating funding by construction contract and funding agency  
df_temp1 <- df_grphl %>% 
  select(awarding_agency_name,  
         construction_indicator, total_dollars_obligated) %>%
  group_by(awarding_agency_name, construction_indicator) %>% 
  mutate(construction_funds_by_agency = sum(total_dollars_obligated)) %>% 
  distinct(construction_funds_by_agency, .keep_all = TRUE) %>% 
  select(-total_dollars_obligated) 

##-- calculating construction funding to minority businesses by agency
df_temp2 <- df_grphl %>% 
  select(awarding_agency_name,
         construction_indicator, total_dollars_obligated, minority_owned_business) %>% 
  group_by(awarding_agency_name, construction_indicator, minority_owned_business) %>% 
  mutate(construction_funds_by_agency_minority = sum(total_dollars_obligated)) %>% 
  distinct(construction_funds_by_agency_minority, .keep_all = TRUE) %>% 
  select(-total_dollars_obligated)
# creating proportions 
df_temp2 <- df_temp2 %>% 
  group_by(awarding_agency_name, construction_indicator) %>% 
  mutate(construction_funds_by_agency_minority_prop = construction_funds_by_agency_minority/sum(construction_funds_by_agency_minority)) %>% 
  filter(minority_owned_business == "t")
  
##-- calculating construction funding to asian businesses by agency
df_temp3 <- df_grphl %>% 
  select(awarding_agency_name,  
         construction_indicator, total_dollars_obligated, 
         asian_pacific_american_owned_business) %>%
  group_by(awarding_agency_name, construction_indicator, asian_pacific_american_owned_business) %>% 
  mutate(construction_funds_by_agency_asian = sum(total_dollars_obligated)) %>% 
  distinct(construction_funds_by_agency_asian, .keep_all = TRUE) %>% 
  select(-total_dollars_obligated)
# creating proportions 
df_temp3 <- df_temp3 %>% 
  group_by(awarding_agency_name, construction_indicator) %>% 
  mutate(construction_funds_by_agency_asian_prop = construction_funds_by_agency_asian/sum(construction_funds_by_agency_asian)) %>% 
  filter(asian_pacific_american_owned_business == "t")

##-- calculating construction funding to black businesses by agency
df_temp4 <- df_grphl %>% 
  select(awarding_agency_name,
         construction_indicator, total_dollars_obligated, 
         black_american_owned_business) %>%
  group_by(awarding_agency_name, construction_indicator, black_american_owned_business) %>% 
  mutate(construction_funds_by_agency_black = sum(total_dollars_obligated)) %>% 
  distinct(construction_funds_by_agency_black, .keep_all = TRUE) %>% 
  select(-total_dollars_obligated)
# creating proportions 
df_temp4 <- df_temp4 %>% 
  group_by(awarding_agency_name, construction_indicator) %>% 
  mutate(construction_funds_by_agency_black_prop = construction_funds_by_agency_black/sum(construction_funds_by_agency_black)) %>% 
  filter(black_american_owned_business == "t")

##-- calculating construction funding to hispanic businesses by agency
df_temp5 <- df_grphl %>% 
  select(awarding_agency_name, 
         construction_indicator, total_dollars_obligated,
         hispanic_american_owned_business) %>%
  group_by(awarding_agency_name, construction_indicator, hispanic_american_owned_business) %>% 
  mutate(construction_funds_by_agency_hispanic = sum(total_dollars_obligated)) %>% 
  distinct(construction_funds_by_agency_hispanic, .keep_all = TRUE) %>% 
  select(-total_dollars_obligated)
# creating proportions 
df_temp5 <- df_temp5 %>% 
  group_by(awarding_agency_name, construction_indicator) %>% 
  mutate(construction_funds_by_agency_hispanic_prop = construction_funds_by_agency_hispanic/sum(construction_funds_by_agency_hispanic)) %>% 
  filter(hispanic_american_owned_business == "t")

##-- calculating construction funding to women businesses by agency
df_temp6 <- df_grphl %>% 
  select(awarding_agency_name, 
         construction_indicator, total_dollars_obligated,
         women_owned_small_business) %>%
  group_by(awarding_agency_name, construction_indicator, women_owned_small_business) %>% 
  mutate(construction_funds_by_agency_women = sum(total_dollars_obligated)) %>% 
  distinct(construction_funds_by_agency_women, .keep_all = TRUE) %>% 
  select(-total_dollars_obligated)
# creating proportions 
df_temp6 <- df_temp6 %>% 
  group_by(awarding_agency_name, construction_indicator) %>% 
  mutate(construction_funds_by_agency_women_prop = construction_funds_by_agency_women/sum(construction_funds_by_agency_women)) %>% 
  filter(women_owned_small_business == "t")

##-- calculating construction funding to educational institutions by agency
df_temp7 <- df_grphl %>% 
  select(awarding_agency_name, 
         construction_indicator, total_dollars_obligated,
         educational_institution) %>%
  group_by(awarding_agency_name, construction_indicator, educational_institution) %>% 
  mutate(construction_funds_by_agency_education = sum(total_dollars_obligated)) %>% 
  distinct(construction_funds_by_agency_education, .keep_all = TRUE) %>% 
  select(-total_dollars_obligated)
# creating proportions 
df_temp7 <- df_temp7 %>% 
  group_by(awarding_agency_name, construction_indicator) %>% 
  mutate(construction_funds_by_agency_education_prop = construction_funds_by_agency_education/sum(construction_funds_by_agency_education)) %>% 
  filter(educational_institution == "t")

# merging all datasets together 
df_sankey <- df_temp1 %>% 
  left_join(df_temp2, by = c("awarding_agency_name", "construction_indicator"))

df_sankey <- df_temp1 %>% 
  left_join(df_temp2, by = c("awarding_agency_name", "construction_indicator")) %>%
    left_join(., df_temp3, by = c("awarding_agency_name", "construction_indicator")) %>%
    left_join(., df_temp4, by = c("awarding_agency_name", "construction_indicator")) %>%
    left_join(., df_temp5, by = c("awarding_agency_name", "construction_indicator")) %>%
    left_join(., df_temp6, by = c("awarding_agency_name", "construction_indicator")) %>%
    left_join(., df_temp7, by = c("awarding_agency_name", "construction_indicator")) 
    

#--saving data set----
write.csv(df_grphl, "Data/clean_fed_funds_contracts.csv")
write.csv(df_temp, "Data/temp_data.csv")
write.csv(df_grphl_eth, "Data/clean_fed_funds_contracts_ethnicity.csv")
