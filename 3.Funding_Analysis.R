##--libraries----
library(tidyverse)

##--importing data----
grants_data1 <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_1.csv")
grants_data2 <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_2.csv")
grants_data3 <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_3.csv")
grants_data4 <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_4.csv")

grants_data_2023 <- rbind(grants_data1, grants_data2, grants_data3, grants_data4)

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

  
##--saving data set---- 
#write.csv(df_grphl, "Data/clean_fed_funds.csv")







