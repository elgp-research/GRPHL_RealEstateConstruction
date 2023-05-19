grants_data <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_1.csv")

# filtering for counties in Greater Philadelphia
df_grphl <- grants_data %>% 
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

# filtering for infrastructure related government departments 
df_grphl <- df_grphl %>% 
  filter(awarding_agency_name == "Department of Transportation" |
           awarding_agency_name == "Department of Energy" |
           awarding_agency_name == "Department of Housing and Urban Development" |
           awarding_agency_name == "Environmental Protection Agency"
  )

# variables of interest: 
# federal_action_obligation ($)
# total_obligated_amount ($)
# cfda_title: purpose of the grant
# assistance_type_description: description of grant type 
# business_types_description: type of organization getting the funding
