grants_data <- read.csv("Data/FY2023_All_Assistance_Full_20230508/FY2023_All_Assistance_Full_20230510_1.csv")

# filtering for Pennsylvania 
df <- grants_data %>% 
  filter(recipient_state_name == "PENNSYLVANIA")

# filtering for infrastructure related government departments 
df <- df %>% 
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
