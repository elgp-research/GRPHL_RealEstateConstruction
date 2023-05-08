##--libraries-------------------------------------------------------------------
library(plotly)
library(RColorBrewer)
library(leaflet)
library(sf)
library(waffle)
library(sfheaders)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

##--2a. ACS Data: Filters for Construction Employment in GRPHL------------------------

# filtering for just race variables 
acs_race <- acs_metro_dta %>% 
  filter(variable != "total_employees" & 
           variable != "total_female" & 
           variable != "total_male" & 
           variable != "fulltime_female" & 
           variable != "fulltime_male" & 
           variable != "total_white" &
           variable != "total_black" &
           variable != "total_native" &
           variable != "total_asian" &
           variable != "total_hawaii" &
           variable != "total_otherrace" &
           variable != "total_multirace" &
           variable != "total_hispanic"
  )

# Separate ethnicity column into race and gender columns
acs_race <- separate(acs_race, variable, into = c("race", "gender"), sep = "_")

# Summing by ethnicity 
acs_race <- acs_race %>% 
  group_by(Year, NAME, race) %>% 
  mutate(total_estimate = sum(estimate, na.rm = TRUE)) %>% 
  distinct(total_estimate, .keep_all = TRUE)

# Removing "white" race category since it double counts as hispanic AND white
acs_race <- acs_race %>% 
  filter(race != "whiteNONHISPANIC") %>% 
  mutate(Year = as.factor(Year),
         race = as.factor(race)) 


##--2c. ACS Data: Filter AND Graph for Gender Proportions of GRPHL----------------------------------------------

# filtering for gender variables 
acs_gender <- acs_metro_dta %>% 
  filter(variable == "total_employees" |
           variable == "total_female" | 
           variable == "total_male" | 
           variable == "fulltime_female" | 
           variable == "fulltime_male"
  ) 

##--2d. ACS Data: Filters for Ethnic Population of GRPHL--------------------------------

acs_pop <- acs_metro_dta %>% 
  filter(variable == "total_white" |
           variable == "total_black" |
           variable == "total_native" |
           variable == "total_asian" |
           variable == "total_hawaii" |
           variable == "total_otherrace" |
           variable == "total_multirace" |
           variable == "total_hispanic")

# creating race variable for merging
acs_pop$variable <- sub("total_", "", acs_pop$variable)

# renaming race variable 
acs_pop <- acs_pop %>% 
  rename(race = variable)

# calculating proportions for each race 
acs_pop <- acs_pop %>% 
  group_by(Year, NAME) %>% 
  mutate(total_pop = sum(estimate),
         ethnic_prop = (estimate/sum(estimate, na.rm = TRUE)) * 100)


##--2e. ACS Data: Ethnic Employment and Ethnic Population of GRPHL----

# calculating proportions of employees by race in the employee data

temp_dta1 <- acs_race %>% 
  group_by(Year, NAME) %>% 
  mutate(emp_prop = (total_estimate/sum(total_estimate, na.rm = TRUE)) * 100) %>% 
  distinct(emp_prop, .keep_all = TRUE) %>% 
  select(-c(gender, estimate, total_estimate))

temp_dta2 <- acs_pop %>% 
  select(-c(estimate, total_pop))

ethnic_merge <- temp_dta1 %>% 
  inner_join(temp_dta2, by = c("Year", "GEOID", "NAME", "race"))

# calculating over/under representation in construction industry by ethnicity 
ethnic_merge <- ethnic_merge %>% 
  group_by(NAME, race) %>% 
  mutate(avg_employment = mean(emp_prop, na.rm = TRUE),
         avg_ethnicity = mean(ethnic_prop, na.rm = TRUE)
  ) %>% 
  distinct(avg_employment, .keep_all = TRUE) %>% 
  select(-c(emp_prop, ethnic_prop)) %>% 
  gather(variable, proportion, avg_employment:avg_ethnicity)

##--2g.1. ACS Data: Ethnic Proportions at County level----------------------------------------------

# filtering for just race variables 
acs_race_employees <- acs_county_dta %>% 
  filter(variable != "total_employees" & 
           variable != "total_female" & 
           variable != "total_male" & 
           variable != "fulltime_female" & 
           variable != "fulltime_male" & 
           variable != "total_white" &
           variable != "total_black" &
           variable != "total_native" &
           variable != "total_asian" &
           variable != "total_hawaii" &
           variable != "total_otherrace" &
           variable != "total_multirace" &
           variable != "total_hispanic"
  )

# Separate ethnicity column into race and gender columns
acs_race_employees <- separate(acs_race_employees, variable, into = c("race", "gender"), sep = "_")

# Summing by ethnicity 
acs_race_employees <- acs_race_employees %>% 
  group_by(Year, GEOID, race) %>% 
  mutate(total_estimate = sum(estimate, na.rm = TRUE)) %>% 
  distinct(total_estimate, .keep_all = TRUE) %>% 
  select(-c(gender, estimate))

# Removing whiteNONHISPANICS 
acs_race_employees <- acs_race_employees %>% 
  filter(race != "whiteNONHISPANIC") %>% 
  mutate(Year = as.factor(Year),
         race = as.factor(race)) 

# Generating proportional employment by race
acs_race_employees <- acs_race_employees %>% 
  group_by(Year, GEOID) %>% 
  mutate(emp_prop = (total_estimate/sum(total_estimate, na.rm = TRUE)) * 100) %>% 
  distinct(emp_prop, .keep_all = TRUE) %>% 
  select(-total_estimate)

##--2g.2. ACS Data: Ethnic Proportions AND Population at County level------------

acs_race_pop <- acs_county_dta %>% 
  filter(variable == "total_white" |
           variable == "total_black" |
           variable == "total_native" |
           variable == "total_asian" |
           variable == "total_hawaii" |
           variable == "total_otherrace" |
           variable == "total_multirace" |
           variable == "total_hispanic")

# creating race variable for merging
acs_race_pop$variable <- sub("total_", "", acs_race_pop$variable)

# renaming race variable 
acs_race_pop <- acs_race_pop %>% 
  rename(race = variable)

# calculating proportions for each race 
acs_race_pop <- acs_race_pop %>% 
  group_by(Year, GEOID) %>% 
  mutate(ethnic_prop = (estimate/sum(estimate, na.rm = TRUE)) * 100) %>% 
  select(-estimate)


##--2g.3. ACS Data: Combining County Ethnic Data and Overall Ethnic Data at County Level-------

# calculating proportions of employees by race in the employee data

ethnic_merge <- acs_race_employees %>% 
  inner_join(acs_race_pop, by = c("Year", "GEOID", "NAME", "race"))

# calculating over/under representation in construction industry by ethnicity 
ethnic_merge <- ethnic_merge %>%
  group_by(Year, GEOID) %>% 
  mutate(diff_prop = emp_prop - ethnic_prop) 

# calculating average difference in representation by county
ethnic_merge <- ethnic_merge %>% 
  group_by(GEOID, race) %>% 
  mutate(avg_diff_prop = mean(diff_prop, na.rm = TRUE)) %>% 
  distinct(avg_diff_prop, .keep_all = TRUE)

##--2g.4. ACS Data: Philadelphia vs. Rest of Greater Philadelphia---------------

# filtering counties in Greater Philadelphia excluding Philadelphia 
philly_prime <- ethnic_merge %>% 
  filter(NAME == "Montgomery County, Pennsylvania" | 
           NAME == "Bucks County, Pennsylvania" |
           NAME == "Chester County, Pennsylvania" |
           NAME == "Delaware County, Pennsylvania" |
           NAME == "Burlington County, New Jersey" |
           NAME == "Camden County, New Jersey" |
           NAME == "Gloucester County, New Jersey" |
           NAME == "New Castle County, Delaware" |
           NAME == "Cecil County, Maryland" | 
           NAME == "Salem County, New Jersey") %>% 
  mutate(region = "Rest of Greater Philadelphia") %>% 
  group_by(race) %>% 
  mutate(region_emp_prop = mean(avg_diff_prop)) # creating average differences across counties excluding Philadelphia 

# filtering Philadelphia 
philly <- ethnic_merge %>% 
  filter(NAME == "Philadelphia County, Pennsylvania") %>% 
  mutate(region = "Philadelphia",
         region_emp_prop = avg_diff_prop)
  
region_employment <- rbind(philly, philly_prime) 
region_employment <- region_employment %>% 
  mutate(race = paste0(toupper(substr(race, 1, 1)), substr(race, 2, nchar(race))))

##--2g.5. ACS Data: Barplot Plot of Representation at County Level----

region_employment %>%
  filter(race != "Otherrace") %>% 
  ggplot(aes(x=reorder(race, region_emp_prop),  y=region_emp_prop, fill = region)) +
  geom_col(position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("#FF4900", "#1097FF")) +
  labs(x = "", y = "Difference (% points)",
       title = "Proportional Representation in Construction Sector \nin Greater Philadelphia (2010 - 2021)",
       subtitle = "This graph shows the representation of employees in the Construction sector by race and ethnicity. \nPositive numbers mean the ethnicity is over-represented in the construction sector compared to \ntheir population proportion in the same region. Negative numbers mean vice versa. The bars are \nalso separated by Philadelphia and rest of Greater Philadelphia excluding Philadelphia.",
       caption = "Source: American Community Survey") +
  theme_minimal() + 
  theme(axis.title = element_blank(),
        #panel.grid.major.x = element_blank(),
        #panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.justification = c(0, 1),
        legend.background = element_blank(),
        legend.position = "bottom",
        legend.direction="horizontal",
        legend.text = element_text(),
        text = element_text(family = "Georgia"),
        strip.text = element_text(color = "black"),
        plot.title = element_text(size = 15, margin = margin(b = 10, t = 5), color = "darkslategrey", hjust = 0),
        plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey50", hjust = 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))









##--2
