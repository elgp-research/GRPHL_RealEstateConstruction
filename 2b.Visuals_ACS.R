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
acs_race_county <- acs_county_dta %>% 
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
acs_race_county <- separate(acs_race_county, variable, into = c("race", "gender"), sep = "_")

# Summing by ethnicity 
acs_race_county <- acs_race_county %>% 
  group_by(Year, NAME, race) %>% 
  mutate(total_estimate = sum(estimate, na.rm = TRUE)) %>% 
  distinct(total_estimate, .keep_all = TRUE)

# Removing whiteNONHISPANICS 
acs_race_county <- acs_race_county %>% 
  filter(race != "whiteNONHISPANIC") %>% 
  mutate(Year = as.factor(Year),
         race = as.factor(race)) 

# Generating proportional employment by race
acs_race_county <- acs_race_county %>% 
  group_by(Year, NAME) %>% 
  mutate(emp_prop = (total_estimate/sum(total_estimate, na.rm = TRUE)) * 100) %>% 
  distinct(emp_prop, .keep_all = TRUE) %>% 
  filter(emp_prop != 0)

##--2g.2. ACS Data: Ethnic Proportions AND Population at County level------------

acs_pop <- acs_county_dta %>% 
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


##--2g.3. ACS Data: Combining County Ethnic Data and Overall Ethnic Data-------

# calculating proportions of employees by race in the employee data

temp_dta1 <- acs_race_county %>% 
  select(-c(gender, estimate, total_estimate))

temp_dta2 <- acs_pop %>% 
  select(-c(estimate, total_pop))

ethnic_merge <- temp_dta1 %>% 
  inner_join(temp_dta2, by = c("Year", "GEOID", "NAME", "race"))

# calculating over/under representation in construction industry by ethnicity 
ethnic_merge <- ethnic_merge %>%
  group_by(Year, NAME) %>% 
  mutate(diff_prop = emp_prop - ethnic_prop) 

# calculating average difference in representation by county
ethnic_merge <- ethnic_merge %>% 
  group_by(GEOID, race) %>% 
  mutate(avg_diff_prop = mean(diff_prop, na.rm = TRUE)) %>% 
  distinct(avg_diff_prop, .keep_all = TRUE)

##--2g.4. ACS Data: Lollipop Plot of Representation at County Level----

ethnic_merge %>%
  filter(NAME == "Philadelphia County, Pennsylvania") %>% 
  ggplot(aes(x=reorder(race, avg_diff_prop),  y=avg_diff_prop)) +
  geom_segment(aes(x=reorder(race, avg_diff_prop), xend=reorder(race, avg_diff_prop), 
                   y=0, yend=avg_diff_prop), color="#1097FF") +
  geom_point( color="#FF4900", size=4) +
  geom_hline(yintercept = 0, color = "grey") +
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("") +
  ylab("Value of Y")



