##--libraries-------------------------------------------------------------------
library(plotly)
library(RColorBrewer)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

##--1. CES Data: Employment Trends in Construction---------------------------------------------
ces_data <- rbind(ces_employees, ces_pctchange)

# line graph of employment over time 
ces_fig <- ces_data %>% 
  plot_ly(
    type = "scatter", 
    mode = "lines", 
    x =~ date, 
    y =~ values,
    color =~ type,
    colors = c("#006EB6","#F79B2E"),
    text =~ type,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y}")
  ) %>% 
  layout(
  title = list(text="<br>      Employment Trends in the Construction Industry<br>      in Greater Philadelphia",
               x=0,y=1),
  font = list(family = "Georgia", color = "darkslategrey"),
  hoverlabel = list(font = list(family = "Georgia")),
  yaxis = list(title = "Employment Trends"),
  xaxis = list(title = "",
               rangeslider = list(type = "date")),
  legend = list(
    orientation = "h",
    xanchor = "center",
    x = 0.5,
    yanchor = "top",
    y = -0.3
  ),
  margin = list(l = 70, r = 70, b = 50, t = 80)
  )

ces_fig

##--2a. ACS Data: Filters for Ethnic Proportions----------------------------------------------

# filtering for just race variables 
acs_race <- acs_dta %>% 
  filter(variable != "total_employees" & 
      variable != "total_female" & 
      variable != "total_male" & 
      variable != "fulltime_female" & 
      variable != "fulltime_male"
      ) 

# Separate ethnicity column into race and gender columns
acs_race <- separate(acs_race, variable, into = c("race", "gender"), sep = "_")

# Summing by ethnicity 
acs_race <- acs_race %>% 
  group_by(Year, NAME, race) %>% 
  mutate(total_estimate = sum(estimate))

# Removing "white" race category since it double counts as hispanic AND white
acs_race <- acs_race %>% 
  filter(race != "white") %>% 
  mutate(race = ifelse(race == "whiteNONHISPANIC", "white", race),
         Year = as.factor(Year),
         race = as.factor(race))

# Generating proportional employment by race
acs_race <- acs_race %>% 
  group_by(Year, NAME) %>% 
  mutate(emp_prop = total_estimate/sum(total_estimate, na.rm = TRUE) * 100)
  
  
##--2b. ACS Data: Ethnic Proportions Over Time-----------------------------------------

# Generate color palette
my_colors <- brewer.pal(6, "Dark2")

# bar graph of ethnic proportions of employment over time 
acs_race_fig <- acs_race %>% 
  distinct(emp_prop, .keep_all = TRUE) %>% 
  filter(NAME == "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metro Area") %>% 
  plot_ly(
    type = 'bar',
    x =~ as.factor(Year), 
    y =~ total_estimate,
    color =~ race,
    colors = my_colors,
    text =~ race,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y}")
  ) %>% 
  layout(barmode = 'stack',
         title = list(text="<br>      Employment Trends in the Construction Industry<br>      in Greater Philadelphia by Ethnicity",
                      x=0,y=1),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         yaxis = list(title = "Number of employees"),
         xaxis = list(title = ""),
         legend = list(
           orientation = "h",
           xanchor = "center",
           x = 0.5,
           yanchor = "top",
           y = -0.1
         ),
         margin = list(l = 70, r = 70, b = 50, t = 80)
         )

acs_race_fig

##--2c. ACS Data: Filters for Gender Proportions----------------------------------------------
# filtering for gender variables 
acs_gender <- acs_dta %>% 
  filter(variable == "total_employees" |
           variable == "total_female" | 
           variable == "total_male" | 
           variable == "fulltime_female" | 
           variable == "fulltime_male"
  ) 

acs_gender_fig <- acs_gender %>%
  filter(NAME == "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metro Area") %>% 
  filter(variable != "total_employees") %>% 
  plot_ly(
    type = "scatter", 
    mode = "lines", 
    x =~ as.factor(Year), 
    y =~ estimate,
    color =~ variable,
    #colors = c("#006EB6","#F79B2E"),
    text =~ variable,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y}")
  )

acs_gender_fig

