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
    colors = c("#1097FF","#FF4900"),
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
  annotations = list(
    x = 1.05, # X position of the caption (right side of the plot)
    y = 1.1, # Y position of the caption (top of the plot)
    text = "Source: CES Data Estimates", # The text of the caption
    showarrow = FALSE, # Don't show an arrow pointing to the caption
    xref = "paper", # Set the X position reference to the plot area
    yref = "paper", # Set the Y position reference to the plot area
    font = list(size = 9, color = "grey80"), # Set the font size of the caption
    align = "right", # Align the caption to the right
    xanchor = "right", # Anchor the caption to the right side of the plot
    yanchor = "top" # Anchor the caption to the top of the plot
  ),
  margin = list(l = 70, r = 70, b = 50, t = 80)
  )

ces_fig

##--2a. ACS Data: Filters for Construction Ethnic Proportions----------------------------------------------

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
my_colors <- c("#FF9200", "darkslategrey", "#FF4900", 
               "#7915FF", "#FFBF00", "#1097FF")  

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
         yaxis = list(title = "Number of Employees"),
         xaxis = list(title = ""),
         legend = list(
           orientation = "h",
           xanchor = "center",
           x = 0.5,
           yanchor = "top",
           y = -0.1
         ),
         annotations = list(
           x = 1.05, # X position of the caption (right side of the plot)
           y = 1.1, # Y position of the caption (top of the plot)
           text = "Source: ACS Data Estimates", # The text of the caption
           showarrow = FALSE, # Don't show an arrow pointing to the caption
           xref = "paper", # Set the X position reference to the plot area
           yref = "paper", # Set the Y position reference to the plot area
           font = list(size = 9, color = "grey80"), # Set the font size of the caption
           align = "right", # Align the caption to the right
           xanchor = "right", # Anchor the caption to the right side of the plot
           yanchor = "top" # Anchor the caption to the top of the plot
         ),
         margin = list(l = 70, r = 70, b = 50, t = 80)
         )

acs_race_fig

##--2c. ACS Data: Filter AND Graph for Gender Proportions----------------------------------------------

# filtering for gender variables 
acs_gender <- acs_dta %>% 
  filter(variable == "total_employees" |
           variable == "total_female" | 
           variable == "total_male" | 
           variable == "fulltime_female" | 
           variable == "fulltime_male"
  ) 

# Generate color palette
my_colors <- c("#FF9200", "darkslategrey", "#FF4900", 
               "#7915FF", "#FFBF00", "#1097FF")  

# line graph of employment trends by gender
acs_gender_fig <- acs_gender %>%
  filter(NAME == "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metro Area") %>% 
  filter(variable != "total_employees") %>% 
  plot_ly(
    type = "scatter", 
    mode = "lines+markers", 
    x =~ as.factor(Year), 
    y =~ estimate,
    color =~ variable,
    colors = my_colors,
    text =~ variable,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y}")
  ) %>% 
  layout(
         title = list(text="<br>      Employment Trends in the Construction Industry<br>      in Greater Philadelphia by Gender",
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
         annotations = list(
           x = 1.05, # X position of the caption (right side of the plot)
           y = 1.1, # Y position of the caption (top of the plot)
           text = "Source: ACS Data Estimates", # The text of the caption
           showarrow = FALSE, # Don't show an arrow pointing to the caption
           xref = "paper", # Set the X position reference to the plot area
           yref = "paper", # Set the Y position reference to the plot area
           font = list(size = 9, color = "grey80"), # Set the font size of the caption
           align = "right", # Align the caption to the right
           xanchor = "right", # Anchor the caption to the right side of the plot
           yanchor = "top" # Anchor the caption to the top of the plot
         ),
         
         margin = list(l = 70, r = 70, b = 50, t = 80)
  )
  

acs_gender_fig


##--2d. ACS Data: Filters for Ethnic Makeup of GRPHL--------------------------------

acs_ethnic <- acs_dta %>% 
  filter(variable == "total_white" |
         variable == "total_black" |
         variable == "total_native" |
         variable == "total_asian" |
         variable == "total_hawaiin" |
         variable == "total_other" |
         variable == "total_multiethnic" |
         variable == "total_hispanic")


##--3a. IPUMS Data: filtering for Self-Employed Respondents-----------------------

ipums_construction <- ipums_construction %>% 
  filter(CLASSWKR == "Self-employed" & 
           IND != 3080) # removing this industry code because of almost all 0 values across years

# combining asian race classification
ipums_construction <- ipums_construction %>% 
  mutate(RACE = as.factor(as.character(RACE)),
    RACE = ifelse(RACE == "Chinese", "Asian", 
                       ifelse(RACE == "Japanese", "Asian",
                              ifelse(RACE == "Other Asian or Pacific Islander", "Asian",
                                     ifelse(RACE == "Other race, nec", "Other",
                                            ifelse(RACE == "Two major races", "Multi-Ethnic",
                                                   ifelse(RACE == "Three or more major races", "Multi-Ethnic",
                                                          ifelse(RACE == "White", "White",
                                                                 ifelse(RACE == "Black/African American", "Black/African American",
                                                                        ifelse(RACE == "American Indian or Alaska Native", "American Indian or Alaska Native", RACE))))))))))
# summing employers by race 
ipums_construction <- ipums_construction %>% 
  group_by(YEAR, RACE) %>% 
  mutate(Freq_adj = sum(Freq))

##--3b. IPUMS Data: Employer Mapping by Ethnicity-----------------------

# Generate color palette
my_colors <- c("#FF9200", "darkslategrey", "#FF4900", 
               "#7915FF", "#FFBF00", "#1097FF")  

# bar graph of ethnic proportions of employers over time 
ipums_race_fig <- ipums_construction %>% 
  distinct(Freq_adj, .keep_all = TRUE) %>% 
  plot_ly(
    type = 'bar',
    x =~ YEAR, 
    y =~ Freq_adj,
    color =~ RACE,
    colors = my_colors,
    text =~ RACE,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y}")
  ) %>% 
  layout(barmode = 'stack',
         title = list(text="<br>      Employer Trends in the Construction Industry<br>      in Greater Philadelphia by Ethnicity",
                      x=0,y=1),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         yaxis = list(title = "Number of Employers"),
         xaxis = list(title = ""),
         legend = list(
           orientation = "h",
           xanchor = "center",
           x = 0.5,
           yanchor = "top",
           y = -0.1
         ),
         annotations = list(
           x = 1.05, # X position of the caption (right side of the plot)
           y = 1.1, # Y position of the caption (top of the plot)
           text = "Source: IPUMS USA Data Estimates", # The text of the caption
           showarrow = FALSE, # Don't show an arrow pointing to the caption
           xref = "paper", # Set the X position reference to the plot area
           yref = "paper", # Set the Y position reference to the plot area
           font = list(size = 9, color = "grey80"), # Set the font size of the caption
           align = "right", # Align the caption to the right
           xanchor = "right", # Anchor the caption to the right side of the plot
           yanchor = "top" # Anchor the caption to the top of the plot
         ),
         margin = list(l = 70, r = 70, b = 50, t = 70)
  )

ipums_race_fig






