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
  mutate(total_estimate = sum(estimate))

# Removing "white" race category since it double counts as hispanic AND white
acs_race <- acs_race %>% 
  filter(race != "white") %>% 
  mutate(race = ifelse(race == "whiteNONHISPANIC", "white", race),
         Year = as.factor(Year),
         race = as.factor(race))

# Generating proportional employment by race excluding hispanics
acs_race_nothispanic <- acs_race %>% 
  filter(race != "hispanic") %>% 
  group_by(Year, NAME) %>% 
  mutate(emp_prop = (total_estimate/sum(total_estimate, na.rm = TRUE)) * 100) %>% 
  distinct(emp_prop, .keep_all = TRUE)
  
# Generating proportional employment by race including hispanics
acs_race_hispanic <- acs_race %>% 
  group_by(Year, NAME) %>% 
  mutate(emp_prop = (total_estimate/sum(total_estimate, na.rm = TRUE)) * 100) %>% 
  distinct(emp_prop, .keep_all = TRUE) %>% 
  filter(race == "hispanic")
  

##--2b.1 ACS Data: Graph Ethnic Proportions Over Time (EXLUDING HISPANICS)-----------

# Generate color palette
my_colors <- c("#FFBF00", "#7915FF",  
               "#FF4900", "grey20", "#1097FF")  

# bar graph of ethnic proportions of employment over time 
acs_race_fig <- acs_race_nothispanic %>% 
  filter(NAME == "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metro Area") %>% 
  plot_ly(
    type = 'bar',
    x =~ as.factor(Year), 
    y =~ emp_prop,
    color =~ race,
    colors = my_colors,
    text =~ race,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y:.1f}%")
  ) %>% 
  layout(barmode = 'stack',
         title = list(text="<br>      Employment Trends in the Construction Industry<br>      in Greater Philadelphia by Ethnicity",
                      x=0,y=1),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         yaxis = list(title = "Proportion of Employees (%)"),
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


##--2b.2 ACS Data: Graph Ethnic Proportions Over Time (INCLUDING HISPANICS)-----------

# bar graph of ethnic proportions of employment over time 
acs_hispanic_fig <- acs_race_hispanic %>% 
  filter(NAME == "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metro Area") %>% 
  plot_ly(
    type = 'bar',
    x =~ as.factor(Year), 
    y =~ emp_prop,
    color =~ race,
    colors = "#FF4900",
    text =~ race,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y:.1f}%")
  ) %>% 
  layout(title = list(text="<br>      Employment Trends in the Construction Industry<br>      in Greater Philadelphia Among Hispanics",
                      x=0,y=1),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         yaxis = list(title = "Proportion of Employees (%)"),
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

acs_hispanic_fig


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
         variable == "total_hawaii" |
         variable == "total_otherrace" |
         variable == "total_multirace" |
         variable == "total_hispanic")

# creating race variable for merging
acs_ethnic$variable <- sub("total_", "", acs_ethnic$variable)

# renaming race variable 
acs_ethnic <- acs_ethnic %>% 
  rename(race = variable)
  

# calculating proportions for each race 
acs_ethnic <- acs_ethnic %>% 
  group_by(Year, NAME) %>% 
  mutate(total_pop = sum(estimate),
         ethnic_prop = (estimate/sum(estimate, na.rm = TRUE)) * 100)


##--2e. ACS Data: Combining Construction Ethnic Data and Overall Ethnic Data----
temp_dta1 <- acs_race %>% 
  select(-c(gender, estimate, total_estimate))

temp_dta2 <- acs_ethnic %>% 
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


##--2f. ACS Data: Graph of Under representation in Construction Industry--------

acs_map <- ethnic_merge %>% 
  filter(NAME == "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD Metro Area") %>% 
  plot_ly(
    type = 'bar',
    x =~ as.factor(race), 
    y =~ proportion,
    color =~ variable,
    colors = c("#1097FF","#FF4900"),
    text =~ race,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y:.1f}%")
  ) %>%
  layout(title = list(text="<br>      Under Representation of Employment in Construction Industry<br>      in Greater Philadelphia by Ethnicity",
                      x=0,y=1),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         yaxis = list(title = "Proportion (%)", tickformat = "%d%%"),
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
         margin = list(l = 70, r = 70, b = 50, t = 70)
  )
acs_map



##--3a. IPUMS Data: filtering for Self-Employed Respondents-----------------------

ipums_construction <- ipums_construction %>% 
  filter(CLASSWKR == "Self-employed" & 
           IND != 3080) # removing this industry code because of almost all 0 values across years

# combining asian race classification
ipums_construction <- ipums_construction %>% 
  mutate(RACE_adj = as.factor(as.character(RACE_adj)),
    RACE_adj = ifelse(RACE_adj == "Chinese", "Asian", 
                       ifelse(RACE_adj == "Japanese", "Asian",
                              ifelse(RACE_adj == "Other Asian or Pacific Islander", "Asian",
                                     ifelse(RACE_adj == "Other race, nec", "Other",
                                            ifelse(RACE_adj == "Two major races", "Multi-Ethnic",
                                                   ifelse(RACE_adj == "Three or more major races", "Multi-Ethnic",
                                                          ifelse(RACE_adj == "White", "White",
                                                                 ifelse(RACE_adj == "Black/African American", "Black/African American",
                                                                        ifelse(RACE_adj == "American Indian or Alaska Native", "American Indian or Alaska Native",
                                                                               ifelse(RACE_adj == "Hispanic", "Hispanic", RACE_adj)))))))))))
# summing employers by race 
ipums_construction <- ipums_construction %>% 
  group_by(YEAR, RACE_adj) %>% 
  mutate(Freq_adj = sum(Freq)) %>% 
  distinct(Freq_adj, .keep_all = TRUE) %>% 
  group_by(YEAR) %>% 
  mutate(prop_employers = (Freq_adj/sum(Freq_adj, na.rm = TRUE)) *100)



##--3b. IPUMS Data: Employer Mapping by Ethnicity-----------------------

# Generate color palette
my_colors <- c("grey", "#FF9200", "darkslategrey", "#FF4900", 
               "#7915FF", "#FFBF00", "#1097FF")  

# bar graph of ethnic proportions of employers over time 
ipums_race_fig <- ipums_construction %>% 
  plot_ly(
    type = 'bar',
    x =~ YEAR, 
    y =~ prop_employers,
    color =~ RACE_adj,
    colors = my_colors,
    text =~ RACE_adj,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y:.1f}%")
  ) %>% 
  layout(barmode = 'stack',
         title = list(text="<br>      Employer Trends in the Construction Industry<br>      in Greater Philadelphia by Ethnicity",
                      x=0,y=1),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         yaxis = list(title = "Proportion of Employers (%)"),
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




##--4a. OES Data: Construction Wage Trends----------------------------------------

oes_graph <- oes_master %>% 
  filter(OCC_CODE == "47-0000" & 
         AREA == "37980")

oes_plot <- plot_ly(data = oes_graph, 
                    x = ~year, 
                    y = ~as.numeric(H_MEAN), 
                    name = "Mean Hourly Wage", 
                    type = 'scatter',
                    mode = 'lines+markers',
                    line = list(color = '#FF4900'),
                    marker = list(color = '#FF4900')) %>%
  
  add_trace(y = ~as.numeric(H_MEDIAN), 
            name = "Median Hourly Wage", 
            mode = 'lines+markers',
            line = list(color = '#1097FF'),
            marker = list(color = '#1097FF')) %>%
  
  add_trace(y = ~as.numeric(A_MEAN), 
            name = "Mean Annual Wage", 
            mode = 'lines+markers', 
            visible = "legendonly",
            line = list(color = '#FFBF00'),
            marker = list(color = '#FFBF00')) %>% 
  
  layout(
         title = list(text="<br>      Wage Trends in the Construction Sector in Greater Philadelphia ",
                      x=0,y=1),
         font = list(family = "Georgia", color = "darkslategrey"),
         hoverlabel = list(font = list(family = "Georgia")),
         yaxis = list(title = "Wage", tickformat = "$"),
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
           text = "Source: OES Data Estimates", # The text of the caption
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

oes_plot

