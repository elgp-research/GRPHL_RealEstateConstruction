##--libraries-------------------------------------------------------------------
library(plotly)
library(RColorBrewer)
library(leaflet)
library(sf)
library(waffle)
library(sfheaders)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

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
  group_by(YEAR, RACE_adj, region) %>% 
  mutate(Freq_adj = sum(Freq)) %>% 
  distinct(Freq_adj, .keep_all = TRUE)

# calculating proportions of employers by race
ipums_construction <- ipums_construction %>% 
  group_by(YEAR, region) %>% 
  mutate(prop_employers = (Freq_adj/sum(Freq_adj, na.rm = TRUE)) *100)

##--3c. IPUMS Data + ACS Data: Mapping Employer and Ethnic Distributions------------------------

# creating temporary data to match with ACS data 
temp_dta3 <- ipums_construction %>% 
  mutate(RACE_adj = tolower(RACE_adj),
         RACE_adj =ifelse(RACE_adj == "black/african american", "black",
                          ifelse(RACE_adj == "multi-ethnic", "multirace",
                                 ifelse(RACE_adj == "american indian or alaska native", "native",
                                        ifelse(RACE_adj == "other", "otherrace", RACE_adj))))
  ) %>% 
  rename(race = RACE_adj, 
         Year = YEAR)

# merging datasets
temp_dta4 <- temp_dta2 

employer_map <- temp_dta4 %>% 
  left_join(temp_dta3, by = c("Year", "race")) %>% 
  filter(!is.na(prop_employers) | prop_employers == 0)

# generating average employer and ethnic distributions
employer_map <- employer_map %>% 
  group_by(race) %>% 
  mutate(avg_ethnicity = mean(ethnic_prop, na.rm = TRUE),
         avg_employer = mean(prop_employers, na.rm = TRUE)
  ) %>% 
  distinct(avg_ethnicity, .keep_all = TRUE) %>% 
  select(-c(ethnic_prop, prop_employers)) %>% 
  arrange(avg_employer)

##--3d. IPUMS Data + ACS Data: Graph Employer and Ethnic Distributions------------------------
emp_map_figure <-   plot_ly(data = employer_map, 
                            x = ~reorder(race, avg_employer),
                            y = ~avg_employer, 
                            name = "Average Employer Proportion", 
                            type = 'bar',
                            marker = list(color = '#FF4900')) %>%
  
  add_trace(y = ~avg_ethnicity, 
            name = "Average Ethnic Proportion", 
            type = 'bar',
            marker = list(color = '#1097FF')) %>% 
  
  layout(
    title = list(text="<br>      10-year Average Employer Distribution and Ethnic Makeup of Residents",
                 x=0,y=1),
    font = list(family = "Georgia", color = "darkslategrey"),
    hoverlabel = list(font = list(family = "Georgia")),
    yaxis = list(title = "Proportion (%)"),
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
      text = "Source: IPUMS and ACS Data Estimates", # The text of the caption
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
emp_map_figure



##--3e. IPUMS Data: Employer Distribution by Age--------------------------------

# Generate color palette
my_colors <- c("#FF9200", "#1097FF", "#FF4900", 
               "#7915FF", "#FFBF00")  

# bar graph of ethnic proportions of employers over time 
ipums_age_fig <- ipums_age %>% 
  plot_ly(
    type = 'bar',
    x =~ YEAR, 
    y =~ age_prop,
    color =~ age_bracket,
    colors = my_colors,
    text =~ age_bracket,
    hovertemplate=paste("<i>%{text} in %{x}:</i><br>%{y:.1f}%")
  ) %>% 
  layout(barmode = 'stack',
         title = list(text="<br>      Employer Trends in the Construction Industry<br>      in Greater Philadelphia by Age",
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

ipums_age_fig
##--3f. IPUMS Data: Employer Distribution by Gender--------------------------------

# Create a named vector of data
ipums_gender <- c(`Females`=5, `Males`= 95)

# Create a waffle plot
waffle(ipums_gender, rows=10,
       colors = c("#FF4900", "#1097FF"),
       legend_pos = "bottom") + 
  labs(title = "Proportion of Employers in the Construction Sector \nin Greater Philadelphia by Gender",
       caption = "Note: Each box represents one percentage of the entire construction industry \nin Greater Philadelphia. These estimates are 16-year average of employer \nproportion across gender.") + 
  theme(
    text = element_text(family = "Georgia", color = "darkslategrey"),
    plot.title = element_text(size = 14, hjust = 0),
    plot.caption = element_text(size = 8, hjust = 0, color = "grey50"))

