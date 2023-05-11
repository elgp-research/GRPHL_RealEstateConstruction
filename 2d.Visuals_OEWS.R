##--libraries-------------------------------------------------------------------
library(plotly)
library(RColorBrewer)
library(leaflet)
library(sf)
library(waffle)
library(sfheaders)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

##--4a. OES Data: Identifying Construction Occupations----------------------------------------

# Occupation titles 
occupation_titles <- oes_philly$OCC_TITLE

# Define regular expressions to match Construction sector occupation titles
construction_regex <- "(?i)(Architect|Building Contractor|Building Inspector|Building Maintenance Technician|Building Surveyor|Carpenter|Concrete Finisher|Construction Equipment Operator|Construction Laborer|Construction Manager|Construction Project Manager|Construction Superintendent|Crane Operator|Drywaller|Electrician|Environmental Engineer|Estimator|Fire Sprinkler Installer|Flooring Installer|General Contractor|Glazier|Heavy Equipment Operator|HVAC Technician|Interior Designer|Ironworker|Landscape Architect|Landscaper|Mason|Painter|Pipefitter|Plumber|Project Engineer|Real Estate Agent|Roofing Contractor|Roofer|Scaffolder|Sheet Metal Worker|Structural Engineer|Surveyor|Tiler|Welder)"

# Use grep function to extract occupation titles that match the Construction sector regex
construction_occupations <- unique(grep(construction_regex, occupation_titles, value = TRUE))

##--4b. OES Data: Creating Dummy for Construction Jobs------------------------------------------


# filtering for "Manufacturing" and "All Occupations" 
oes_graph <- oes_philly %>% 
  filter(OCC_CODE == "47-0000" | OCC_CODE == "00-0000") %>% 
  select(PRIM_STATE, AREA, OCC_CODE, OCC_TITLE, year, H_MEAN_real, H_MEDIAN_real, A_MEAN_real, A_MEDIAN_real) %>% 
  gather(wage_type, amount, H_MEAN_real:A_MEDIAN_real) %>% 
  mutate(year_date = as.Date(paste0(year, "-01-01")),
         wage_type = ifelse(wage_type == "A_MEAN_real", "Mean Annual Wage",
                            ifelse(wage_type == "A_MEDIAN_real", "Median Annual Wage",
                                   ifelse(wage_type == "H_MEAN_real", "Mean Hourly Wage",
                                          ifelse(wage_type == "H_MEDIAN_real", "Median Hourly Wage", wage_type))))
  ) 

# ggplot of wages
oes_graph %>% 
  ggplot(aes(x=year_date, y = amount, color = OCC_TITLE)) + 
  geom_line(aes(linetype = OCC_TITLE, group = OCC_TITLE)) +  
  facet_wrap(~wage_type, scale = "free_y") + 
  scale_linetype_manual(values = c("dashed", "solid")) + 
  scale_color_manual(values = c("#1097FF", "#FF4900")) +
  scale_y_continuous(labels = function(x) paste0("$", x)) + 
  labs(y = "Wages", x = "", 
       title = "Wages in Greater Philadelphia for the Construction Sector",
       caption = "Note: All wages are inflation-adjusted to 2022 dollars. \nSource: OES Data") +
  theme_light() + 
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
        plot.title = element_text(size = 15, margin = margin(b = 10, t = 5), color = "darkslategrey", hjust = 0.5),
        plot.subtitle = element_text(size = 10, color = "grey40", margin = margin(b = 10)),
        plot.caption = element_text(size = 8, margin = margin(t = 10), color = "grey50", hjust = 0),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.title.x = element_text(margin = margin(t = 10, r = 10, b = 0, l = 10), color = "darkslategrey", ),
        axis.ticks.x = element_blank(),
        plot.margin = margin(0.2,0.2,0.2,0.2, "cm"))



