##--libraries-------------------------------------------------------------------
library(plotly)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

##--1. Employment Trends in Construction---------------------------------------------
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






##--2. Employment Proportion By Ethnicity----------------------------------------------

