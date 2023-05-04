##--libraries-------------------------------------------------------------------
library(plotly)
library(RColorBrewer)

##--sourcing R file-------------------------------------------------------------------
source("1.Data_Wrangle.R")

##--1. CES Data: Indexed Growth of Construction----------------------------------------
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

