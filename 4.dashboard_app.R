##--installing libraries----
library(shiny)
library(shinydashboard)
library(ggplot2)

##--sourcing data file----
#source("3.Funding_Analysis.R")

##--creating User Interface for Dashboard----

# Example data frame
df7_clean <- data.frame(
  region = c("Philadelphia", "Philadelphia", "Greater Philadelphia", "Greater Philadelphia",
             "Philadelphia", "Philadelphia", "Greater Philadelphia", "Greater Philadelphia",
             "Philadelphia", "Philadelphia", "Greater Philadelphia", "Greater Philadelphia"),
  awarding_agency_name = c("Department of Energy", "Department of Education", "Department of Education", "Department of Energy",
                           "Department of Energy", "Department of Education", "Department of Education", "Department of Energy",
                           "Department of Energy", "Department of Education", "Department of Education", "Department of Energy"),
  cfda_title = c("Grant A", "Grant B", "Grant C", "Grant D",
                 "Grant A", "Grant B", "Grant C", "Grant D",
                 "Grant A", "Grant B", "Grant C", "Grant D"),
  assistance_type_description = c("A", "B", "C", "D",
                                  "A", "B", "C", "D",
                                  "A", "B", "C", "D"),
  business_types_description = c("Business", "Local Governmnet", "State government", "Business",
                                 "Business", "Local Governmnet", "State government", "Business",
                                 "Business", "Local Governmnet", "State government", "Business"),
  total_obligated_amount = c(100, 120, 140, 160, 150, 170, 190, 210, 220, 240, 260, 280),
  year = c(2011, 2011, 2011, 2011,
           2012, 2012, 2012, 2012,
           2013, 2013, 2013, 2013)
)

ui <- fluidPage(
  titlePanel("Federal Assistance Dashboard /nInfrastructure Investment"),
  sidebarLayout(
    sidebarPanel(
      selectInput("region","Select a region:", choices=c("", unique(df7_clean$region))),
      conditionalPanel(
        condition="input.region != ''",
        selectInput("awarding_agency_name","Select an awarding agency:", choices=NULL)
      ),
      conditionalPanel(
        condition="input.awarding_agency_name != ''",
        selectInput("business_types_description","Select a business type:", choices=NULL)
      ),
      conditionalPanel(
        condition="input.business_types_description != ''",
        selectInput("cfda_title","Select a assistance type:", choices=NULL)
      )
    ),
    mainPanel(
      textOutput("text"),
      plotOutput("plot")
    )
  )
)

server <- function(input,output,session) {
  # Update awarding agency choices based on selected region
  observeEvent(input$region,{
    updateSelectInput(session,"awarding_agency_name",
                      choices=c("",unique(df7_clean$awarding_agency_name[df7_clean$region==input$region]))
    )
  })
  
  # Update business type choices based on selected awarding agency
  observeEvent(input$awarding_agency_name,{
    updateSelectInput(session,"business_types_description",
                      choices=c("",unique(df7_clean$business_types_description[df7_clean$awarding_agency_name==input$awarding_agency_name]))
    )
  })
  
  # Update CFDA title choices based on selected business type
  observeEvent(input$business_types_description,{
    updateSelectInput(session,"cfda_title",
                      choices=c("",unique(df7_clean$cfda_title[df7_clean$business_types_description==input$business_types_description]))
    )
  })
  
  # Create reactive data frame based on selected inputs
  data <- reactive({
    if (input$cfda_title != "") {
      df7_clean[df7_clean$cfda_title == input$cfda_title & df7_clean$year == max(df7_clean$year), ]
    } else if (input$business_types_description != "") {
      aggregate(total_obligated_amount ~ business_types_description + assistance_type_description + year,
                df7_clean[df7_clean$business_types_description == input$business_types_description & df7_clean$year == max(df7_clean$year), ], sum)
    } else if (input$awarding_agency_name != "") {
      aggregate(total_obligated_amount ~ awarding_agency_name + assistance_type_description + year,
                df7_clean[df7_clean$awarding_agency_name == input$awarding_agency_name & df7_clean$year == max(df7_clean$year), ], sum)
    } else if (input$region != "") {
      aggregate(total_obligated_amount ~ region + assistance_type_description + year,
                df7_clean[df7_clean$region == input$region & df7_clean$year == max(df7_clean$year), ], sum)
    } else {
      data.frame()
    }
  })
  
  # Create reactive data frame for line plot
  plot_data <- reactive({
    if (input $cfda_title != "") {
      df7_clean[df7_clean $cfda_title == input $cfda_title , ]
    } else if (input $business_types_description != "") {
      aggregate(total_obligated_amount ~ year + business_types_description + assistance_type_description + cfda_title + awarding_agency_name + region,
                df7_clean[df7_clean $business_types_description == input $business_types_description , ], sum)
    } else if (input $awarding_agency_name != "") {
      aggregate(total_obligated_amount ~ year + awarding_agency_name + assistance_type_description + cfda_title + region,
                df7_clean[df7_clean $awarding_agency_name == input $awarding_agency_name , ], sum)
    } else if (input $region != "") {
      aggregate(total_obligated_amount ~ year + region + assistance_type_description + cfda_title,
                df7_clean[df7_clean $region == input $region , ], sum)
    } else {
      data.frame()
    }
    
  })
  
  # Display grant description and funding amount for most recent year as text
  output$text <- renderText({
    if (nrow(data()) >0) {
      if (input $cfda_title != "" && input $business_types_description !="") {
        paste0(
          "\nFunding: $ ", formatC(sum(data() $total_obligated_amount), big.mark=", ", format="d")
        )
      } else {
        paste0("Funding: $ ", formatC(sum(data() $total_obligated_amount), big.mark=", ", format="d"))
      }
    }
  })
  
  # Display line plot of funding over time
  output $plot <- renderPlot({
    if(nrow(plot_data()) >0) {
      ggplot(plot_data(), aes(x=year,y=total_obligated_amount)) +
        geom_line() +
        geom_point()
    }
  })
}

shinyApp(ui=ui , server=server)
