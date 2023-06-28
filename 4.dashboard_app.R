##--installing libraries----
library(shiny)
library(shinydashboard)
library(ggplot2)

##--sourcing data file----
#source("3.Funding_Analysis.R")

##--creating User Interface for Dashboard----

# Example data frame
df_grphl <- data.frame(
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
            2013, 2013, 2013, 2013),
   recipient_name = c("Haseeb","Alina","Annie","Mike",
                      'Alvin','Simon','Haseeb','Alina',
                      'Alexander','Haseeb','Alina','Phillip'),
   recipient_address = c("123 Main St.", "Elm St.", "Oak St.", " Pine St.",
                         " Maple St.", " Birch St.", " Cedar St.", " Spruce St.",
                         " Willow St.", " Cherry St.", " Walnut St.", " Chestnut St."),
   recipient_city = c("Philadelphia","Philadelphia","Philadelphia","Philadelphia",
                      'Philadelphia','Philadelphia','Philadelphia','Philadelphia',
                      'Philadelphia','Philadelphia','Philadelphia','Philadelphia')
)

ui <- dashboardPage(
  dashboardHeader(title="Federal Funding"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard",
               selectInput("region","Select a region:", choices=c("", unique(df_grphl$region))),
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
                 selectInput("cfda_title","Select a CFDA title:", choices=NULL)
               )
      )
    )
  ),
  dashboardBody(
    fluidRow(
      # tags$style("#text {font-size: 30px;}"),
      # box(textOutput("text"), width = 2, height = "32em"),
      box(plotOutput("plot"), width = 12),
      box(tableOutput("table"), width = 12)
      #box(tableOutput("table"), width = 12)
      
    )
  )
)

server <- function(input, output, session) {
  # Update awarding agency choices based on selected region
  observeEvent(input$region,{
    updateSelectInput(session,"awarding_agency_name",
                      choices=c("",unique(df_grphl$awarding_agency_name[df_grphl$region==input$region]))
    )
  })
  
  # Update business type choices based on selected awarding agency
  observeEvent(input$awarding_agency_name,{
    updateSelectInput(session,"business_types_description",
                      choices=c("",unique(df_grphl$business_types_description[df_grphl$awarding_agency_name==input$awarding_agency_name]))
    )
  })
  
  # Update CFDA title choices based on selected business type
  observeEvent(input$business_types_description,{
    updateSelectInput(session,"cfda_title",
                      choices=c("",unique(df_grphl$cfda_title[df_grphl$business_types_description==input$business_types_description]))
    )
  })
  
  data <- reactive({
    filtered_data <- df_grphl
    
    if (input$cfda_title != "") {
      filtered_data <- filtered_data[filtered_data$cfda_title == input$cfda_title, ]
    }
    
    if (input$business_types_description != "") {
      filtered_data <- filtered_data[filtered_data$business_types_description == input$business_types_description, ]
    }
    
    if (input$awarding_agency_name != "") {
      filtered_data <- filtered_data[filtered_data$awarding_agency_name == input$awarding_agency_name, ]
    }
    
    if (input$region != "") {
      filtered_data <- filtered_data[filtered_data$region == input$region, ]
    }
    
    if (nrow(filtered_data) > 0) {
      if (input$cfda_title == "") {
        aggregate(total_obligated_amount ~ region + year, filtered_data, sum)
      } else {
        filtered_data
      }
    } else {
      data.frame()
    }
  })
  
  plot_data <- reactive({
    filtered_data <- df_grphl
    
    if (input$cfda_title != "") {
      filtered_data <- filtered_data[filtered_data$cfda_title == input$cfda_title, ]
    }
    
    if (input$business_types_description != "") {
      filtered_data <- filtered_data[filtered_data$business_types_description == input$business_types_description, ]
    }
    
    if (input$awarding_agency_name != "") {
      filtered_data <- filtered_data[filtered_data$awarding_agency_name == input$awarding_agency_name, ]
    }
    
    if (input$region != "") {
      filtered_data <- filtered_data[filtered_data$region == input$region, ]
    }
    
    if (nrow(filtered_data) > 0) {
      if (input$cfda_title == "") {
        aggregate(total_obligated_amount ~ year, filtered_data, sum)
      } else {
        filtered_data
      }
    } else {
      data.frame()
    }
  })
  
  table_data <- reactive({
    filtered_data <- df_grphl
    
    if (input$region != "") {
      filtered_data <- filtered_data[filtered_data$region == input$region, ]
    }
    
    filtered_data %>%
      group_by(recipient_name, recipient_address) %>%
      summarize(total_grants = sum(total_obligated_amount)) %>%
      top_n(5, total_grants) %>%
      arrange(desc(total_grants)) %>% 
      select(recipient_name, recipient_address_line_1, total_grants)
  })
  
  
  
  # # Create reactive data frame based on selected inputs
  # data <- reactive({
  #   if (input$cfda_title != "") {
  #     df_grphl[df_grphl$cfda_title == input$cfda_title & df_grphl$year == max(df_grphl$year), ]
  #   } else if (input$business_types_description != "") {
  #     aggregate(total_obligated_amount ~ business_types_description + assistance_type_description + year,
  #               df_grphl[df_grphl$business_types_description == input$business_types_description & df_grphl$year == max(df_grphl$year), ], sum)
  #   } else if (input$awarding_agency_name != "") {
  #     aggregate(total_obligated_amount ~ awarding_agency_name + assistance_type_description + year,
  #               df_grphl[df_grphl$awarding_agency_name == input$awarding_agency_name & df_grphl$year == max(df_grphl$year), ], sum)
  #   } else if (input$region != "") {
  #     aggregate(total_obligated_amount ~ region + assistance_type_description + year,
  #               df_grphl[df_grphl$region == input$region & df_grphl$year == max(df_grphl$year), ], sum)
  #   } else {
  #     data.frame()
  #   }
  # })
  # 
  # # Create reactive data frame for line plot
  # plot_data <- reactive({
  #   if (input $cfda_title != "") {
  #     df_grphl[df_grphl $cfda_title == input $cfda_title , ]
  #   } else if (input $business_types_description != "") {
  #     aggregate(total_obligated_amount ~ year + business_types_description + assistance_type_description + cfda_title + awarding_agency_name + region,
  #               df_grphl[df_grphl $business_types_description == input $business_types_description , ], sum)
  #   } else if (input $awarding_agency_name != "") {
  #     aggregate(total_obligated_amount ~ year + awarding_agency_name + assistance_type_description + cfda_title + region,
  #               df_grphl[df_grphl $awarding_agency_name == input $awarding_agency_name , ], sum)
  #   } else if (input $region != "") {
  #     aggregate(total_obligated_amount ~ year + region + assistance_type_description + cfda_title,
  #               df_grphl[df_grphl $region == input $region , ], sum)
  #   } else {
  #     data.frame()
  #   }
  #   
  # })
  # 
  # # Display grant description and funding amount for most recent year as text
  # output$text <- renderText({
  #   if (nrow(data()) >0) {
  #     if (input $cfda_title != "" && input $business_types_description !="") {
  #       paste0(
  #         "\nMost Recent Funding Amount: $ ", formatC(sum(data() $total_obligated_amount), big.mark=", ", format="d")
  #       )
  #     } else {
  #       paste0("Most Recent Funding Amount: $ ", formatC(sum(data() $total_obligated_amount), big.mark=", ", format="d"))
  #     }
  #   }
  # })
  # 
  # Display line plot of funding over time
  output$plot <- renderPlot({
    if(nrow(plot_data()) >0) {
      ggplot(plot_data(), aes(x=year,y=as.numeric(total_obligated_amount))) +
        geom_line() +
        geom_point()
    }
  })
  # 
  # Create reactive data frame for table
  table_data <- reactive({
    df_grphl %>%
      group_by(year) %>%
      top_n(3,total_obligated_amount) %>%
      arrange(year,-total_obligated_amount) %>%
      ungroup() %>% 
      select(recipient_name, recipient_address_line_1, total_obligated_amount)

  })
  # 
  # Display table of top recipients
  # output$table <- renderTable({
  #   table_data()
  # })
  # # 
  # output$table <- renderTable({
  #   table_data()
  # })
  
  # # Highlight top three rows by year
  # observe({
  #   req(input$table_rows_all)
  #   
  #   selected_rows <- which(rownames(table_data()) %in% input$table_rows_all[1:3])
  #   
  #   shinyjs::runjs(
  #     sprintf("$('#table tr:eq(%s)').css('background-color', '#FFFF00');", 
  #             selected_rows+1)
  #   )
  #   
  # })
}

shinyApp(ui=ui , server=server)

