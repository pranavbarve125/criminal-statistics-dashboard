library(readxl)
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(scales)
options(scipen = 100)
###################################################

criminal_status <- read_excel(path = "Data_Tables_LGA_Criminal_Incidents_Year_Ending_December_2022.xlsx", sheet = "Table 05")
criminal_status <- criminal_status %>% pivot_wider(
  names_from = "Charge Status",
  values_from = "Incidents Recorded",
  values_fill = 0
)

###################################################


# Import the "Sheet1" tab from the Excel file
criminal_locations <- read_excel(path = "Data_Tables_LGA_Criminal_Incidents_Year_Ending_December_2022.xlsx", sheet = "Table 04")

criminal_locations$`Location Division` <- gsub("^\\d+\\s", "", criminal_locations$`Location Division`)

# Group the data by "Local Government Area" and "Location Division" and sum the incidents
criminal_locations <- criminal_locations %>%
  group_by(`Year`, `Local Government Area`, `Location Division`) %>%
  summarise(Total = sum(`Incidents Recorded`, na.rm = TRUE)) %>%
  pivot_wider(
    names_from = `Location Division`,
    values_from = `Total`,
    values_fill = 0
  )


####################
criminals <- merge(criminal_status, criminal_locations, by = c("Year", "Local Government Area"))

# adding a total statistics
criminals$`Total Crime` <- criminals$Community + criminals$Other + criminals$Residential

###########################
victims_location <- read_excel(path = "Data_Tables_LGA_Victim_Reports_Year_Ending_December_2022.xlsx", sheet = "Table 02")

victims_location <- victims_location %>% select(-c("Offence Subdivision")) %>%
  group_by(`Year`, `Local Government Area`, `Offence Division`) %>%
  summarise(Total = sum(`Victim Reports`)) %>%
  pivot_wider(
    names_from = `Offence Division`,
    values_from = `Total`,
    values_fill = 0
  )


###############################
victims_age <- read_excel(path = "Data_Tables_LGA_Victim_Reports_Year_Ending_December_2022.xlsx", sheet = "Table 03")

victims_age <- victims_age %>% 
  pivot_wider(
    names_from = `Age Group`,
    values_from = `Victim Reports`,
    values_fill = 0
  )


#################################


victims_sex <- read_excel(path = "Data_Tables_LGA_Victim_Reports_Year_Ending_December_2022.xlsx", sheet = "Table 04")

victims_sex <- victims_sex %>%
  pivot_wider(
    names_from = `Sex`,
    values_from = `Victim Reports`,
    values_fill = 0
  )

################################

victims <- inner_join(victims_location, victims_age, by = c("Year", "Local Government Area")) %>% 
  inner_join(victims_sex, by = c("Year", "Local Government Area"))

################################
ui <- dashboardPage(
  dashboardHeader(title = "Criminal Statstics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Criminal Incidents", tabName = "page1"),
      menuItem("Victim Reports", tabName = "page2")
    )
  ),
  dashboardBody(
    tabItems(
    tabItem(tabName = "page1",
              fluidRow(
                valueBoxOutput('totalCrime'),
                valueBoxOutput('totalUnsolvedCrime'),
                valueBoxOutput('totalResidentialCrime')),
              fluidRow(
                box(
                  title = "Slicers",
                  status = "primary",
                  height = 300,
                  width = 4,
                  sliderInput("year", "Year", min = min(criminals$Year), max = max(criminals$Year), value = c(2013,2020)),
                  selectInput("location", "Suburbs",
                              choices = c(criminals$`Local Government Area`, "All"),
                              multiple = TRUE, selected = "All")
                ),
                box(
                  title = "Type of Crimes",
                  status = "warning",
                  height = 300,
                  width = 8,
                  plotOutput("crimeType", height = "250px")
                ),
              ),
              fluidRow(
                box(
                  title = "Crime Status",
                  status = "primary",
                  height = 300,
                  plotOutput("crimeStatus", height = "250px", width = "100%")
                ),
              )
            ),
    tabItem(tabName = "page2",
              fluidRow(
                valueBoxOutput("femaleVictims"),
                valueBoxOutput('seniorVictims'),
                valueBoxOutput('publicSecurityBreach')
              ),
              fluidRow(
                box(
                  title = "Slicers",
                  status = "primary",
                  height = 300,
                  width = 4,
                  sliderInput("victimYear", "Year", min = min(victims$Year), max = max(victims$Year), value = c(2013,2020)),
                  selectInput("victimLocation", "Suburbs",
                              choices = c(victims$`Local Government Area`, "All"),
                              multiple = TRUE, selected = "All")
                ),
                box(
                  title = "Age of victims",
                  status = "warning",
                  height = 300,
                  width = 8,
                  plotOutput("age", height = "250px")
                )
              )
            )
  ))
)
server <- function(input, output) {
  output$barPlot1 <- renderPlot({
    #get filtered data
    data <- criminals[criminals$Year >= input$year[1] & criminals$Year <= input$year[2], ]
    data <- subset(data, ifelse(input$location == "All", TRUE, data$`Local Goverment Area` == input$location))
    plot(data$Year, data$`Charges laid`)
  })
  
  output$scatterPlot1 <- renderPlot({
    #get filtered data
    data <- victims[victims$victimYear >= input$victimYear[1] & victims$victimYear <= input$victimYear[2], ]
    data <- subset(data, ifelse(input$victimLocation == "All", TRUE, data$`Local Goverment Area` == input$victimLocation))
    plot(data$Year, data$Unsolved)
  })
  
  output$age <- renderPlot({
    data <- victims[victims$Year >= input$victimYear[1] & victims$Year <= input$victimYear[2], ]
    data <- subset(data, ifelse(input$victimLocation == "All", TRUE, data$`Local Goverment Area` == input$victimLocation))
    p <- data.frame(
      Type = c('00 - 24 years', '25 - 34 years', '35 - 44 years', '45 - 54 years', '55+ years'),
      Count = c(
        sum(data$`00 - 24 years`), 
        sum(data$`25 - 34 years`), 
        sum(data$`35 - 44 years`), 
        sum(data$`45 - 54 years`), 
        sum(data$`55+ years`))
    )
    par(mar = c(5, 6, 4, 2))
    barplot(p$Count, names.arg = p$Type, horiz = TRUE, xlab = "Count", main = "Bar Graph", xlim=c(0, max(p$Count)*1.2), las = 1)
  })
  
  output$crimeStatus <- renderPlot({
    data <- criminals[criminals$Year >= input$year[1] & criminals$Year <= input$year[2], ]
    data <- subset(data, ifelse(input$location == "All", TRUE, data$`Local Goverment Area` == input$location))
    p <- data.frame(
      Type = c('Charges laid', 'No charges laid', 'Unsolved'),
      Count = c(sum(data$`Charges laid`), sum(data$`No charges laid`), sum(data$`Unsolved`))
    )
    #barplot(p$Count, names.arg = p$Type, xlab = "Category", ylab = "Count", main = "Bar Graph")
    pie(p$Count, labels = paste0(percent(p$Count / sum(p$Count))), col = rainbow(length(p$Type)))
    legend("topright", legend = p$Type, fill = rainbow(length(p$Type)))
  })
  
  output$crimeType <- renderPlot({
    data <- criminals[criminals$Year >= input$year[1] & criminals$Year <= input$year[2], ]
    data <- subset(data, ifelse(input$location == "All", TRUE, data$`Local Goverment Area` == input$location))
    p <- data.frame(
      Type = c('Community', 'Other', 'Residential'),
      Count = c(sum(data$`Community`), sum(data$`Other`), sum(data$`Residential`))
    )
    par(mar = c(5, 6, 4, 2))
    barplot(p$Count, names.arg = p$Type, horiz = TRUE, xlab = "Count", main = "Bar Graph", xlim=c(0, max(p$Count)*1.1), las = 1)
  })
  
  
  
  output$totalCrime <- renderValueBox({
    data <- criminals[criminals$Year >= input$year[1] & criminals$Year <= input$year[2], ]
    data <- subset(data, ifelse(input$location == "All", TRUE, data$`Local Goverment Area` == input$location))
    valueBox(sum(data$`Total Crime`), 'Total criminal incidents')
  })
  
  output$totalUnsolvedCrime <- renderValueBox({
    data <- criminals[criminals$Year >= input$year[1] & criminals$Year <= input$year[2], ]
    data <- subset(data, ifelse(input$location == "All", TRUE, data$`Local Goverment Area` == input$location))
    valueBox(sum(data$`Unsolved`), 'Unsolved incidents')
  })
  
  output$totalResidentialCrime <- renderValueBox({
    data <- criminals[criminals$Year >= input$year[1] & criminals$Year <= input$year[2], ]
    data <- subset(data, ifelse(input$location == "All", TRUE, data$`Local Goverment Area` == input$location))
    valueBox(sum(data$`Residential`), 'Residential Crimes')
  })
  
  output$femaleVictims <- renderValueBox({
    data <- victims[victims$Year >= input$victimYear[1] & victims$Year <= input$victimYear[2], ]
    data <- subset(data, ifelse(input$victimLocation == "All", TRUE, data$`Local Goverment Area` == input$victimLocation))
    valueBox(sum(data$`Females`), 'Female Victims')
  })
  
  output$seniorVictims <- renderValueBox({
    data <- victims[victims$Year >= input$victimYear[1] & victims$Year <= input$victimYear[2], ]
    data <- subset(data, ifelse(input$victimLocation == "All", TRUE, data$`Local Goverment Area` == input$victimLocation))
    valueBox(sum(data$`55+ years`), 'Senior Victims')
  })
  
  output$publicSecurityBreach <- renderValueBox({
    data <- victims[victims$Year >= input$victimYear[1] & victims$Year <= input$victimYear[2], ]
    data <- subset(data, ifelse(input$victimLocation == "All", TRUE, data$`Local Goverment Area` == input$victimLocation))
    valueBox(sum(data$`D Public order and security offences`), 'Public Security Breach')
  })

}

shinyApp(ui = ui, server = server)
