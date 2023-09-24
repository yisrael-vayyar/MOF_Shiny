#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(tidyverse)

schools <- read_excel("../data/schools_full.xlsx")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("MOF Schools Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          
          
              sliderInput("year",
                          "Select Years:",
                          min = min(schools$year),
                          max = max(schools$year),
                          value = c(min(schools$year), max(schools$year)),
                          step = 1,
                          sep = '',
              ),
              br(),
              checkboxGroupInput("district", "איזור",
                                 choiceNames = unique(schools$district),
                                 choiceValues = unique(schools$district),
                                 selected =  unique(schools$district)
              ),

          
              br(),
              
              checkboxGroupInput("SchoolEduType", "סוג מוסד חינוכי",
                                 choiceNames = unique(schools$school_edu_type),
                                 choiceValues = unique(schools$school_edu_type),
                                 selected =  unique(schools$school_edu_type)
              ),
              
              br(),
              
              checkboxGroupInput("ClassEduType", "סוג כיתה",
                                 choiceNames = unique(schools$class_edu_type),
                                 choiceValues = unique(schools$class_edu_type),
                                 selected = unique(schools$class_edu_type),
              ),
              
              br(),
              
              checkboxGroupInput("ClassType", "סוג לקות:",
                                 choiceNames = unique(schools$class_type),
                                 choiceValues = unique(schools$class_type),
                                 selected = unique(schools$class_type),
              ),
              checkboxInput("selectall","Select All", value = TRUE, width = NULL),
              br(),
              

            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
        
           plotOutput("allPlot"),
           
           br(),
           
           plotOutput("perDist"),
           
           br()

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    observe({
      if(input$selectall == FALSE)
      {
        updateCheckboxGroupInput(session,"ClassType",choices=unique(schools$class_type))
      }
      else
      {
        updateCheckboxGroupInput(session,"ClassType",choices=unique(schools$class_type),selected=unique(schools$class_type))
      }
    })
  
      output$allPlot <- renderPlot({
        schools %>%
          filter(year >= input$year[1] & year <= input$year[2]) %>%
          filter(class_type %in% input$ClassType) %>%
          filter(school_edu_type %in% input$SchoolEduType) %>%
          filter(class_edu_type %in% input$ClassEduType) %>%
          filter(district %in% input$district) %>%
          group_by(year,class_type) %>%
          summarise(students = sum(students)) %>%
          ggplot(mapping = aes(x = year,y = students, group=class_type, col=class_type)) + geom_line()
      })
  
      
    output$perDist <- renderPlot({
      schools %>%
        filter(year >= input$year[1] & year <= input$year[2]) %>%
        filter(class_type %in% input$ClassType) %>%
        filter(school_edu_type %in% input$SchoolEduType) %>%
        filter(class_edu_type %in% input$ClassEduType) %>%
        filter(district %in% input$district) %>%
        group_by(year,class_type, district) %>%
        summarise(students = sum(students)) %>%
        ggplot(mapping = aes(x = year,y = students, group=class_type, col=class_type)) + geom_line() + facet_wrap(~district, ncol = 2)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
