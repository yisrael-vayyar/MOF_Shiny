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
library(leaflet)
library(plotly)
library(highcharter)

schools <- read_excel("schools_new.xlsx")
coord <- read_excel("coordinates.xlsx") %>%
  mutate(long = as.double(long), lat = as.double(lat))
schools <- merge(x = schools, y = coord, by.x = "town", by.y = "address", all.x = TRUE)

gradeOrder = c("גן גילאי 5-3",
               "טרום חובה",
               "חובה",
               "א",
               "ב",
               "ג",
               "ד",
               "ה",
               "ו",
               "ז",
               "ח",
               "ט",
               "י",
               "יא",
               "יב",
               "יג",
               "יד")
               
schools$grade = ordered(schools$grade, gradeOrder)

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
              checkboxGroupInput("sector", "מגזר",
                                 choiceNames = unique(schools$sector),
                                 choiceValues = unique(schools$sector),
                                 selected =  unique(schools$sector)
              ),
                                 
                                 
              br(),
              checkboxGroupInput("district", "איזור",
                                 choiceNames = unique(schools$district),
                                 choiceValues = unique(schools$district),
                                 selected =  unique(schools$district)
              ),

          
              br(),
              checkboxGroupInput("edu_level", "שלב חינוך כיתה",
                                 choiceNames = unique(schools$edu_level),
                                 choiceValues = unique(schools$edu_level),
                                 selected =  unique(schools$edu_level)
              ),
              checkboxGroupInput("supervision", "פיקוח",
                                 choiceNames = unique(schools$supervision),
                                 choiceValues = unique(schools$supervision),
                                 selected =  unique(schools$supervision)
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
           plotlyOutput("TotalPlot"),
           plotlyOutput("TotalPlotBar"),
           plotlyOutput("allPlot"),
           plotlyOutput("allBars"),
           br(),
           
           plotlyOutput("perDist"),
           plotlyOutput("perDistBar"),
           
           br(),
           plotlyOutput("plotByGrade"),
           
           br(),
            h2("Map scatter for the year", textOutput(outputId = "mapYear", inline=T), "."),
           leafletOutput("myMap", height = 1000)

        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    base <- reactive({
      schools %>%
        filter(year >= input$year[1] & year <= input$year[2]) %>%
        filter(sector %in% input$sector) %>%
        filter(school_edu_type %in% input$SchoolEduType) %>%
        filter(class_edu_type %in% input$ClassEduType) %>%
        filter(supervision %in% input$supervision) %>%
        filter(district %in% input$district) %>%
        filter(edu_level %in% input$edu_level)

    })
    
    filtered <- reactive({
      base() %>%
        filter(class_type %in% input$ClassType)
    })
    
    observeEvent(list(input$year, input$district, input$SchoolEduType, input$ClassEduType) , {
      updateCheckboxGroupInput(session,"ClassType",choices=unique(base()$class_type), selected = input$ClassType )
    })
    
    observeEvent(input$selectall, {
      
      if(input$selectall == FALSE)
      {
        updateCheckboxGroupInput(session,"ClassType",choices=unique(base()$class_type))
      }
      else
      {
        updateCheckboxGroupInput(session,"ClassType",choices=unique(base()$class_type),selected=unique(base()$class_type))
      }
    })
      output$TotalPlot <- renderPlotly({
        filtered() %>%
          group_by(year,school_edu_type) %>%
          summarise(students = sum(students), boys_num = sum(boys_num), girls_num = sum(girls_num))  %>%
          ggplot(aes(group=school_edu_type, col=school_edu_type)) +
            geom_line(aes(x = year,y = students))  + scale_color_manual(values = c("בנות" = '#ff00ff','בנים' = '#3399ff', 'רגיל' = "orange", 'מיוחד' = "lightgreen")) +
            geom_line(aes(x = year,y = boys_num)) + geom_point(mapping = aes(x = year,y = boys_num, color = 'בנים')) +
            geom_line(aes(x = year,y = girls_num)) + geom_point(mapping = aes(x = year,y = girls_num, color = 'בנות'))
            
      })
    
      output$TotalPlotBar <- renderPlotly({
        filtered() %>%
          pivot_longer(cols=c('boys_num', 'girls_num'), names_to='sex',values_to='sex_num') %>%
          group_by(year,school_edu_type, sex) %>%
          summarise(students = sum(sex_num))  %>%
          ggplot(data, mapping = aes(fill=sex, y=students, x=year))  + 
            scale_fill_manual(values = c(girls_num = '#ff00ff',boys_num = '#3399ff')) + 
            scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            geom_bar(position="fill", stat='identity') +
            facet_wrap(~school_edu_type, ncol = 2)

        
      })
      
      output$allPlot <- renderPlotly({
        filtered() %>%
          group_by(year,class_type) %>%
          summarise(students = sum(students)) %>%
          ggplot(mapping = aes(x = year,y = students, group=class_type, col=class_type)) + geom_line()
      })
  
      output$allBars <- renderPlotly({
        filtered() %>%
          group_by(year,class_type) %>%
          summarise(students = sum(students)) %>%
          ggplot(data, mapping = aes(fill=class_type, y=students, x=year)) + geom_bar(position="fill", stat='identity')
      })
      
    output$perDist <- renderPlotly({
      filtered() %>%
        group_by(year,class_type, district) %>%
        summarise(students = sum(students)) %>%
        ggplot(mapping = aes(x = year,y = students, group=class_type, col=class_type)) + geom_line() + facet_wrap(~district, ncol = 2)
    })
    
    output$perDistBar <- renderPlotly({
      filtered() %>%
        group_by(year,class_type, district) %>%
        summarise(students = sum(students)) %>%
        ggplot(data, mapping = aes(fill=class_type, y=students, x=year)) + geom_bar(position="fill", stat='identity') + facet_wrap(~district, ncol = 2)
    })
    
    output$plotByGrade <- renderPlotly({
      filtered() %>%
        group_by(year, class_type, grade) %>%
        summarise(students = sum(students)) %>%
        ggplot(mapping = aes(x = grade,y = students, group=class_type, col=class_type)) + geom_line() + facet_wrap(~year, ncol = 2)
    })
    
    output$myMap <- renderLeaflet({
      
      pal <- colorFactor(
        palette = 'Dark2',
        domain = filtered()$class_type
      )
      output$mapYear <- renderText(input$year[2])
      map_data <- filtered() %>%
        filter(year == input$year[2]) %>%
        group_by(year,class_type, long, lat) %>%
        summarise(students = sum(students))
      
      base_map <- leaflet() %>%
        addTiles() %>%
        addScaleBar() %>%
        setView(lat = mean(map_data$lat, na.rm=TRUE), lng = mean(map_data$long, na.rm=TRUE), zoom = 10) %>%
        addMiniMap()
      
      base_map %>% addCircleMarkers(
        lng = map_data$long, lat = map_data$lat,
        label = paste(map_data$students , map_data$class_type),
        color = pal(map_data$class_type),
        stroke = FALSE, fillOpacity = 0.5) %>%
        addLegend("topleft", pal = pal, values = map_data$class_type, opacity = 1)
      
      })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
