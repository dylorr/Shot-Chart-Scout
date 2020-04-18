#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(DT)

server <- function (input, output) {
    
    source("function_callf.R", local = TRUE)
    
    
    
    #store reactive values
    specify_team <<- reactive({input$specify_team})
    specify_player <<- reactive ({input$specify_player})
    specify_year <<- reactive ({input$specify_year})
    
    autoInvalidate <- reactiveTimer(60000)
    
    
    data <- observeEvent(input$run, {
        showModal(modalDialog(title="Calculation in progress", "Please allow up to 60 seconds. Progress shown in bottom right corner.", easyClose = TRUE, fade=TRUE))
        bball(specify_team(),specify_player(),specify_year())
        shot_area_summary <- shot_area_summary
        shot_zone_summary <- shot_zone_summary
        overall_summary <- overall_summary
        end_plot <- end_plot
        
    })
    
    
    output$area = renderDataTable({
        autoInvalidate()
        shot_area_summary
    })#newlist[3] #shot_area_summary 
    output$zone = renderDataTable({
        autoInvalidate()
        shot_zone_summary
    }) #newlist[2] #shot_zone_summary
    output$overall = renderDataTable({
        autoInvalidate()
        overall_summary
    }) #newlist[1] #overall_summary
    output$shotchart = renderPlot({
        autoInvalidate()
        end_plot
    })
    
    
    vals <-reactiveValues(area = NULL,
                          zone = NULL,
                          overall = NULL,
                          shotchart = NULL)
    
}

