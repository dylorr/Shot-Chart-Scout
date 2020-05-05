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
library(ncaahoopR)


server <- function (input, output,session) {
    
    source("function_callf.R", local = TRUE)
    
    opps <<- reactive({input$dropdown})
    
    #store reactive values
    specify_team <- reactive({input$specify_team})
    specify_player <- reactive ({input$specify_player})
    specify_year <- reactive ({input$specify_year})
    
    observeEvent(input$getgames, {
        
        sched<-get_schedule(specify_team(),specify_year())
        items <- as.character(paste(sched$opponent, sched$date))
        updateSelectInput(session, "dropdown", choices = items)
        
    })
    
    
    vals <-reactiveValues(area = NULL,
                          zone = NULL,
                          overall = NULL,
                          shotchart = NULL)

    
   # autoInvalidate <- reactiveTimer(30000)
    
    
    data <- observeEvent(input$run, {
        showModal(modalDialog(title="Calculation in progress", "Please allow up to 60 seconds. Progress shown in bottom right corner.", easyClose = TRUE, fade=TRUE))
       
        
        #new --------
        get_dates = function(x){
            substring(x,nchar(x)-10+1)
        }
        
         sched <<-get_schedule(specify_team(), specify_year())
         dates1 <- as.Date(unname(sapply(opps(),get_dates)))
         use_game_ids <-sched$game_id[sched$date %in% dates1]
        #new --------
        
        bball(specify_team(),specify_player(),specify_year(), specify_game_ids= use_game_ids)
        vals$area <- shot_area_summary
        vals$zone <- shot_zone_summary
        vals$overall <- overall_summary
        vals$shotchart <- end_plot
        
    })
   
    
    options(DT.options = list(dom = 't'))
    
    
    output$area = renderDataTable({
        if (is.null(vals$area)) return()
        vals$area
        
        # autoInvalidate()
        #shot_area_summary
    })#newlist[3] #shot_area_summary 
    output$zone = renderDataTable({
        if (is.null(vals$zone)) return()
        vals$zone
        # autoInvalidate()
       # shot_zone_summary
    }) #newlist[2] #shot_zone_summary
    output$overall = renderDataTable({
        if (is.null(vals$overall)) return()
        vals$overall
        # autoInvalidate()
       # overall_summary
    }) #newlist[1] #overall_summary
    output$shotchart = renderPlot({
        if (is.null(vals$shotchart)) return()
        vals$shotchart
        # autoInvalidate()
        # end_plot
    })
    
    
    
    
}

