#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(plyr)
library(DT)
library(ncaahoopR)



source('pull_shot_locs.R', local = TRUE)


server <- function (input, output,session) {
    
    source("function_callf.R", local = TRUE)
    opps <<- reactive({input$dropdown})
    
    #store reactive values
    specify_team <- reactive({input$specify_team})
    specify_player <- reactive ({input$specify_player})
    specify_year <- reactive ({input$specify_year})
    
    observeEvent(input$getplayers, {
        
        rost<-get_roster(specify_team(), specify_year())
        names <- rost$name
        updateSelectInput(session, "specify_player", choices = names)
        
    })
    
    observeEvent(input$getgames, {
        #old year attempt-----
        if (specify_year() != "2019-20"){
            
            initial_name = specify_team()
            
            split_team_name = strsplit(initial_name, " ")
            if (length(split_team_name[[1]]) > 1){
                words = (split_team_name[[1]])
                ''
                build_name = ''
                for (i in 1:(length(words))){
                    build_name = paste0(build_name,words[i], '_')
                }
                build_name = substr(build_name,1,nchar(build_name)-1)
            }
            else {
                build_name = split_team_name[[1]]
            }
            
            #sched_loc = paste0('2018-19/schedules/', build_name, '_schedule.csv')
            sched_loc = paste0(specify_year(),'/schedules/', build_name, '_schedule.csv')
            #sched <- read.csv(paste0('2018-19/schedules/', build_name, '_schedule.csv'))
            
            s3BucketName <- "cbb-data"
            Sys.setenv("AWS_ACCESS_KEY_ID" = "KEY",
                       "AWS_SECRET_ACCESS_KEY" = "KEY",
                       "AWS_DEFAULT_REGION" = "us-east-2")
            
            
            #non-s3 implementation
            #sched <<- read.csv(paste0(sched_loc))
            
            #s3 implementation
            sched <<- s3read_using(read.csv, object =sched_loc, bucket = s3BucketName)
            
            sched$date <<- as.Date(
                paste0(as.character(sched$date)), 
                format = "%Y-%m-%d")
            
            
            
            items <- as.character(paste(sched$opponent, sched$date))
            updateSelectInput(session, "dropdown", choices = c("",items), selected = if(input$all) c("",items))
        }
        else {
            sched<<-get_schedule(specify_team(),specify_year())
            items <- as.character(paste(sched$opponent, sched$date))
            updateSelectInput(session, "dropdown", choices = c("",items), selected = if(input$all) c("",items))
        }
        
        
        
        #end old year attempt------ (uncomment)
        #sched<-get_schedule(specify_team(),specify_year())
        #items <- as.character(paste(sched$opponent, sched$date))
        #updateSelectInput(session, "dropdown", choices = c("",items), selected = if(input$all) c("",items))
        
    })
    
    
    vals <-reactiveValues(area = NULL,
                          zone = NULL,
                          overall = NULL,
                          shotchart = NULL,
                          fg = NULL,
                          ft = NULL,
                          threept = NULL,
                          made_fg = NULL,
                          total_shots = NULL,
                          made_ft = NULL,
                          total_ft = NULL,
                          total_made_3pt = NULL,
                          total_3pt = NULL,
                          ts = NULL
                          )

    
   # autoInvalidate <- reactiveTimer(30000)
    
    
    data <- observeEvent(input$run, {
        showModal(modalDialog(title="Calculation in progress", "Please allow up to 60 seconds. Progress shown in bottom right corner.", easyClose = TRUE, fade=TRUE))
       
        
        #new --------
        get_dates = function(x){
            substring(x,nchar(x)-10+1)
        }

         #sched <<-get_schedule(specify_team(), specify_year())
         dates1 <<- as.Date(unname(sapply(opps(),get_dates)))
         use_game_ids <<-sched$game_id[sched$date %in% dates1]
         
         #df with date, game_id columns to use for finding game
         pbp_loc <- select(sched, date, game_id)
         #filter this to only keep info from user selected games for easy iteration
         pbp_loc <<-pbp_loc[pbp_loc$date %in% dates1,]
        

        #new --------
        
        if (specify_year() !='2020-21'){
            pull_shot_locs(specify_year(), specify_player(), specify_team())
        } 
         
         else{
        bball(specify_team(),specify_player(),specify_year(), specify_game_ids= use_game_ids)
         }
         
        vals$area <- shot_area_summary
        vals$zone <- shot_zone_summary
        #vals$overall <- overall_summary
        vals$shotchart <- end_plot
        vals$fg <- shooting_percentage
        vals$ft <- ft_shooting_percentage
        vals$threept <- threept_shooting_percentage
        vals$total_shots <- total_shots
        vals$made_fg <- made_fg
        
        vals$made_ft <- made_ft
        vals$total_ft <- total_ft
        vals$total_made_3pt <- total_made_3pt
        vals$total_3pt <- total_3pt
        vals$ts <- ts_percentage
    })
    
   #format dataframes to be clean, remove ugly filter / search /pagination
    options(DT.options = list(dom = 't'))
    
    
    output$fg <- renderInfoBox({
        
        if (vals$fg > 0.6) {
        
        infoBox(
            "FG %", paste0(vals$fg*100, "%"), paste0(made_fg, "/", total_shots) ,icon = icon("basketball-ball"),
            color = "green", fill = TRUE)}
        
        else if (vals$fg < 0.6 & vals$fg > 0.40) {
            infoBox(
                "FG %", paste0(vals$fg*100, "%"),  paste0(made_fg, "/", total_shots) ,icon = icon("basketball-ball"),
                color = "yellow", fill = TRUE)}
        else if (vals$fg > 0) {
            infoBox(
                "FG %", paste0(vals$fg*100, "%"),  paste0(made_fg, "/", total_shots) ,icon = icon("basketball-ball"),
                color = "red", fill = TRUE)}
        else {
            infoBox(
                "FT %", paste0("No FG data from selected game(s)."), icon = icon("plus-circle"),
                color = "blue", fill = TRUE)}
    })
    
    output$ft <- renderInfoBox({
        
        if (vals$ft > 0.80) {
            
            infoBox(
                "FT %", paste0(vals$ft*100, "%"), paste0(made_ft, "/", total_ft) ,icon = icon("plus-circle"),
                color = "green", fill = TRUE)}
        
        else if (vals$ft < 0.8 & vals$ft > 0.70) {
            infoBox(
                "FT %", paste0(vals$ft*100, "%"), paste0(made_ft, "/", total_ft) ,icon = icon("plus-circle"),
                color = "yellow", fill = TRUE)}
        else if (vals$ft > 0) {
            infoBox(
                "FT %", paste0(vals$ft*100, "%"), paste0(made_ft, "/", total_ft) ,icon = icon("plus-circle"),
                color = "red", fill = TRUE)}
        else {
            infoBox(
                "FT %", paste0("No FT data from selected game(s)."), icon = icon("plus-circle"),
                color = "blue", fill = TRUE)}
    })
    
    output$threept <- renderInfoBox({
        
        if (vals$threept > 0.35) {
            
            infoBox(
                "3PT %", paste0(vals$threept*100, "%"), paste0(total_made_3pt, "/", total_3pt) ,icon = icon("bullseye"),
                color = "green", fill = TRUE)}
        
        else if (vals$threept < 0.35 & vals$threept > 0.3) {
            infoBox(
                "3PT %", paste0(vals$threept*100, "%"), paste0(total_made_3pt, "/", total_3pt) ,icon = icon("bullseye"),
                color = "yellow", fill = TRUE)}
        else if (vals$threept > 0) {
            infoBox(
                "3PT %", paste0(vals$threept*100, "%"), paste0(total_made_3pt, "/", total_3pt) ,icon = icon("bullseye"),
                color = "red", fill = TRUE)}
        else {
            infoBox(
                "FT %", paste0("No 3PT data from selected game(s)."), icon = icon("plus-circle"),
                color = "blue", fill = TRUE)}
    })
    
    output$analysis_loc <- renderInfoBox({
        
        if (vals$fg >= 0 ) {
            infoBox(
                "Location Tendency", paste0(shot_location_analysis),'Based on queried game(s)', icon = icon("map-marker-alt"),
                color = "light-blue", fill = TRUE) }
        })
    
    output$analysis_shot <- renderInfoBox({
        
        if (vals$fg >= 0 ) {
            infoBox(
                "Shot Tendency", paste0(shot_location_analysis_2),'Based on queried game(s)', icon = icon("praying-hands"),
                color = "purple", fill = TRUE) }
    })
    
    output$ts <- renderInfoBox({
        
        if (vals$fg >= 0 ) {
            infoBox(
                "True Shooting %", paste0(vals$ts*100, '%'),'Average = 50' ,icon = icon("calculator"),
                color = "orange", fill = TRUE) }
    })
    
    
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
   # output$overall = renderDataTable({
   #     if (is.null(vals$overall)) return()
    #    vals$overall
        # autoInvalidate()
       # overall_summary
   # }) #newlist[1] #overall_summary
    
    output$shotchart = renderPlot({
        bg="transparent"
        if (is.null(vals$shotchart)) return()
        vals$shotchart
        # autoInvalidate()
        # end_plot
    }, bg="transparent", )
    
    
    
}

