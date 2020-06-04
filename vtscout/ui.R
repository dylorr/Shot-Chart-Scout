#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("htmltools")
addDeps <- function(x) {
    if (getOption("shiny.minified", TRUE)) {
        adminLTE_js <- "app.min.js"
        adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
    } else {
        adminLTE_js <- "app.js"
        adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
    }
    
    dashboardDeps <- list(
        htmlDependency("AdminLTE", "2.0.6",
                       c(file = system.file("AdminLTE", package = "shinydashboard")),
                       script = adminLTE_js,
                       stylesheet = adminLTE_css
        ),
        htmlDependency("shinydashboard",
                       as.character(utils::packageVersion("shinydashboard")),
                       c(file = system.file(package = "shinydashboard")),
                       script = "shinydashboard.js",
                       stylesheet = "shinydashboard.css"
        )
    )
    
    shinydashboard:::appendDependencies(x, dashboardDeps)
}

library(shiny)
library(DT)
library(shinythemes)
library(shinydashboard)
load("ncaa_colors.rda")    

# Define UI for application that draws a histogram


ui <- fluidPage(
    
    
    tags$head(tags$style(
        HTML(
            '.content-wrapper,
            .right-side {
                background-color: #ffffff;
                margin-left: 20px;
            }
            
            .content {
            min-height: 0px;
            }'
        )
    )),
    
    
    #theme = shinytheme("cyborg"),
    titlePanel("Shot Chart Scout Tool v3.2"),
    
    sidebarLayout(
        sidebarPanel(
            #textInput(inputId = 'specify_team', label = "Specify Team", placeholder= "Virginia Tech"),
            selectInput(inputId = 'specify_team', label = "Specify Team", choices = c("",ncaa_colors$espn_name), selected = NULL),
            selectInput(inputId = 'specify_year', label = "Specify Year", choices = c("", "2020-21 (N/A)", "2019-20", "2018-19", "2017-18"), selected=NULL),
            actionButton("getplayers", "Get Players"),
            br(),
            br(),
            selectInput(inputId = 'specify_player', label = "Specify Player", choices="", selected=NULL),
            #textInput(inputId = 'specify_player', label = "Specify Player", placeholder= "Jalen Cone"),
            em('Check this box to run analysis on all possible games. All games will be pre-selected.'),
            checkboxInput('all', 'Pre-select all games', value=FALSE),
            
            actionButton("getgames", "Get Games"),
            br(),
            br(),
            selectInput("dropdown", 'Specify Games', choices = "", selected = NULL, multiple = TRUE),
            actionButton("run", "Run"),
            br(),
            br(),
            img(src='basketball.png', height=37.7,width = 250, style="display: block; margin-left: auto; margin-right: auto;"),
            h6("Created by", 
            a('Dylan Orrell', href='https://www.linkedin.com/in/dylan-orrell/', align='center'), align='center'),
            br(),
            tags$div(tags$ol(
                tags$li(tags$span("Select desired team from the dropdown menu.")),
                tags$li(tags$span("Select desired year from the dropdown menu.")),
                tags$li(tags$span("Click the 'Get Players' button.")),
                tags$li(tags$span("Select desired player from the newly populated dropdown menu.")),
                tags$li(tags$span("If you plan to run analysis on the full season of games (or up to the current number of completed games), select the checkbox to auto-fill the games selection.")),
                tags$li(tags$span("Click the 'Get Games' button.")),
                tags$li(tags$span("Select desired combination of games to perform analysis on.")),
                tags$li(tags$span("Click run and view results. Modify inputs and click run again to perform another query. If you encounter an error where you see 'Disconnected from Server', simply refresh the webpage and modify your query."))))
            ),
        
        mainPanel(
            
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            plotOutput('shotchart'),
            dashboardBody(
            fluidRow(
                infoBoxOutput('fg'),
                infoBoxOutput('ft'),
                infoBoxOutput('threept')
            ),
            fluidRow(
                infoBoxOutput('ts'),
                infoBoxOutput('analysis_loc'),
                infoBoxOutput('analysis_shot'))),
           # dataTableOutput('overall', width = '80%'),
           
           
            dataTableOutput('area', width = '80%'),
            dataTableOutput('zone', width = '80%')
        ))
)

# Attach dependencies
ui <- addDeps(
    tags$body(shiny::fluidPage(ui)
    )
)



