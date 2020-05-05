#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram


ui <- fluidPage(
    titlePanel("Shot Chart Scout Tool"),
    
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = 'specify_team', label = "Specify Team", placeholder= "Virginia Tech"),
            textInput(inputId = 'specify_player', label = "Specify Player", placeholder= "Jalen Cone"),
            textInput(inputId = 'specify_year', label = "Specify Year", placeholder= "2019-20"),
            actionButton("getgames", "Get Games"),
            br(),
            br(),
            selectInput("dropdown", 'Select Games', choices = "", selected = NULL, multiple = TRUE),
            actionButton("run", "Run"),
            br(),
            br(),
            img(src='basketball.png', height=37.7,width = 250, style="display: block; margin-left: auto; margin-right: auto;"),
            h6("Created by", 
            a('Dylan Orrell', href='https://www.linkedin.com/in/dylan-orrell/', align='center'), align='center')),
        
        mainPanel(
            
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            ),
            plotOutput('shotchart'),
            dataTableOutput('overall', width = '80%'),
            dataTableOutput('area', width = '80%'),
            dataTableOutput('zone', width = '80%')
        ))
)



