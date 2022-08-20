# shiny::runApp('path')
# setwd("/media/manu/BD_manu/Shiny/test")

# if(!require(dplyr)) install.packages("dplyr")#, repos = "http://cran.us.r-project.org")
library(dplyr)
# library(rgdal)
# library(sp)
library(raster)
library(leaflet)
library(plotly)
library(shiny)
library(shinythemes)

source("helper.R") #uso stn_ls

shinyUI(
    navbarPage(theme = shinytheme("flatly"), #sandstone
              collapsible = TRUE,
              HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Name APP</a>'), 
              id="nav",
              windowTitle = "nosepaqesesto",
           
              tabPanel("Name TAB", 
                       sidebarPanel(
                                    sliderInput("bins",
                                                "Number of bins:",
                                                min = 1,
                                                max = 50,
                                                value = 30)
                                   ),
                        plotOutput("distPlot")
                        ),
              
              navbarMenu("More",
                          tabPanel("Summary",
                                   h1("titulo 1", align="center"),
                                   hr(),
                                   fluidRow(
                                            column(3,
                                                   p("Este es un ej de parrafo", align="left")
                                                   ),
                                            column(6,
                                                    # img(src="logo.png", height="100%", width="100%") #media/manu/BD_manu/Shiny/test/
                                                    # HTML('<img src="luffy_dark.jpg", height="400px",
                                                    #     style="float:right"/>','<p style="color:black"></p>')
                                                   imageOutput('image01'),
                                                   align = "center"
                                                   ),
                                            column(3,
                                                   p("Este sigue seindo un ej de parrafo", align="right")
                                                    )
                                            )
                                   )
                          # "----",
                          # "Section header",
                          # tabPanel("Table")
                        ),
              tabPanel("VISOR TAB",
                       h3("ejemplo h3"),
                       
                       DT::dataTableOutput("table"), #stn_senamhi
                       column(8, verbatimTextOutput('rows')),
                       
                       selectInput("selectrows", label = "rows",
                                  choices = selrows, selected=c(1),multiple = TRUE),
                       
                       p("parrafo last"),
                       leafletOutput('ubi_stn'),
                       br(),
                       
                       selectInput("selectvar", label = "vars",
                                   choices = selvars), #, selected,multiple = TRUE),
                       plotlyOutput("stn_plot")

                    ),
              tabPanel('Plot line',
                       selectInput('stn', label='Estaciones', choices=stn_ls,
                                   selected='Iquitos'),
                       selectInput('select_stn', label = 'vars_2',
                                   choices = stn_vars, selected='MIN'), #, selected,multiple = TRUE),
                       plotlyOutput("met"),
                       column(8, verbatimTextOutput('rows2'))
                       )
              #hr(),
              #footer = "Version: 0.0.1"
            )
        )


# como utilizar data de directorios ajenos a test?