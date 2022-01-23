
library(dplyr)
library(lubridate)
library(shiny)
library(viridis)
library(kableExtra)
library(leaflet)
library(shinyAce)
library(shinythemes)

#showfiles =  list(
#  shinyAce::aceEditor("ui",
#                      value = paste(readLines("ui.R"), collapse="\n"),
#                      mode="r", theme="cobalt", height="500px",
#                      readOnly = TRUE),
#  shinyAce::aceEditor("server",
#                      value = paste(readLines("server.R"), collapse="\n"),
#                      mode="r", theme="cobalt", height="500px",
#                      readOnly = TRUE)
#)

#ui=
shiny::fluidPage(theme = shinythemes::shinytheme("slate"),

  tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 15pt !important; }")),

  shiny::br(),
  shiny::h2(list("Visualizing the spread of COVID-19 in the US", shiny::a(shiny::img(src = 'https://raw.githubusercontent.com/jhass/github_desktop_notifications/master/res/icons/GitHub-Mark-Light-32px.png', align = "right"), href = "https://github.com/edpeyton/COVID-19", target = "_blank"))),
  #,
  shiny::helpText("This application animates the spread of COVID-19 throughout the US, using data sourced from the COVID-19 data hub."),
  shiny::br(),
  shiny::helpText("For further details see the References tab."),
  shiny::hr(),
  shiny::br(),
  shiny::tabsetPanel(
    shiny::tabPanel("Map",
                    shiny::br(),
                    shiny::br(),
                    shiny::sidebarLayout(
                      shiny::sidebarPanel(width = 3,
                                          shiny::radioButtons("cumulative",
                                                              "",
                                                              choices = c("New", "Cumulative")),
                                          shiny::br(),
                                          shiny::selectInput("type_choice",
                                                             "Select metric",
                                                             choices = c("Confirmed cases",
                                                                         "Deaths",
                                                                         "Hospitalizations",
                                                                         "ICU cases",
                                                                         "Vaccinations")),
                                          shiny::br(),
                                          shiny::selectInput("per_choice", "Select units", choices = c("Total", "Per capita", "Per case")),
                                          shiny::br(),
                                          #shiny::selectInput("plot_choice",
                                          #                   "Select plot type",
                                          #                   choices = c("Choropleth",
                                          #                               "Markers")),
                                          shiny::br(),
                                          shiny::hr(),
                                          shiny::fluidRow(shiny::column(width = 10,
                                                                        offset = 1,
                                                                        shiny::uiOutput("slider")))
                                          ),
                      shiny::mainPanel(width = 9,
                                       shiny::br(),
                                       shiny::htmlOutput("title"),
                                       leaflet::leafletOutput("map", height = 800),
                                       shiny::br(),
                                       shiny::br())
                      )
                    ),
    shiny::tabPanel("References",
                    shiny::br(),
                    shiny::br(),
                    shiny::fluidRow(shiny::column(width = 10,
                                                  offset = 1,
                                                  shiny::h2("Citation"),
                                                  shiny::p('Guidotti, E., Ardia, D., (2020), "COVID-19 Data Hub", Journal of Open Source Software 5(51):2376, doi: 10.21105/joss.02376.'),
                                                  shiny::br(),
                                                  shiny::hr(),
                                                  shiny::br(),
                                                  shiny::h2("Metadata"),
                                                  shiny::dataTableOutput("citation"),
                                                  shiny::br()),
                                    shiny::column(width = 1)))#,
    #shiny::tabPanel("app.R",
    #                shiny::br(),
    #                shiny::helpText("The following are the ui.R and server.R files used to generate this application, respectively."),
    #                shiny::br(),
    #                showfiles)
    ),
  shiny::hr()

)

