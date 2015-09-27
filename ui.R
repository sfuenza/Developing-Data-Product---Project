library(shiny)
library(rCharts)
require(markdown)

shinyUI(
      navbarPage("USA arrest in 1973", theme="bootstrap.css",
                 tabPanel("Graphics",
                        sidebarPanel(
                              h2("States"),
                              uiOutput("statesCheckbox")
                              
      ),
      
      mainPanel(
            
            tabsetPanel(
            
            #Map Graphics
            tabPanel(p(icon("map-marker"), "Maps"),
                     column(12,
                            wellPanel(
                                  h3("Select Type of Arrest:"),
                                  actionButton("murder", "Murder",class="btn btn-info"),
                                  actionButton("rape", "Rape",class="btn btn-info"),
                                  actionButton("assault", "Assault",class="btn btn-info")
                                 )
                     ),
                     column(12,
                            plotOutput("statesGraphic")
                            ),
                     column(12,
                            wellPanel(
                             h3("Urban Population:")
                     )
                             ),
                     column(12,
                            plotOutput("urbanPopulationGraphic")
                            )
                     ),
            tabPanel(p(icon("bar-chart"), "Compare"),
                     column(12,
                            wellPanel(
                                  h3("Select what to compare"),
                                  actionButton("bystates", "All States",class="btn btn-default"),
                                  actionButton("north_south", "North vs South",class="btn btn-default"),
                                  actionButton("west_central_east", "West vs Central vs East",class="btn btn-default"),
                                  actionButton("byregions", "By Regions",class="btn btn-default")
                                  )
                            ),
                     column(12,class="panel panel-info",
                            h4("Urban Population", align="center",class="panel-heading"),
                            plotOutput("urbanGraphic")
                            ),
                     column(12,class="panel panel-danger",
                            h4("Murders", align="center",class="panel-heading"),
                            plotOutput("murdersGraphic")
                            ),
                     column(12,class="panel panel-success",
                            h4("Assaults", align="center",class="panel-heading"),
                            plotOutput("assaultsGraphic")
                     ),
                     column(12,class="panel panel-warning",
                            h4("Rapes", align="center",class="panel-heading"),
                            plotOutput("rapesGraphic")
                            )
                     ),
            tabPanel(p(icon("table"),"Data Set"),
                     dataTableOutput("dataset_table")
                     )
            )
      )),
      tabPanel("Information",
               mainPanel(
                     includeMarkdown("info.md")
                     )
               )
      
      )
)
      
