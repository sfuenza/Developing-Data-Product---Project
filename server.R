library(shiny)
library(ggplot2)
library(maps)
library(rCharts)
library(mapproj)
library(markdown)

#Load some functions
source("functions.R", local = TRUE)

#Load dataset
arrests<-read.csv("data/USArrests.csv",header=TRUE,sep=";")
#Get states names
states<-as.character(arrests$State)
#Generate the map of the states for the graphics
map_of_states<-map_data("state")


shinyServer(function(input, output,session) {
      
      #We initialize values of the reactives variables
      reac_values <- reactiveValues()
      reac_values$states <- states
      reac_values$fill <-"Murder"
      reac_values$compare<-"North_South"
      
      #Separation of states for the compare part
      north_states<-c("washington","oregon","idaho","montana","wyoming","north dakota","south dakota","nebraska","minnesota",
                      "iowa","wisconsin","illinois","michigan","indiana","ohio","pennsylvania","new jersey","connecticut",
                      "rhode island","massachusetts","new hampshire","vermont","maine","new york")
      south_states<- states[-which(states %in% north_states)]
      
      west_states<-c("wahsington","oregon","idaho","montana","wyoming","california","hawaii","nevada","arizona","utah",
                     "colorado","new mexico")
      central_states<-c("north dakota","south dakota","nebraska","minnesota","iowa","wisconsin","illinois", "kansas",
                        "missouri","oklahoma","arkansas","texas","louisiana")
      east_states<-states[-which(states %in% c(west_states,central_states))]
      
      #Create columns for North_South and West_Central_East
      arrests$North_South<-"South"
      arrests[which(arrests$State %in% north_states),]$North_South<-"North"
      arrests$West_Central_East<-"East"
      arrests[which(arrests$State %in% west_states),]$West_Central_East<-"West"
      arrests[which(arrests$State %in% central_states),]$West_Central_East<-"Central"
      
      #The checkbox of States
      output$statesCheckbox <- renderUI({
            if(1) {
                  checkboxGroupInput('states',"", states, selected=reac_values$states)
            }
      })
      
      #Observers for each of the buttons
      observe({
            if(input$murder == 0) return()
            reac_values$fill <- "Murder"
      })
      
      observe({
            if(input$rape == 0) return()
            reac_values$fill <- "Rape"
      })
      
      observe({
            if(input$assault == 0) return()
            reac_values$fill <- "Assault"
      })
      
      observe({
            if(input$north_south == 0) return()
            reac_values$compare <- "North_South"
      })
      
      observe({
            if(input$west_central_east == 0) return()
            reac_values$compare <- "West_Central_East"
      })
      
      observe({
            if(input$bystates == 0) return()
            reac_values$compare <- "all_states"
      })
      
      observe({
            if(input$byregions == 0) return()
            reac_values$compare <- "by_region"
      })
      
      #Update the dataset depending on which states are selected
      arrests.select <- reactive({
       temp<-arrests
           if(length(input$states)==0)
           {
                 temp$Murder<-NA
                 temp$Rape<-NA
                 temp$Assault<-NA
                 temp$UrbanPop<-NA
           }
           else if(length(input$states)<49)
           {
                 temp[-which(arrests$State %in% input$states),]$Murder<-NA
                 temp[-which(arrests$State %in% input$states),]$Rape<-NA
                 temp[-which(arrests$State %in% input$states),]$Assault<-NA
                  temp[-which(arrests$State %in% input$states),]$UrbanPop<-NA
            }
            return(temp)
      })
      
      #Sum murders depending of selected compare
      arrests.select.murders <- reactive({
            sum_murders(arrests.select(),reac_values$compare)
      })
      #Sum rapes depending of selected compare
      arrests.select.rapes <- reactive({
            sum_rapes(arrests.select(),reac_values$compare)
      })
      #Sum assaults depending of selected compare
      arrests.select.assaults <- reactive({
            sum_assaults(arrests.select(),reac_values$compare)
      })
      #Sum urban population depending of selected compare
      arrests.select.urban <- reactive({
            sum_urban(arrests.select(),reac_values$compare)
      })
      
      #REactive for the dataset_Table
      Dataset_Table_generate <- reactive({
            arrests.select()
      })
      
      
      #Map Graphic for type of arrests
      output$statesGraphic <- renderPlot({
            title=paste("N° of arrests for",reac_values$fill,"per 100.000 residents in 1973 by States")
            
            print(plot_graphic_map(
                  data=arrests.select(),
                  map_of_states=map_of_states,
                  fill=reac_values$fill,
                  title=title
                  
            )
            )
            
      })
      #Map Graphic for urban population
      output$urbanPopulationGraphic <- renderPlot({
            title="Percent USA Urban Population in 1973 by States"
                        print(plot_graphic_map(
                  data=arrests.select(),
                  map_of_states=map_of_states,
                  fill="UrbanPop",
                  title=title,
                  min_col="white",
                  max_col="blue"
                  
                  
            )
            )
            
      })
      
      #Barplots for each type of arrests depending on the comparison
      output$urbanGraphic <- renderPlot({
            urban<-ggplot(arrests.select.urban(),aes(x=Zone, y=UrbanPop, fill=Zone)) + geom_bar(stat="identity")
            urban<- urban + ylab("Percent of urban population")
            urban
      })
      
      output$murdersGraphic <- renderPlot({
            murder<-ggplot(arrests.select.murders(),aes(x=Zone, y=Murder, fill=Zone)) + geom_bar(stat="identity")
            murder<- murder+ ylab("Number of Murders per 100.000 residents") 
            murder
      })
      
      output$assaultsGraphic <- renderPlot({
            assaults<-ggplot(arrests.select.assaults(),aes(x=Zone, y=Assault, fill=Zone)) + geom_bar(stat="identity")
            assaults <- assaults + ylab("Number of Murders per 100.000 residents") 
            assaults
      })
      
      output$rapesGraphic <- renderPlot({
            rapes<-ggplot(arrests.select.rapes(),aes(x=Zone, y=Rape, fill=Zone)) + geom_bar(stat="identity")
            rapes<- rapes + ylab("Number of Rapes per 100.000 residents") 
            rapes    
      })
      
      #Generates the table with all the data selectd
      output$dataset_table <- renderDataTable(
            {arrests.select()}, options = list(bFilter = FALSE, iDisplayLength = 50))
      
      
})