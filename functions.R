
#Function that plot the Map Graphic
plot_graphic_map <- function(data, map_of_states, fill, title,min_col="yellow", max_col="#a51606"){
      
      if(sum(is.na(data$Murder))==49)
      {
            
            p <- ggplot(data, aes(map_id = State))
            p <- p + geom_map(map = map_of_states, colour='gray')
            p <- p + expand_limits(x = map_of_states$long, y = map_of_states$lat)
            p <- p + coord_map() + theme_bw()
            p + labs(x = "Long", y = "Lat", title = title)
            
      }
      else
      {
            
            p <- ggplot(data, aes(map_id = State))
            p <- p + geom_map(aes_string(fill = fill), map = map_of_states, colour='gray')
            p <- p + expand_limits(x = map_of_states$long, y = map_of_states$lat)
            p <- p + coord_map() + theme_bw()
            p <- p + labs(x = "Longitude", y = "Latitude", title = title)
            p + scale_fill_gradient(low = min_col, high = max_col, na.value = "black")
            
      }
      
}

#Function that adds the total of murders depending of the selected compare
sum_murders <- function(data, compare)
{
      if(compare =="North_South")
      {
            murders<-aggregate(Murder ~ North_South,data=data,FUN=sum)
            names(murders)<-c("Zone","Murder")
      }
      else if(compare == "West_Central_East")
      {
            murders<-aggregate(Murder ~ West_Central_East,data=data,FUN=sum)
            names(murders)<-c("Zone","Murder")
      }
      else if(compare == "by_region")
      {
            murders<-aggregate(Murder ~ North_South+West_Central_East,data=data,FUN=sum)
            murders$Zone<-paste(murders$North_South, murders$West_Central_East)
            murders$North_South<-NULL
            murders$West_Central_East<-NULL
      }
      else if (compare == "all_states")
      {
            murders<-data
            murders$Zone<-murders$State
            murders$State<-NULL
      }
      return(murders)
}

#Function that adds the total of rapes depending of the selected compare
sum_rapes <- function(data, compare)
{
      
      if(compare =="North_South")
      {
            rapes<-aggregate(Rape ~ North_South,data=data,FUN=sum)
            names(rapes)<-c("Zone","Rape")
      }
      else if(compare == "West_Central_East")
      {
            rapes<-aggregate(Rape ~ West_Central_East,data=data,FUN=sum)
            names(rapes)<-c("Zone","Rape")
      }
      else if(compare == "by_region")
      {
            rapes<-aggregate(Rape ~ North_South+West_Central_East,data=data,FUN=sum)
            rapes$Zone<-paste(rapes$North_South, rapes$West_Central_East)
            rapes$North_South<-NULL
            rapes$West_Central_East<-NULL
      }
      else if (compare == "all_states")
      {
            rapes<-data
            rapes$Zone<-rapes$State
            rapes$State<-NULL
      }
      
      return(rapes)
}

#Function that adds the total of assaults depending of the selected compare
sum_assaults <- function(data, compare)
{
      if(compare =="North_South")
      {
            assaults<-aggregate(Assault ~ North_South,data=data,FUN=sum)
            names(assaults)<-c("Zone","Assault")
      }
      else if(compare == "West_Central_East")
      {
            assaults<-aggregate(Assault ~ West_Central_East,data=data,FUN=sum)
            names(assaults)<-c("Zone","Assault")
      }
      else if(compare == "by_region")
      {
            assaults<-aggregate(Assault ~ North_South+West_Central_East,data=data,FUN=sum)
            assaults$Zone<-paste(assaults$North_South, assaults$West_Central_East)
            assaults$North_South<-NULL
            assaults$West_Central_East<-NULL
      }
      else if (compare == "all_states")
      {
            assaults<-data
            assaults$Zone<-assaults$State
            assaults$State<-NULL
      }
      return(assaults)
}

#Function that adds the total of urban population depending of the selected compare
sum_urban <- function(data, compare)
{
      if(compare =="North_South")
      {
            UrbanPop<-aggregate(UrbanPop ~ North_South,data=data,FUN=sum)
            names(UrbanPop)<-c("Zone","UrbanPop")
      }
      else if(compare == "West_Central_East")
      {
            UrbanPop<-aggregate(UrbanPop ~ West_Central_East,data=data,FUN=sum)
            names(UrbanPop)<-c("Zone","UrbanPop")
      }
      else if(compare == "by_region")
      {
            UrbanPop<-aggregate(UrbanPop ~ North_South+West_Central_East,data=data,FUN=sum)
            UrbanPop$Zone<-paste(UrbanPop$North_South, UrbanPop$West_Central_East)
            UrbanPop$North_South<-NULL
            UrbanPop$West_Central_East<-NULL
      }
      else if (compare == "all_states")
      {
            UrbanPop<-data
            UrbanPop$Zone<-UrbanPop$State
            UrbanPop$State<-NULL
      }
      return(UrbanPop)
}

#Function that generates a barplot with the murders arrests
plot_murders <- function(data)
{
      murderPlot<- nPlot(Murder ~ Zone, color="Zone", data=data, type="multiBarChart",width=650)
      murderPlot$yAxis(axisLabel="Number of Murders arrests",width=80)
      murderPlot$xAxis(axisLabel="Zones",width=70)
      murderPlot
}

#Function that generates a barplot with the murders arrests
plot_urban <- function(data)
{
      urbanPlot<- nPlot(UrbanPop ~ Zone, data=data,color="Zone", type="multiBarChart",width=650)
      urbanPlot
}

#Function that generates a barplot with the murders arrests
plot_rapes <- function(data)
{
      rapesPlot<- nPlot(Rape ~ Zone, color="Zone", data=data, type="multiBarChart",width=650)
      rapesPlot$yAxis(axisLabel="Number of Rape arrests ",width=80)
      rapesPlot$xAxis(axisLabel="Zones",width=70)
      rapesPlot
}

#Function that generates a barplot with the murders arrests
plot_assaults <- function(data)
{
      assaultsPlot<- nPlot(Assault ~ Zone, color="Zone", data=data, type="multiBarChart",width=650)
      assaultsPlot$yAxis(axisLabel="Number of Assaults arrests ",width=80)
      assaultsPlot$xAxis(axisLabel="Zones",width=70)
      assaultsPlot
}
