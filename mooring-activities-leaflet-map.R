
# HTML map of planned mooring deployment and recovery activities

library(tidyverse)
library(leaflet)
library(leaflegend)
library(sf)
library(htmltools)
library(htmlwidgets)

# read in eez shapefile
#eez<-readOGR("C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/Canada_EEZ/EEZ_WGS.shp")

# load shapefiles for MPAs & critical habitat
nbwCH<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/NBW_CH_WGS1984/NBW_CH_WGS1984.shp')
narwCH<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/NARW_CH_WGS1984/NARW_CH_WGS1984.shp')
fcbbAOI<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/FCBB_AOI_WGS1984/FCBB_AOI_WGS1984.shp')
allMPAs<-read_sf('C:/Users/STANISTREETJ/Documents/arcgis/R_shapefiles/MPAs_WGS1984/MPAs_WGS1984.shp')

# read in site data
mooring_sites<-read_csv("mooring_activities_2023.csv") %>% 
  mutate(Activity = as_factor(Activity)) %>% 
  mutate(Project = as_factor(Project))

recover<-mooring_sites %>% filter(Activity == 'Recovery')
recover_deploy<-mooring_sites %>% filter(Activity == 'Recovery & Deployment')
deploy<-mooring_sites %>% filter(Activity == 'Deployment')

#####
# start basemap
map <- leaflet() %>% 
  
  # add ocean basemap
  addProviderTiles(
    "Esri.OceanBasemap",
    options = providerTileOptions(
      variant = "Ocean/World_Ocean_Base")) %>%
  
  # focus map in a certain area / zoom level
  setView(lng = -63, lat = 44, zoom = 6) %>% 
  
  # add measuring tool
  addMeasure(
    position = "bottomleft",
    primaryLengthUnit = "kilometers",
    secondaryLengthUnit = NULL,
    primaryAreaUnit = "sqkilometers",
    secondaryAreaUnit = NULL) %>% 
  
  # add Canada EEZ
  # addPolylines(data = eez,
  #              group = "EEZ",
  #              #stroke = T,
  #              weight = 2,
  #              color = 'blue',
  #              #fill = F,
  #              smoothFactor = 0.5) %>%
  
  # add MPAs
  addPolygons(data = allMPAs,
              group = 'MSP',
              stroke = T,
              weight = 1,
              color = 'grey',
              fill = T,
              fillColor = 'grey',
              fillOpacity = 0.35,
              smoothFactor = 3) %>%
  
  # add NARW CH
  addPolygons(data = narwCH,
              group = 'MSP',
              stroke = T,
              weight = 1,
              color = 'grey',
              fill = T,
              fillColor = 'grey',
              fillOpacity = 0.35,
              smoothFactor = 3) %>%
  
  # add NBW CH
  addPolygons(data = nbwCH,
              group = 'MSP',
              stroke = T,
              weight = 1,
              color = 'grey',
              fill = T,
              fillColor = 'grey',
              fillOpacity = 0.35,
              smoothFactor = 3) %>%
  
  # add AOI
  addPolygons(data = fcbbAOI,
              group = 'MSP',
              stroke = T,
              dashArray = c(5,5),
              weight = 1,
              color = 'grey',
              fill = T,
              fillColor = 'grey',
              fillOpacity = 0.25,
              smoothFactor = 3) %>%
  
  # add mooring recovery sites
  addCircleMarkers(data = recover,
                   ~Longitude, ~Latitude,
                   label = paste(recover$Site,": ",recover$Mooring),
                   weight = 0.5,
                   col = 'black',
                   fillColor = "darkred",
                   radius = 5,
                   fillOpacity = 0.7,
                   stroke = T,
                   #clusterOptions = markerClusterOptions(disableClusteringAtZoom=9),
                   #clusterOptions = markerClusterOptions(maxClusterRadius=3, disableClusteringAtZoom=9),
                   group = 'Mooring Sites',
                   popup=(paste0(
                     "Station: ", recover$Site, "<br>",
                     "Mooring Ops: ", recover$Mooring, "<br>",
                     "Latitude: ", recover$Latitude, "<br>",       
                     "Longitude: ", recover$Longitude,"<br>",
                     "Depth: ", recover$Depth, " m", "<br>",
                     #"Operation: ", pam$Activity, "<br>",
                     "Project: ", recover$Project, "<br>",
                     "Contact: ", recover$Contact, "<br>")
                   )
  ) %>%
  
  # add recovery & deployment sites
  addCircleMarkers(data = recover_deploy,
                   ~Longitude, ~Latitude,
                   label = paste(recover_deploy$Site,": ",recover_deploy$Mooring),
                   weight = 0.5,
                   col = 'black',
                   fillColor = "darkblue",
                   radius = 5,
                   fillOpacity = 0.7,
                   stroke = T,
                   #clusterOptions = markerClusterOptions(maxClusterRadius=3, disableClusteringAtZoom=9),
                   group = 'Mooring Sites',
                   popup=(paste0(
                     "Station: ", recover_deploy$Site, "<br>",
                     "Mooring Ops: ", recover_deploy$Mooring, "<br>",
                     "Latitude: ", recover_deploy$Latitude, "<br>",
                     "Longitude: ", recover_deploy$Longitude,"<br>",
                     "Depth: ", recover_deploy$Depth, " m", "<br>",
                     #"Operation: ", benthic$Activity, "<br>",
                     "Project: ", recover_deploy$Project, "<br>",
                     "Contact: ", recover_deploy$Contact, "<br>")
                   )
  ) %>%

  # add deployment sites
  addCircleMarkers(data = deploy,
                   ~Longitude, ~Latitude,
                   label = paste(deploy$Site,": ",deploy$Mooring),
                   weight = 0.5,
                   col = 'black',
                   fillColor = "yellow",
                   radius = 5,
                   fillOpacity = 0.7,
                   stroke = T,
                   #clusterOptions = markerClusterOptions(maxClusterRadius=3, disableClusteringAtZoom=9),
                   group = 'Mooring Sites',
                   popup=(paste0(
                     "Station: ", deploy$Site, "<br>",
                     "Mooring Ops: ", deploy$Mooring, "<br>",
                     "Latitude: ", deploy$Latitude, "<br>",
                     "Longitude: ", deploy$Longitude,"<br>",
                     "Depth: ", deploy$Depth, " m", "<br>",
                     #"Operation: ", nscmp$Activity, "<br>",
                     "Project: ", deploy$Project, "<br>",
                     "Contact: ", deploy$Contact, "<br>")
                   )
  ) %>%

  # # add OTN sites
  # addCircleMarkers(data = otn,
  #                  ~Longitude, ~Latitude,
  #                  label = paste(otn$Site,":",otn$Mooring),
  #                  weight = 0.5,
  #                  col = 'black',
  #                  fillColor = "yellow",
  #                  radius = 5,
  #                  fillOpacity = 0.7,
  #                  stroke = T,
  #                  #clusterOptions = markerClusterOptions(maxClusterRadius=3, disableClusteringAtZoom=9),
  #                  group = 'Mooring Sites',
  #                  popup=(paste0(
  #                    "Station: ", otn$Site, "<br>",
  #                    "Mooring Ops: ", otn$Mooring, "<br>",
  #                    "Latitude: ", otn$Latitude, "<br>",       
  #                    "Longitude: ", otn$Longitude,"<br>",
  #                    "Depth: ", otn$Depth, " m", "<br>",
  #                    #"Operation: ", otn$Activity, "<br>",
  #                    "Project: ", otn$Project, "<br>",
  #                    "Contact: ", otn$Contact, "<br>")
  #                  )
  # ) %>%
  
  
  # add scale bar
  addScaleBar("topright",options=scaleBarOptions(maxWidth=100,imperial=F,metric=T,updateWhenIdle=T)) %>% 
  
  # addLayersControl(
  #   overlayGroups = c("MSP", "Mooring Sites", "EEZ"),
  #   options = layersControlOptions(collapsed = T)
  # ) %>% 
  
  # add legend
  leaflet::addLegend(position = "bottomright",
                     colors= c("darkred", "darkblue","yellow"),
                     labels=c("Recover","Recover & Deploy","Deploy"),
                     #title="Project",
                     title=paste("Mooring Operation (map created on ",Sys.Date(),")"),
                     opacity=0.7)

map

# # # save a stand-alone, interactive map as an html file
#library(htmlwidgets)
saveWidget(widget = map, file = 'MooringActivitiesMap-2023.html', selfcontained = T)
