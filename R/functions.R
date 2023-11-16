library(leaflet) 
library(leaflet.providers)  
library(viridis)
library(sf) 
library(echarts4r) 
library(echarts4r.maps) 
library(shinyjs)

#my_colors <- c("#1AACAC", "#B83B5E", "#FFD099", "#706233", "#D5B4B4")
#color_palette<- colorFactor(my_colors, levels = unique(data$scientificName))




###########
poland_geojson <- sf::st_read("poland.geojson") 

generateOptions2 <- function() {
  options <- vernacularNames
  return(options)
}

# Function to generate options for the scientific input

names_filter <- function(name1,df){
  df %>% filter((scientificName %in% name1)) 
  
  
}

map_names <- function(df){ 
  my_colors <- c("#1AACAC", "#B83B5E", "#F4CE14", "#706233" , "#3A4D39")
  color_palette <- colorFactor(my_colors , levels = unique(df$scientificName))
  leaflet(df) %>% 
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%  
    addPolygons(data = poland_geojson, weight = 3, color = "grey", fill = FALSE) %>% 
    addCircleMarkers(
      lat = ~latitudeDecimal, 
      lng = ~longitudeDecimal, 
      group = ~scientificName,
      color = ~color_palette(scientificName) , 
      popup = ~paste(
        "<h3>", scientificName, "</h3>",
        "<strong>Vernacular Name:</strong> ", vernacularName, "<br>",  
        "<strong>Date:</strong>", eventDate, "<br>",
        "<strong>Kingdom:</strong>", kingdom, "<br>", 
        "<strong>Family:</strong>", family, "<br>" , 
        "<strong>Individual Count:</strong>", individualCount, "<br>", 
        "<strong>Uncertinity:</strong>", coordinateUncertaintyInMeters ,'m', "<br>"
      )
     
    ) %>%  
    addLegend(
      position = "bottomright",
      pal = color_palette,
      values = ~scientificName,
      title = "Scientific Name"
    ) %>% 
   setView(lat = 52 , lng = 20 , zoom = 5)
  
}

names_timline <- function(df){
  df <- df %>%
    group_by(eventDate, scientificName) %>%
    summarise(NumberOfobservation = n(), countIndividu = sum(individualCount))
  
  df |> 
    group_by(scientificName) |> 
    e_charts(eventDate, name = 'Date') |> 
    e_line(NumberOfobservation) |>
    e_tooltip(trigger = 'axis') |> 
    e_axis(
      name = 'Date'  # X-axis label
    ) |> 
    e_y_axis(name = 'Num of Observations') 
  
}




cleanNoteList <- function(folder) {
  notes <- list.files(path = folder, pattern = "\\.txt$")
  notes <- gsub(".txt", "", notes)
   return(notes)
}



clean_title <- function(text){
  notes <- gsub("\\.txt$", "", text) 
  notes <- gsub("[^a-zA-Z0-9]", " ", notes)
  notes <- trimws(notes)
  notes <- notes[nzchar(notes)] 
  return (text)
}



loaded_packages <- search()

# Extract package names
package_names <- loaded_packages[grepl("^package:", loaded_packages)]

# Remove "package:" prefix
package_names <- gsub("^package:", "", package_names)
package_names <- paste(package_names  ,  collapse = ' , ') 
package_names


rm(dt)
