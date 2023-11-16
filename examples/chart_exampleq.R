




dota=data %>% group_by(kingdom) %>% 
  summarise(number_of_spacies=n_distinct(scientificName)) 




replacement_vector <- ifelse(dota$kingdom == "", "unknown", dota$kingdom)
dota$kingdom=replacement_vector  


dota |> e_charts(kingdom) |> 
  e_pie(number_of_spacies, radius = c("50%", "70%")) |> 
  e_theme("macarons2") 


#------------------- Observation timeline----------------- 


data$eventDate=as.Date(data$eventDate) 


date_data=data %>% group_by(eventDate) %>% summarise(observations=n() , species=n_distinct(scientificName)) 


p=date_data |> 
  e_charts(eventDate) |> 
  e_line(observations) |> 
  e_line(species) |>  
  e_datazoom(type = "slider")  |> e_theme("macarons2")
 
class(p)
  
date_data|> 
  e_charts(eventDate) |> 
  e_calendar(range = "1998" ) |>
  e_visual_map(max = 220)|> 
  e_heatmap(observations, coord_system = "calendar") |> 
  e_title("Calendar", "Heatmap")

  
 #---------------- Observation map ____________- 
  
  
dori <- names_filter(c('Acrocephalus paludicola', 'Corvus cornix','Carex canescens'), data)
dori$eventDate <- as.Date(dori$eventDate)

# Agréger les données
dori1 <- dori %>%
  group_by(eventDate, scientificName) %>%
  summarise(NumberOfobservation = n(), countIndividu = sum(individualCount))

# Trier les données par date
dori <- dori %>% arrange(eventDate)

# Créer le graphique interactif avec Plotly
#-----
plot <- ggplot(dori, aes(x = eventDate, y = NumberOfobservation, color = scientificName)) +
  geom_line() +
  geom_point() + 
  scale_x_date(date_labels = "%Y-%m-%d")  +# Add points
  labs(title = "Série temporelle interactive pour Acrocephalus paludicola et Corvus cornix",
       x = "Date",
       y = "Nombre d'observations" ) +
  scale_color_manual(values = c('#e41a1c', '#377eb8'))

# Convert ggplot to plotly
plotly_plot <- ggplotly(plot) %>% layout( rangeslider = list(visible = TRUE)) 

#---------------------------

dori1 <- dori %>%
  group_by(eventDate, scientificName) %>%
  summarise(NumberOfobservation = n(), countIndividu = sum(individualCount))


dori1 |> 
  group_by(scientificName) |> 
  e_charts(eventDate, name = 'Date') |> 
  e_line(NumberOfobservation) |>
  e_tooltip(trigger = 'axis') |> 
  e_axis(
    name = 'Date'  # X-axis label
  ) |> 
  e_y_axis(name = 'Number of Observations') 



 





















