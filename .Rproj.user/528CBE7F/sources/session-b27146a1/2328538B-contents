library(dplyr) 
library(stringr)  
library(shiny.semantic)
library(semantic.dashboard) 
library(fontawesome)
library(dplyr)  
library(fs)

data=read.csv('Poland_data.csv') 
adresse=read.csv('locations.csv')

names(adresse)
data=left_join(data , adresse , by=c("latitudeDecimal" ,"longitudeDecimal"))



polish_provinces <- tolower(c("Dolnośląskie", "Kujawsko-Pomorskie", "Lubelskie", "Lubuskie", 
                                    "Łódzkie", "Małopolskie", "Mazowieckie", "Opolskie", 
                                    "Podkarpackie", "Podlaskie", "Pomorskie", "Śląskie", 
                                    "Świętokrzyskie", "Warmińsko-Mazurskie", "Wielkopolskie", 
                                    "Zachodniopomorskie"))




pattern <- paste(polish_provinces , collapse = "|")
result <- str_extract(data$adresse, pattern)
na_count <- sum(is.na(result)) 
print(na_count)


data$stateProvince=result  

provinces_df <- data.frame(
  stateProvince = polish_provinces,
  polish_provinces_eng = c("Lower Silesian Voivodeship", "Kuyavian-Pomeranian Voivodeship", "Lublin Voivodeship", 
                           "Lubusz Voivodeship", 
                           "Łódź Voivodeship", "Lesser Poland Voivodeship", "Masovian Voivodeship", "Opole Voivodeship", 
                           "Subcarpathian Voivodeship", "Podlaskie Voivodeship", "Pomeranian Voivodeship", 
                           "Silesian Voivodeship", 
                           "Świętokrzyskie Voivodeship", "Warmian-Masurian Voivodeship", "Greater Poland Voivodeship", 
                           "West Pomeranian Voivodeship")
)


data = left_join(data, provinces_df , by='stateProvince')  



###################---------
replacement_vector <- ifelse(data$kingdom == "", "unknown", data$kingdom) 
data$kingdom <- replacement_vector 

replacement_vector2 <- ifelse(data$vernacularName == "", "unknown", data$vernacularName)  
data$vernacularName <- replacement_vector2 



dt=data %>% select(scientificName , vernacularName)
dt=unique(dt) 

n=1
for(i in 1:length(dt$vernacularName)){
  if(dt$vernacularName[i] == "unknown"){
    
    dt$vernacularName[i]=paste("unknown" , n , collapse = "_")
    n=n+1
    
  }
  
}

vernacularNames <- dt$vernacularName
scientificNames <- dt$scientificName 

options(semantic.themes = TRUE) 
options(shiny.custom.semantic = "styles/")   





rm(replacement_vector)
rm(replacement_vector2)
rm(adresse)
rm(polish_provinces)
















