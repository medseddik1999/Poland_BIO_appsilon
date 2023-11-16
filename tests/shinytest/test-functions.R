context("Functions testing")
library(here) 
library(testthat) 







test_that("Filter by scientificNam ", {
  # Call the function to create a Leaflet map
  data1 <-names_filter('Cygnus olor' , data)
  
  # Check if the result is a Leaflet map object
  expect_true(unique(data1$scientificName) == 'Cygnus olor' ) 
  expect_true(unique(data1$vernacularName) == 'Mute Swan') 

})



test_that('leaflet Map' , {
  map=map_names(names_filter('Polygonia c-album' , data))
  expect_true("leaflet" %in% class(map)) 
}) 


test_that('timeline' , {
  line=names_timline(names_filter('Perdix perdix' , data))
  expect_true("echarts4r" %in% class(line)) 
}) 




test_that('timeline' , {
  line=names_timline(names_filter('Perdix perdix' , data))
  expect_true("echarts4r" %in% class(line)) 
}) 




test_that('clean note' , {
  list=cleanNoteList(c('pa.txt' , 'pi.txt'))
  expect_true(all(list == c('pa','pi'))) 

}) 





testServer(HomepageModuServer , {
  session$setInputs(province = 'Podlaskie Voivodeship') 
  d<-length(unique(data2()$scientificName))
  expect_true(d == 981)
  
  
})



testServer(HomepageModuServer , {
  session$setInputs(province = 'All') 
  d<-length(unique(data2()$scientificName))
  expect_true(d == 1508)  
  
  
})


testServer(HomepageModuServer , {
  session$setInputs(province = 'All') 
  d<-length(unique(data2()$scientificName))
  expect_true(d == 1508)  
  
  
}) 











