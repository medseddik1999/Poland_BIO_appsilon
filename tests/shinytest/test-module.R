context("Server Testing")



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




testServer(Mappageserver,{
  session$setInputs(vernacularInput = c('Alder Leaf Beetle'))
  
  
  debounced_value <- debouncedVernacularInput()
  isolate(debounced_value <- "your_debounced_value")
  observeEvent(debounced_value, {
    selectedVernacular <- debounced_value  # Use the captured value
    scientificChoices <- scientificNames[vernacularNames %in% selectedVernacular]
    
    # Assuming updateSelectizeInput is available, adjust accordingly
    updateSelectizeInput(session, "scientificInput", selected = scientificChoices)
  })
  
  
  system.time(5)  
  
  dita=map_data()
  print(unique(dita$vernacularName))
  expect_true( unique(dita$vernacularName) == 'Agelastica alni') 
  
  
})




#test_that("observeEvent should update scientificInput", {
#  Create a Shiny test
#  testServer(ui, server, {
    # Set initial vernacularInput value
#    shinytest::setInputs("vernacularInput", 'Alder Leaf Beetle')
    
    # Check that scientificInput is updated based on the observeEvent
#    expect_equal(shinytest::getValue("your_module-scientificInput"), c("Sci2"))
#  })




test_that("Test updating scientificInput based on vernacular input", {
  testServer(Mappageserver, {
    # Set initial values
    session$setInputs(vernacularInput = c('Adonis Blue'))
  # Check the resulting value of scientificInput
    expect_equal(input$scientificInput, c('Lysandra bellargus'))
  })
})



test_dir("path_to_your_app_directory", {
  # Start the Shiny app
  app <- shinytest::ShinyDriver$new("path_to_your_app_directory")
  
  # Set initial values
  app$setInputs(vernacularInput = c('Adonis Blue'))
  
  # Trigger the debouncedVernacularInput observeEvent
  app$setValue("vernacularInput", "New Vernacular Input")
  
  # Allow time for debouncing
  app$waitForSelector("#scientificInput", timeout = 1500)
  
  # Check the resulting value of scientificInput
  expect_equal(app$getValue("scientificInput"), c('New Scientific Input'))
  
  # Navigate to the "map" tab
  app$navigateTo("map")
  
  # You can add additional interactions or assertions specific to the "map" tab here
  
  # Close the app
  app$stop()
})


test_that("automatically switches to other", {
  app <- ShinyDriver$new('app.R') 
  app$navigateTo("shiny-tab-Map")
  app$setValue("vernacularInput", "Adonis Blue") 
  app$waitForSelector("#scientificInput", timeout = 1500)
  
  # Use () to access the value of the reactive expression
  expect_equal(app$getValue("scientificInput"), c('Lysandra bellargus'))
})












