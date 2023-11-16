library(shiny.semantic)
library(semantic.dashboard) 
library(fontawesome)
library(dplyr)  
library(fs)


options(semantic.themes = TRUE) 
options(shiny.custom.semantic = "styles/")  
#options(shiny.custom.semantic = NULL)  

#[dt$vernacularName != 'unknown'] 





ui <- dashboardPage(  
  margin = FALSE , 
  dashboardHeader(disable = TRUE,
                  title = h2("Data Science Salaries") , 
                  
  ),
  dashboardSidebar(
    side = "left",
    size = "",
    sidebarMenu(fluidRow(class='title row',
                         'Poland Biodiversity' , style='
         text-align: center;
    font-weight: 800;
    font-size: 23px;
    color: #104e66;
    margin-right: 20px;
    margin-top: 20px;
    margin-bottom: 20px;
      '
                         
                         
    ),
    menuItem(tabName = "Overview", text = "Home Page",  icon = fa("home", margin_right='12px') ,
             selected = TRUE) , 
    menuItem(tabName = "Map", text = " Observations Map", icon = fa("map" , margin_right='12px'))
    
    )
  ),
  dashboardBody(
    tabItems(
      tabItem('Overview' ,fluidRow(column(12 ,box(h1('Home Page', 
                                                     style='margin-left: 5px;
    font-size: 35px;
    font-weight: 700;
    text-align: center;
    margin-top: -20px;'
      ) , id='titlebox'), 
      style='margin-top: 0.6em; 
                                                 width: 1130px;') ), 
      fluidRow(class='infoboxes', style='margin-left: 100px;
                                          margin-top: -25px;',
               infoBoxOutput("info_box1"),
               infoBoxOutput("info_box2"),
               infoBoxOutput("info_box3") 
      ) ,  
      fluidRow(column(4, selectInput('province' , label = 'Province' , choices = c('All',na.omit(unique(data$polish_provinces_eng)))),
                      style='margin-left: 300px;'    
      ),
      column(4 , selectInput('metric' , label= 'Metric Number of:' , choices=c('Observations', 'Species')))),
      
      fluidRow(column(8, box(echarts4rOutput('piechart',height = "260px") , title = 'Biodiversity regarding species kingdom' , collapsible = FALSE),
      ),
      column(8,box(echarts4rOutput('poland_map' ,height = "260px" , width = "95%" ), 
                   collapsible = FALSE , title = 'Biodiversity in Polish regions' ))) , 
      
      fluidRow(column(12,box(echarts4rOutput('timeline' ,height = "260px" ) , 
                             collapsible = FALSE , title = 'Observations Timeline')), style='margin-top: -20px') 
      
      
      
      ) ,  
      
      tabItem("Map", fluidRow(column(12 ,box(h1('observations Map', 
                                                style='margin-left: 5px;
                                                font-size: 35px;
                                                font-weight: 700;
                                                text-align: center;
                                                margin-top: -20px;'
      )))) , 
      fluidRow(column(4,
                      selectizeInput("vernacularInput", "Search By Vernacular Name", choices = NULL , 
                                     multiple = TRUE , options = list(maxItems = 4 ))) ,column(4,
                                                                                               selectizeInput("scientificInput", "Search By Scientific Name", choices =NULL, 
                                                                                                              multiple = TRUE , options = list(maxItems = 4))) , 
               column(6 , message_box(class = "info" , header = 'Info' , 
                                      content = 'You can select maximum 4 species at the same time'))
      ) , 
      
      
      fluidRow(column(8, box(h3('Species observations Timeline') , echarts4rOutput('sptime')) , br() , 
                      box(h3('See a Note') ,selectInput('notesSel' , 'Select Notes:' , 
                                                        choices = cleanNoteList('Notes_bio')
                      ) , br() , textOutput('notes_sel'))
                      
      ) ,column(8 , box(h3('Poland Map') , leafletOutput("map")) , br(), box( 
        h3("Add a Note"), 
        textInput("note_title", "Note Title"),br() , 
        textAreaInput("note_content", "Note Content"), br(),
        actionButton("save_note", "Save Note")) 
      )))
      
      
      
    ) 
    
    
  ))  




server <- function(input, output , session) {
  # Server logic goes here
  
  
  #####. Analytics --------------------------
  
  data2<-reactive({
    if(input$province != 'All'){
      data %>% filter(polish_provinces_eng== input$province)}
    else{
      data
    }
    
  })
  
  Metric<-reactive({
    if(input$metric =='Observations'){
      'observations'
    }else{
      'Species'
    }
  }) 
  
  
  
  output$info_box1 <- renderInfoBox({
    valueBox(
      value = nrow(data2()), 
      subtitle = 'Total Observations',
      icon = fa("clipboard"),
      color = "green",
      width = 1)
  }) 
  
  
  output$info_box2 <- renderInfoBox({
    valueBox(
      value = n_distinct(data2()$scientificName), 
      subtitle = 'Total Species',
      icon = fa("paw"),
      color = "green",
      width = 1)
  })
  
  
  
  
  output$info_box3 <- renderInfoBox({
    
    valueBox(
      value = round(nrow(data2())/ length(unique(data2()$scientificName)) , 2), 
      subtitle = 'Observation per species',
      icon = fa("eye" , fill = '#776B5D'),
      color = "green",
      width = 1)
  }) 
  
  
  
  
  
  
  
  
  
  output$poland_map<-renderEcharts4r({
    dita=data2() %>% group_by( polish_provinces_eng ) %>% summarise(observations=n() , 
                                                                    Species=n_distinct(scientificName))
    
    dita$Metric=dita[[Metric()]]
    
    dita |> 
      e_charts(polish_provinces_eng ) |>
      em_map("Poland") |> 
      e_map_3d(Metric, map = "Poland") |> 
      e_visual_map(Metric) |> 
      e_theme("macarons2") 
  })
  
  
  
  output$piechart<-renderEcharts4r({
    dota=data2() %>% group_by(kingdom) %>% 
      summarise(Species=n_distinct(scientificName) , observations=n()) 
    
    dota$Metric=dota[[Metric()]]
    
    dota |> e_charts(kingdom) |> 
      e_pie(Metric, radius = c("50%", "70%")) |> 
      e_legend(show = FALSE) |> 
      e_theme("macarons2") 
    
    
  })
  
  
  output$timeline<-renderEcharts4r({
    date_data=data2() %>% group_by(eventDate) %>% summarise(observations=n() , Species=n_distinct(scientificName)) 
    date_data$Metric=date_data[[Metric()]]  
    
    date_data |> 
      e_charts(eventDate) |> 
      e_line(Metric) |> 
      e_datazoom(type = "slider")  |> e_theme("macarons2")
    
  })
  
  
  
  
  
  
  
  
  
  ########## Observations map ----------- 
  
  generateOptions1 <- function() {
    
    options <-scientificNames
    return(options)
  }
  
  
  generateOptions2 <- function() {
    
    options <- vernacularNames
    return(options)
  }
  
  observe({
    updateSelectizeInput(session, "vernacularInput", choices = generateOptions2() , server = TRUE)
    updateSelectizeInput(session, "scientificInput", choices = generateOptions1() , server = TRUE)
  })
  
  
  
  
  debouncedVernacularInput <- debounce(reactive(input$vernacularInput), 1200)
  
  
  observeEvent(debouncedVernacularInput(), {
    
    selectedVernacular <- debouncedVernacularInput()
    scientificChoices <- scientificNames[vernacularNames %in% selectedVernacular]
    updateSelectizeInput(session, "scientificInput", selected = scientificChoices)
  })
  
  
  debouncedScientificInput <- debounce(reactive(input$scientificInput), 1200)
  
  
  observeEvent(debouncedScientificInput(), {
    # Update the choices of the vernacular input based on the selected scientific input
    selectedScientific <- debouncedScientificInput()
    vernacularChoices <- vernacularNames[scientificNames %in% selectedScientific]
    updateSelectizeInput(session, "vernacularInput", selected = vernacularChoices)
  })
  
  
  
  map_data<-debounce(
    reactive(
      if (!is.null(input$scientificInput)){
        names_filter(input$scientificInput  ,data)}
      else{
        names_filter(c('Acrocephalus paludicola' , 'Propylea quatuordecimpunctata')  ,data)
      }) , 30) 
  
  
  
  output$map<- renderLeaflet({
    map_names(map_data()) 
    
    
    
    
  })
  
  
  output$sptime <-renderEcharts4r(
    
    names_timline(map_data())
    
  )
  
  
  observeEvent(input$save_note, {
    # Get the values from the input fields
    title <- input$note_title
    content <- input$note_content
    
    # Save the note to a text file in the notes folder
    note_filename <-  paste0("Notes_bio/",clean_title(title) ,".txt")
    writeLines(content, note_filename)
    
    # Clear the input fields after saving
    updateTextInput(session, "note_title", value = "")
    updateTextAreaInput(session, "note_content", value = "") 
    
  })
  
  
  output$notes_sel<-renderText({
    selected_note <- input$notesSel
    note_content <- readLines(paste0("Notes_bio/",selected_note, ".txt"))
    note_content
    
  }
  
  )  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
}




shinyApp(ui, server)