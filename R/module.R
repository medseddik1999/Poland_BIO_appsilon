

##### UI Module--------------

HomepageModu <- function(id) {
  tagList(
    fluidRow(
      column(
        12,
        box(
          h1('Home Page',
             style = 'margin-left: 5px;
                    font-size: 35px;
                    font-weight: 700;
                    text-align: center;
                    margin-top: -20px;'
          ),
          id = NS(id, 'titlebox'),
          style = 'margin-top: 0.6em; width: 1130px;'
        )
      )
    ),
    fluidRow(
      class = 'infoboxes',
      style = 'margin-left: 100px; margin-top: -25px;',
      infoBoxOutput(NS(id, "info_box1")),
      infoBoxOutput(NS(id, "info_box2")),
      infoBoxOutput(NS(id, "info_box3"))
    ),
    fluidRow(
      column(
        4,
        selectInput(
          NS(id, 'province'),
          label = 'Province',
          choices = c('All', na.omit(unique(data$polish_provinces_eng)))
        ),
        style = 'margin-left: 300px;'
      ),
      column(
        4,
        selectInput(
          NS(id, 'metric'),
          label = 'Metric Number of:',
          choices = c('Observations', 'Species')
        )
      )
    ),
    fluidRow(
      column(
        8,
        box(
          echarts4rOutput(NS(id, 'piechart'), height = "260px"),
          title = 'Biodiversity regarding species kingdom',
          collapsible = FALSE
        )
      ),
      column(
        8,
        box(
          echarts4rOutput(NS(id, 'poland_map'), height = "260px", width = "95%"),
          collapsible = FALSE,
          title = 'Biodiversity in Polish regions'
        )
      )
    ),
    fluidRow(
      column(
        12,
        box(
          echarts4rOutput(NS(id, 'timeline'), height = "260px"),
          collapsible = FALSE,
          title = 'Observations Timeline'
        ),
        style = 'margin-top: -20px'
      )
    )
  )
}

MapPageUi <- function(id) {
  
  
  tagList(
    fluidRow(
      column(12,
             box(
               h1('Observations Map',
                  style = 'margin-left: 5px;
                     font-size: 35px;
                     font-weight: 700;
                     text-align: center;
                     margin-top: -20px;'
               )
             )
      )
    ),
    fluidRow(
      column(4,
             selectizeInput(NS(id,"vernacularInput"), "Search By Vernacular Name", choices = NULL,
                            multiple = TRUE, options = list(maxItems = 4 )
             )
      ),
      column(4,
             selectizeInput(NS(id,"scientificInput"), "Search By Scientific Name", choices = NULL,
                            multiple = TRUE, options = list(maxItems = 4)
             )
      ),
      column(6,
             message_box(class = "info", header = 'Info',
                         content = 'You can select a maximum of 4 species at the same time'
             )
      )
    ),
    fluidRow(
      column(8,
             box(
               h3('Species Observations Timeline'),
               echarts4rOutput(NS(id,'sptime')),
               br(),
               NoteOutModule(NS(id,'note_out'))
             )
      ),
      column(8,
             box(
               h3('Poland Map'),
               leafletOutput(NS(id,"map")),
               br(),
               NoteInModu(NS(id,'note_in'))
             )
      )
    )
  )
}


NoteOutModule <- function(id) {
  ns <- NS(id)
  tagList(
    box(h3('See a Note'),
        selectizeInput(ns('notesSel'), 'Select Notes:', choices = NULL),
        br(),
        textOutput(ns('notes_sel'))
    )
  )
}

  
NoteInModu<-function(id){
  box( 
      h3("Add a Note"), 
      textInput( NS(id,"note_title"), "Note Title"),br() , 
      textAreaInput(NS(id,"note_content"), "Note Content"), br(),
      actionButton( NS( id,"save_note"), "Save Note"), 
      textOutput(NS(id,'notif'))
  ) 
}  


##### Server Module--------------
HomepageModuServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    data2 <- reactive({
      if (input$province != 'All') {
        data %>% filter(polish_provinces_eng == input$province)
      } else {
        data
      }
    })
    
    Metric <- reactive({
      if (input$metric == 'Observations') {
        'observations'
      } else {
        'Species'
      }
    })
    
    output$info_box1 <- renderInfoBox({
      valueBox(
        value = nrow(data2()),
        subtitle = 'Total Observations',
        icon = fa("clipboard"),
        color = "green",
        width = 1
      )
    })
    
    output$info_box2 <- renderInfoBox({
      valueBox(
        value = n_distinct(data2()$scientificName),
        subtitle = 'Total Species',
        icon = fa("paw"),
        color = "green",
        width = 1
      )
    })
    
    output$info_box3 <- renderInfoBox({
      valueBox(
        value = round(nrow(data2()) / length(unique(data2()$scientificName)), 2),
        subtitle = 'Observation per species',
        icon = fa("eye", fill = '#776B5D'),
        color = "green",
        width = 1
      )
    })
    
    output$poland_map <- renderEcharts4r({
      dita <- data2() %>%
        group_by(polish_provinces_eng) %>%
        summarise(observations = n(), Species = n_distinct(scientificName))
      
      dita$Metric <- dita[[Metric()]]
      
      dita |>
        e_charts(polish_provinces_eng) |>
        em_map("Poland") |>
        e_map_3d(Metric, map = "Poland") |>
        e_visual_map(Metric) |>
        e_theme("macarons2")
    })
    
    output$piechart <- renderEcharts4r({
      dota <- data2() %>%
        group_by(kingdom) %>%
        summarise(Species = n_distinct(scientificName), observations = n())
      
      dota$Metric <- dota[[Metric()]]
      
      dota |>
        e_charts(kingdom) |>
        e_pie(Metric, radius = c("50%", "70%")) |>
        e_legend(show = FALSE) |>
        e_theme("macarons2")
    })
    
    output$timeline <- renderEcharts4r({
      date_data <- data2() %>%
        group_by(eventDate) %>%
        summarise(observations = n(), Species = n_distinct(scientificName))
      
      date_data$Metric <- date_data[[Metric()]]
      
      date_data |>
        e_charts(eventDate) |>
        e_line(Metric) |>
        e_datazoom(type = "slider") |>
        e_theme("macarons2")
    })
  })
}


NoteInModuServer <- function(id){
  moduleServer(id ,function(input, output , session){
    observeEvent(input$save_note, {
      
      title <- input$note_title
      content <- input$note_content
      
      # Save the note 
      note_filename <-  paste0("Notes_bio/",clean_title(title) ,".txt")
      writeLines(content, note_filename)
      
      # clear the input 
      updateTextInput(session, "note_title", value = "")
      updateTextAreaInput(session, "note_content", value = "") 
      
      
      output$notif <- renderText({
        "Saved, your note will be available in the next session" 
       
      })
      
      
    }) 
    
  }
)}
  

NoteOutModuleServer <- function(id , sessioon) {
  moduleServer(id , function(input, output , session){
    files <- reactive({
      cleanNoteList('Notes_bio')
    })
    
    # Update choices in the selectizeInput
    observe({
      updateSelectizeInput(session, 'notesSel', choices = files(), server = TRUE)
    })
    
    # Render the selected note content
    observeEvent(input$notesSel, {
      output$notes_sel <- renderText({
        selected_note <- input$notesSel
        
        # Check if a note is selected
        if (!is.null(selected_note)) {
          note_content <- readLines(paste0("Notes_bio/", selected_note, ".txt"))
          paste(note_content, collapse = '\n')
        } else {
          # If no note is selected, display a default message or handle it as needed
          "Select a note to see its content."
        }
      })
    })
  })
  
  # Reactive expression for reading note files
  
}


Mappageserver <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
  
    
    # Observe event to update selectize inputs scientificNames
    observe({
      updateSelectizeInput(session, "vernacularInput", choices = vernacularNames , server = TRUE)
      updateSelectizeInput(session, "scientificInput", choices = scientificNames, server = TRUE)
    })
    
    # Debounced input for vernacular input
    debouncedVernacularInput <- debounce(reactive(input$vernacularInput), 1200)
    
    # Observe event for updating scientific input based on vernacular input
    observeEvent(debouncedVernacularInput(), {
      selectedVernacular <- debouncedVernacularInput()
      scientificChoices <- scientificNames[vernacularNames %in% selectedVernacular]
      updateSelectizeInput(session, "scientificInput", selected = scientificChoices)
    })
    
    # Debounced input for scientific input
    debouncedScientificInput <- debounce(reactive(input$scientificInput), 1200)
    
    # Observe event for updating vernacular input based on scientific input
    observeEvent(debouncedScientificInput(), {
      selectedScientific <- debouncedScientificInput()
      vernacularChoices <- vernacularNames[scientificNames %in% selectedScientific]
      updateSelectizeInput(session, "vernacularInput", selected = vernacularChoices)
    })
    
    # Debounced map data
    map_data <- debounce(
      reactive(
        if (!is.null(input$scientificInput)) {
          names_filter(input$scientificInput, data)
        } else {
          names_filter(c('Acrocephalus paludicola', 'Propylea quatuordecimpunctata'), data)
        }
      ),
      30
    )
    
    # Render the leaflet map
    output$map <- renderLeaflet({
      map_names(map_data())
    })
    
   
    output$sptime <- renderEcharts4r({
      names_timline(map_data())
    })
    
   
    NoteInModuServer('note_in')
    NoteOutModuleServer('note_out')
  })
}
























