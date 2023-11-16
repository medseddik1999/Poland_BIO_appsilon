# Biodiversity in Poland Shiny App

## App Description

This Shiny application comprises two main sections. The first section is the homepage, featuring three primary charts: the Poland Region Map, Species Diversity, and Observations Timeline. The second section is the Observations Map, allowing users to view observation locations on a map and add notes for specific information.

-   app.R (App run file)
-   data_prepa (Data preparation before setup)
    -   data_importation.R
    -   Provance_names.py
-   data_setup.R
-   examples. (useful examples)
    -   APP2.R (app in one block)
    -   chart_exampleq.R
    -   example.R
-   locations.csv
-   Notes_bio
    -   Mergus.txt
-   Poland_data.csv
-   poland.geojson. poland geojson for leaflet
-   R
    -   functions.R. (functions for the app)
    -   module.R (app modules will be used in app.R)
-   README.md
-   styles (UI/UX)
    -   semantic.min.css
    -   semantic.min.js
-   tests
    -   shinytest
        -   mytest.R
        -   test-functions.R
        -   test-module.R

## Data Preparation:

The data received is more than 19GB, so I used data from Poland, which is much smaller. To extract the data from this large file, I used [xsv](https://github.com/BurntSushi/xsv) to search for Poland observations.

```{r}
cmd <- 'xsv search -s countryCode "PL" /Users/midou/Desktop/Projets_Data/appsilon_project/occurence.csv'


Poland_data <- read.csv(pipe(cmd))

write.csv(Poland_data,'Poland_data.csv',row.names = FALSE ) 
```

1.  The regions data are empty.

2.  Some vernacular names are empty.

To address these issues:

1.  I used a Python API to obtain the full address from geographical coordinates (**`data_prepa/Provance_names.py`**).

2.  I replaced empty names with "unknown" followed by a unique number for each observation (**`data_setup.R`**).

## Styles and UX/UI:

I use semantic.dashboards for the interface.

and I modify the style from this file styles/semantic.min.css

to use more pre-prepared themes from [here](https://semantic-ui-forest.com/themes/)

**important** : to use the theme be sure that this is activated

```{r}
options(semantic.themes = TRUE) 
options(shiny.custom.semantic = "styles/")  
```

# Server:

## Home Page

### UI Structure (HomepageModu function in [module.R](path/to/module.R))

1.  **Title**
2.  **Value Boxes**
3.  **Input Selectors (Region and Metric)**
4.  **Charts (Map, Time Series, and Pie)**

### How it Works

The Regions input selector is connected to all charts through a reactive function. This function filters data based on the selected region and then distributes the filtered data to all charts. When the user selects 'All,' the data remains unfiltered and distributed to all charts.

```{r}
data2 <- reactive({
      if (input$province != 'All') {
        data %>% filter(polish_provinces_eng == input$province)
      } else {
        data
      }
    })
```

The metric selection is not connected to boxes but to charts. There are two types of metrics: Number of Observations and Number of Species. When the user selects a metric, the charts update accordingly.

Taking the input metric by reactive :

```{r}
Metric <- reactive({
      if (input$metric == 'Observations') {
        'observations'
      } else {
        'Species'
      }
    })

```

Example of chart updating based on the selected metric:

```{r}
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



```

Overall , The modules are presented in \*\*\*HomepageModuServer\*\*\* in the file R/module.R

## Observation Map Page:

### UI structure:

1.  Title

2.  Shear Ames Fields

3.  Time Line and Map

4.  Read and Write Notes

This section can be divided into three features.

### First Feature: Shearing and Displaying by Names

The user can select species by their scientific and vernacular names, and the observations of the selected species will be displayed on the map and in a time series related to the observation date.

**Important**: When the user selects a scientific name, the vernacular name should automatically appear in the dedicated shear box and vice versa.

**Important**: the user can select maximum 4 names at the same time.

To implement this, three steps are followed:

1.  Update shear boxes when the user selects a name. Debounce is used to avoid confusion and bugs on the user side.

```{r}
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
    



```

**Filtering Data:**

Once the user selects a specific name, the server is prepared to filter data based on the user's request. The server prioritizes filtering data using the scientific name since both scientific and vernacular names are available in all states. Filtering by just one name is implemented to optimize the feature. If no names are selected, the server defaults to a predefined value.

```{r}
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
```

Once the data is filtered can be processed in the chart functions :

```{r}
 output$map <- renderLeaflet({
      map_names(map_data())
    })
    
   
    output$sptime <- renderEcharts4r({
      names_timline(map_data())
    })
```

## Write and read Notes:

This features help the user to write and read notes.

#### Write notes:

The features of writing a note is simple the user write the title and content and once he click on submit the server will store the content in text file which have the same name as title.

the notes will be saved in [Notes_bio](Notes_bio)

the part module NoteInModuServer :

```{r}
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
```

### Read Notes:

This feature allow the user to see notes made by him or other users , the server will list the names of notes and once the user select a notes will be displayed.

This is on : module.R with names (NoteOutModuleServer)

```{r}

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
      })})

```

Useful links:

<https://echarts4r.john-coene.com/> <https://semantic-ui-forest.com/themes/> <https://github.com/BurntSushi/xsv> <https://github.com/Appsilon/semantic.dashboard>
