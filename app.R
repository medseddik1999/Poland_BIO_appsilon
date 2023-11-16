source('data_setup.R') 
source('R/functions.R') 
source('R/module.R')







#options(semantic.themes = TRUE) 
options(shiny.custom.semantic = "styles/")  
#options(shiny.custom.semantic = NULL)  



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
      tabItem('Overview' , HomepageModu('home')
              
              
      ) ,  
      
      tabItem("Map",MapPageUi('Map'))
      
      
      
    ) 
    
    
  ))  




server <- function(input, output , session) {
  # Server logic goes here

  HomepageModuServer("home") 
  Mappageserver('Map')
  
}






shinyApp(ui, server) 