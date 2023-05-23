require(shiny)
require(shinyWidgets)
require(shinydashboard)
require(tidyverse)
require(lubridate)
require(magrittr)
require(ggalluvial)
require(highcharter)
require(tmap)
require(sf)
require(tidyterra)
require(terra)
require(rerddap)
require(DT)
require(wior)


#theme = “cerulean”, “cosmo”, “cyborg”, “darkly”, “flatly”, “journal”, 
# “litera”, “lumen”, “lux”, “materia”, “minty”, “morph”, “pulse”, 
# “quartz”, “sandstone”, “simplex”, “sketchy”, “slate”, “solar”, 
# “spacelab”, “superhero”, “united”, “vapor”, “yeti”, “zephyr”


ui = navbarPage(
  title = "Wior",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  tabPanel(
    title = "Data Extraction",
    fluidRow(
      column(
        width = 1
      ),
      column(
        width = 2, 
        tags$text("Pick a variable of interest"),
        pickerInput(inputId = "variable_id", 
                    choices = c("chlModis", "sstModis",  "ppMODIS", "precip"), 
                    selected = "chlModis", multiple = F),
        tags$hr(),
        numericInput(inputId = "lon_min",label = "Enter a minimum longitude", value = 37, min = -180, max = 180),
        tags$hr(),
        numericInput(inputId = "lon_max", label = "Enter a maximum longitude", value = 42,min = -180, max = 180),
        tags$hr(),
        numericInput(inputId = "lat_min",label = "Enter a minimum latitude", value = -11, min = -90, max = 90),
        tags$hr(),
        numericInput(inputId = "lat_max", label = "Enter a maximum latitude", value = -4,min = -90, max = 90),
        tags$hr(),
        sliderInput(inputId = "level_id", label = "Select level, 1=daily, 2=8-days, 3=monthly composite", value = 3, min = 1, max = 3,step = 1),
        tags$hr(),
        dateRangeInput(inputId = "date_id",label = "Select a time interval (start & end)", start = "2007-01-01", end = "2008-12-31", 
                      min = dmy(01011997), max = Sys.Date()-1, startview = "year"),
        actionButton(inputId = "reset", label = "Reset")
        
        
      ),
      column(
        width = 6,
        DT::dataTableOutput(outputId = "data") %>% shinycustomloader::withLoader(type = "html", loader = "loader1"),
        tags$hr(),
        downloadButton(outputId = "download",label = "Download")
        
      )
    )
    
  )
)
  
  
  

  
server = function(input, output, session){
  
  aa = reactive({
    
    case_when(
      input$variable_id == "chlModis" ~ wior::get_chlModis(lon.min = input$lon_min,
                                                           lon.max = input$lon_max,
                                                           lat.min = input$lat_min,
                                                           lat.max = input$lat_max,
                                                           t1 = input$date_id,
                                                           t2 = input$date_id,
                                                           level = input$level_id),
      
      input$variable_id == "sstModis" ~ wior::get_sstMODIS(lon.min = input$lon_min,
                                                           lon.max = input$lon_max,
                                                           lat.min = input$lat_min,
                                                           lat.max = input$lat_max,
                                                           t1 = input$date_id,
                                                           t2 = input$date_id,
                                                           level = input$level_id),
      
      input$variable_id == "ppMODIS" ~ wior::get_ppMODIS(lon.min = input$lon_min,
                                                         lon.max = input$lon_max,
                                                         lat.min = input$lat_min,
                                                         lat.max = input$lat_max,
                                                         t1 = input$date_id,
                                                         t2 = input$date_id,
                                                         level = input$level_id)
      
    )
    
  })

  
  output$data = DT::renderDataTable({
    aa() %>% 
      janitor::remove_empty(which = "cols") %>% 
      mutate_if(is.numeric, round, 4)
  })
  output$download = downloadHandler(
    filename = function(){
      paste(input$variable_id, ".csv", sep = "")
    },
    content = function(file){
      write_csv(aa(), file)
    }
  )

}


# Run the application 
shinyApp(ui = ui, server = server)
