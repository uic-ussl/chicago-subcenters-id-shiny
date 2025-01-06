#! Set up workplace
invisible(source("www/scripts/setup.R"))

## Read context layers
layer.chicagocity <- st_read("www/shapefiles_contextlayers/Boundary_Chicago/Boundary_Chicago.shp")
layer.msacounties <- st_read("www/shapefiles_contextlayers/Boundary_MSACounties/Boundary_MSACounties.shp")
layer.states <- st_read("www/shapefiles_contextlayers/Boundary_States/Boundary_States.shp")

## Styles
style.title.panel = 'text-align:center;
                     background-color: #003366;
                     font-size: 40px;
                     margin-left: -15px;
                     margin-right: -15px;
                     padding-bottom: 15px;
                     padding-left: 15px;
                     padding-top: 10px;
                     color:#FFFFFF;'

style.subheader = 'text-align: center;
                   font-size: 20px;
                   padding-top: 10px;
                   padding-bottom: 10px;'

style.bodytext = 'text-align: center;
                  font-size: 15px;
                  padding-top: 10px;
                  padding-bottom: 10px;'

## UI
ui <- fluidPage(
  
  # Title
  titlePanel(title = div("Identifying Employment Subcenters in the Chicago Metropolitan Statistical Area",
                         style = style.title.panel)),
  
  ## Introduction and Instructions
  fluidRow(column(2),
           column(8,
                  align = "center",
                  h3("In December 2024, the Urban Spatial Structure Lab at the Univeristy of Illinois
                     Chicago completed an in-depth analysis of methods for the identification of employment
                     subcenters in U.S. metropolitan areas (MSAs), and applied said methods to the Chicago 
                     MSA. This interactive application allows users to review outputs of the application of 
                     these methods to the Chicago MSA.",
                     style = style.bodytext),
                  h3("Below, you may select any of the five methods. Depending on the method selected,
                     you may receive options to adjust function parameters. By default, these parameters are 
                     set to the values used in the article. For an explanation of how each parameter value 
                     affects the method, please refer to the information at the bottom of the page. For an 
                     in-depth explanation of each method, please refer to the article. Click the button below 
                     to open the article in a new tab.",
                  style = style.bodytext),
                  actionButton(inputId = "readMoreButton",
                               label = "Read the article",
                               onclick = "window.open('http://google.com', '_blank')"),
                  h3("Once you are satisfied with your method and parameter selections, click the 
                     'View Results' button to begin generating the map. The progress bar on the bottom right 
                     of the page will indicate whether the map has been updated. You may re-render the map 
                     as many times as you wish.",
                     style = style.bodytext)),
           column(2)),
  
  ## sidebar and map display
  fluidRow(
    ## sidebar
    column(3,
           align = 'center',
           selectInput("input.method",
                       "Select an identification method:",
                       choices = c("",
                                   "Commuting Flows",
                                   "Density Peaks",
                                   "Double Thresholds",
                                   "Positive Residuals",
                                   "Spatial Autocorrelation"),
                       multiple = FALSE),
           uiOutput('reactiveOption.contiguity'),
           uiOutput('reactiveOption.jobCutoff'),
           uiOutput('reactiveOption.nearestNeighbors'),
           uiOutput('reactiveOption.alphaLevel'),
           uiOutput('reactiveText.positiveresidualsWarning', style = style.bodytext),
           actionButton('input.goButton', 'View Results')
    ),
    ## map display
    column(6,
           align = 'center',
           leafletOutput("output.resultsMap", height = "65vh", width = "50vw"))
  ),
  
  ## Parameter explanations
  fluidRow(
    column(2),
    column(8,
           align = "center",
           ## Contiguity
           h3("Contiguity Criteria and Dissolving Neighbors",
              style = style.subheader),
           h3("Lorem ipsum dolor",
              style = style.bodytext))
  )
  
)


## Server
server <- function(input, output) {
  
  # Reactive UI elements
  ## Contiguity
  output$reactiveOption.contiguity <- renderUI({
    if (input$input.method %in% c("Commuting Flows", "Double Thresholds")) {
      checkboxInput("reactiveOption.contiguity",
                    "Apply contiguity and dissolve neighbors?",
                    value = TRUE)
    }
  })
  ## jobCutoff
  output$reactiveOption.jobCutoff <- renderUI({
    if (input$input.method == "Positive Residuals") {
      sliderInput("reactiveOption.jobCutoff",
                  "Cutoff for total jobs within a cluster:",
                  min = 0,
                  max = 20000,
                  value = 10000,
                  step = 1000)
    }
  })
  ## alphaLevel
  output$reactiveOption.alphaLevel <- renderUI({
    if (input$input.method %in% c("Positive Residuals", "Spatial Autocorrelation")) {
      selectInput("reactiveOption.alphaLevel",
                  "Alpha Level:",
                  choices = c("0.01",
                              "0.05",
                              "0.10"),
                  selected = "0.05",
                  multiple = FALSE)
    }
  })
  ## nearestNeighbors
  output$reactiveOption.nearestNeighbors <- renderUI({
    if (input$input.method == "Spatial Autocorrelation") {
      sliderInput("reactiveOption.nearestNeighbors",
                  "K for K-nearest-neighbor estimation:",
                  min = 0,
                  max = 10,
                  value = 7,
                  step = 1)
    }
  })
  
  ## Positive residuals time warning
  output$reactiveText.positiveresidualsWarning <- renderUI({
    if (input$input.method == "Positive Residuals") {
      renderText("Please note that this method will take up to 10 minutes to run.")
    }
  })
  
  # On goButton press
  observeEvent(input$input.goButton, {
    
    if (input$input.method == "Density Peaks") {
      
      withProgress(message = 'Progress indicators', {
      layers <- prepareShapefile(input$input.method,
                                 input$reactiveOption.contiguity,
                                 input$reactiveOption.jobCutoff,
                                 input$reactiveOption.nearestNeighbors,
                                 input$reactiveOption.alphaLevel)
      })
      
      layer.toMap <- layers[[1]]
      rings.toMap <- layers[[2]]
      
      output$output.resultsMap <- renderLeaflet({
        mapmaker(layer.toMap, rings.toMap)
      })
      
    } else {
      withProgress(message = "Progress indicators", {
      layer.toMap <- prepareShapefile(input$input.method,
                                      input$reactiveOption.contiguity,
                                      input$reactiveOption.jobCutoff,
                                      input$reactiveOption.nearestNeighbors,
                                      input$reactiveOption.alphaLevel)
      })
      
      output$output.resultsMap <- renderLeaflet({
        mapmaker(layer.toMap)
      })
    }
    
    
    
  })
  
}

## Call app
shinyApp(ui, server)