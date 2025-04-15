library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(), # Initialize shinyjs
  titlePanel("Local Image Viewer"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Select Image", accept = c(".png", ".jpg", ".jpeg", ".gif"))
    ),
    mainPanel(
      imageOutput("image")
    )
  )
)

server <- function(input, output, session) {
  observe({
    if (!is.null(input$file)) {
      shinyjs::enable("image") # Enable the imageOutput
      
      output$image <- renderImage({
        list(src = input$file$datapath, width = "100%") # Adjust width as needed
      }, deleteFile = FALSE)
    } else {
      shinyjs::disable("image") # Disable the imageOutput if no file is selected
    }
  })
}

shinyApp(ui, server)
