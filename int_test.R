library(shiny)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),
  # Create a reactive variable to track which image is clicked
  reactiveVal("image1"),
  # Define the UI components
  uiOutput("image_click"),
  uiOutput("output")
)

server <- function(input, output, session) {
  # Define an observer to handle image clicks
  observeEvent(input$image_click, {
    clicked_image <- input$image_click
    
    # Check which image was clicked and update the output accordingly
    if (clicked_image == "image1") {
      output$output <- renderText({
        "You clicked Image 1. Display a different Shiny script here for Image 1."
      })
    } else if (clicked_image == "image2") {
      output$output <- renderText({
        "You clicked Image 2. Display a different Shiny script here for Image 2."
      })
    }
  })
  
  # Render the images and make them clickable
  output$image_click <- renderUI({
    div(
      img(
        src = "/thumbnail1.jpg", 
        id = "image1", 
        height = "300px", 
        width = "300px"
      ),
      img(
        src = "/thumbnail2.jpg", 
        id = "image2", 
        height = "300px", 
        width = "300px"
      )
    )
  })
}

shinyApp(ui, server)
