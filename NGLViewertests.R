library(shiny)
library(NGLVieweR)



ui <- fluidPage(
  titlePanel("Viewer with API inputs"),
  sidebarLayout(
    sidebarPanel(
      textInput("selection", "Selection", "1-20"),
      selectInput("type", "Type", c("ball+stick", "cartoon", "backbone")),
      selectInput("color", "Color", c("orange", "grey", "white")),
      actionButton("add", "Add"),
      actionButton("remove", "Remove")
    ),
    mainPanel(
      fluidRow(
        column(4, 
               div(style = "border: 1px solid #ccc; padding: 10px;",
                   h4("Box 1"),
                   p("This is some text in Box 1.")
               )
        ),
        
        column(4, 
               div(style = "border: 1px solid #ccc; padding: 10px;",
                   h4("Box 2"),
                   p("This is some text in Box 2.")
               )
        ),
        
        column(4, 
               div(style = "border: 1px solid #ccc; padding: 10px;", 
                   h4("Box 3"),
                   NGLVieweROutput("B1"),
               )
        )
      ),
      NGLVieweROutput("structure")
    )
  )
)

#HERE'S THE DATA FOR A1
server <- function(input, output) {
  output$structure <- renderNGLVieweR({
    NGLVieweR("iGEM2023Wiki/PDBFilesA/A1.pdb") %>%
      addRepresentation("cartoon",
                        param = list(name = "cartoon", color = "#d3d3d3")
      ) %>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  observeEvent(input$add, {
    NGLVieweR_proxy("structure") %>%
      addSelection(isolate(input$type),
                   param =
                     list(
                       name = "sel1",
                       sele = isolate(input$selection),
                       colorValue = isolate(input$color)
                     )
      )
  })
  observeEvent(input$remove, {
    NGLVieweR_proxy("structure") %>%
      removeSelection("sel1")
  })
  
#HERE'S THE DATA FOR B1
  output$B1 <- renderNGLVieweR({
    NGLVieweR("iGEM2023Wiki/PDBFilesA/B1.pdb") %>%
      addRepresentation("cartoon",
                        param = list(name = "cartoon", color = "#dda0dd")
      ) %>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  observeEvent(input$add, {
    NGLVieweR_proxy("structure") %>%
      addSelection(isolate(input$type),
                   param =
                     list(
                       name = "sel1",
                       sele = isolate(input$selection),
                       colorValue = isolate(input$color)
                     )
      )
  })
  observeEvent(input$remove, {
    NGLVieweR_proxy("structure") %>%
      removeSelection("sel1")
  })
}

 output$A1BArev <- renderNGLVieweR({
    NGLVieweR("PDBFilesA/A1.pdb") %>%
      addRepresentation("cartoon",
                        param = list(name = "cartoon", color = A1BArev_residue_colors)
      ) %>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  observeEvent(input$add, {
    NGLVieweR_proxy("A1BArev") %>%
      addSelection(isolate(input$type),
                   param =
                     list(
                       name = "A1BArevsel1",
                       sele = isolate(input$selection),
                       colorValue = isolate(input$color)
                     )
      )
  })
  observeEvent(input$remove, {
    NGLVieweR_proxy("A1BArev") %>%
      removeSelection("A1BArevsel1")
  })
shinyApp(ui, server)
