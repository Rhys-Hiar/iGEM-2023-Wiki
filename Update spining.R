#NGLVieweR_proxy("structure") %>% updateRock(TRUE)

## End(Not run)
if (interactive()) {
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  
  ui = fluidPage(
    titlePanel("Viewer with API inputs"),
    sidebarLayout(
      sidebarPanel(
        radioButtons("animate", label = "Animation",
                     choices = c("None", "Spin", "Rock"), selected = "None"),
        # Dropdown menu to select the PDB file
        selectInput("pdbFile", "Select PDB File", choices = c("A1C2p1", "A1C2p2", "A1C2p3", "A1C2p4")),
        actionButton("colorButton", "Color Sequence"),
        textInput("color", "Update the Colours", "white"),
        actionButton("updatecolourbutton", "update the colour!"),
        #Selecting different parts of the proteins
        textInput("selection", "Selection", "1-20"),
        selectInput("type", "Type", c("ball+stick", "cartoon", "backbone")),
        actionButton("add", "Add"),
        actionButton("remove", "Remove"),
        
      ),
      
      mainPanel(
        NGLVieweROutput("structure")
      )
    )
  )
  server = function(input, output) {
    output$structure <- renderNGLVieweR({
      selected_file <- switch(input$pdbFile,
                              "A1C2p1" = "A1C2Files/CombinedPreds/A1C2-A1C2p1.pdb",
                              "A1C2p2" = "A1C2Files/CombinedPreds/A1C2-A1C2p2.pdb",
                              "A1C2p3" = "A1C2Files/CombinedPreds/A1C2-A1C2p3.pdb",
                              "A1C2p4" = "A1C2Files/CombinedPreds/A1C2-A1C2p4.pdb"
      )
      viewer <- NGLVieweR(selected_file) %>%
        addRepresentation("cartoon", param = list(name = "selec1", sele = ":A", color = "#d3d3d3")) %>%
        addRepresentation("cartoon", param = list(name = "selec2", sele = ":C", color = "#90ee90"))
      
    })
    
    observeEvent(input$animate,{
      if(input$animate == "Rock"){
        NGLVieweR_proxy("structure") %>%
          updateRock(TRUE)
      } else if(input$animate == "Spin") {
        NGLVieweR_proxy("structure") %>%
          updateSpin(TRUE)
      } else{
        NGLVieweR_proxy("structure") %>%
          updateRock(FALSE) %>%
          updateSpin(FALSE)
      }
    })
    observeEvent(input$add, {
      NGLVieweR_proxy("structure") %>%
        addSelection(isolate(input$type),
                     param =
                       list(
                         name = "sel1",
                         sele = isolate(input$selection),
                         color = isolate(input$color)
                       )
        )
    })
    
    observeEvent(input$updatecolourbutton, {
      NGLVieweR_proxy("structure") %>%
        updateColor("cartoon", isolate(paste0(input$color)))
    })
  }
  shinyApp(ui, server)
}
