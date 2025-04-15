### ** Examples

## Not run: 
##D NGLVieweR_proxy("structure") %>% updateRock(TRUE)
## End(Not run)
if (interactive()) {
  library(shiny)
  library(DT)
  
  ui = fluidPage(
    titlePanel("NGL Viewer with Features"),
    
    sidebarLayout(
      sidebarPanel(
        # Dropdown menu to select the PDB file
        selectInput("pdbFile", "Select PDB File", choices = c("A1C2p1", "A1C2p2", "A1C2p3", "A1C2p4")),
        # Checkbox to toggle spinning
        radioButtons("animate", label = "Animation",
                     choices = c("None", "Spin", "Rock"), selected = "None"),
        radioButtons("color", label = "SEQCOLTEST",
                     choices = c("None", "Present"), selected = "None"),
        checkboxInput("spin", "Enable Spin", value = TRUE),
        # Text input to enter the amino acid sequence
        textInput("sequenceInput", "Enter Amino Acid Sequence", "AAMNNNVVKKLL"),
        # Dropdown menu to select the CSV file
        selectInput("csvFile", "Select CSV File", choices = c("File1.csv", "File2.csv", "File3.csv")),
      ),
      
      mainPanel(
        NGLVieweROutput("structure"),
        # Add space for the CSV content
        div(
          style = "height: 300px; overflow-y: scroll;",
          dataTableOutput("csvTable")
        )
      )
    )
  )
  server = function(input, output, session) {
    output$structure <- renderNGLVieweR({
      selected_file <- switch(input$pdbFile,
                              "A1C2p1" = "A1C2Files/CombinedPreds/A1C2-A1C2p1.pdb",
                              "A1C2p2" = "A1C2Files/CombinedPreds/A1C2-A1C2p2.pdb",
                              "A1C2p3" = "A1C2Files/CombinedPreds/A1C2-A1C2p3.pdb",
                              "A1C2p4" = "A1C2Files/CombinedPreds/A1C2-A1C2p4.pdb"
      )
      NGLVieweR(selected_file) %>%
        addRepresentation("ribbon", param = list(sele = ":A", color = "#d3d3d3")) %>%
        addRepresentation("ribbon", param = list(sele = ":C", color = "#90ee90"))     #HEY, if you get an error saying something like "unexpected token } or ) it's because you have a %>% without anything below, that's the error

    })
    
    observeEvent(input$update, {
      NGLVieweR_proxy("structure") %>%
        updateRepresentation("ribbon",
                             param = list(
                               sele = ":A 59-77",
                               color = "red",
                               opacity = isolate(input$opacity)
                             )
        )
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
    selected_csv_data <- reactive({
      csv_file <- switch(input$csvFile,
                         "File1.csv" = "A1C2Files/CSV/A1F2Trial1.csv",  # Replace with the actual file path
                         "File2.csv" = "A1C2Files/CSV/A1F2Trial2.csv",  # Replace with the actual file path
                         "File3.csv" = "A1C2Files/CSV/A1F2Trial3.csv",
                        # Replace with the actual file path
      )
      # Read the CSV file
      csv_data <- read.csv(csv_file)
      # Display the CSV content as a DataTable
      datatable(csv_data)
    })
    output$csvTable <- renderDataTable({
      selected_csv_data()
    })
  }
  shinyApp(ui, server)
}
