

# Define a color mapping for amino acids
if (interactive()) {
  library(shiny)
  library(NGLVieweR)
  library(DT)
ui <- fluidPage(
  titlePanel("NGL Viewer with Features"),
  sidebarLayout(
      selectInput("prediction", "Choose The Prediction", choices = c("A1C2", "A1F2")),
    sidebarPanel(
      # Dropdown menu to select the PDB file
      selectInput("pdbFile", "Select PDB File", choices = c("A1C2p1", "A1C2p2", "A1C2p3", "A1C2p4")),
      # Checkbox to toggle spinning
      checkboxInput("spin", "Enable Spin", value = TRUE),
      # Text input to enter the amino acid sequence
      textInput("sequenceInput", "Enter Amino Acid Sequence", "AAMNNNVVKKLL"),
      # Button to apply the color to the sequence
      actionButton("colorButton", "Color Sequence"),
      # Button to toggle chain :A visibility
      actionButton("toggleButton", "Toggle Chain :A"),
      # Dropdown menu to select csv files deleted added to other pdb selection input
    ),
    
    mainPanel(
      NGLVieweROutput("viewer"),
      # Add space for the CSV content
      div(
        style = "height: 300px; overflow-y: scroll;",
        dataTableOutput("csvTable")
      )
    )
  )
)


server <- function(input, output, session) {
  chain_A_visible <- reactiveVal(TRUE)
  
  output$viewer <- renderNGLVieweR({
    selected_file <- switch(input$pdbFile,
                            "A1C2p1" = "A1C2Files/CombinedPreds/A1C2-A1C2p1.pdb",
                            "A1C2p2" = "A1C2Files/CombinedPreds/A1C2-A1C2p2.pdb",
                            "A1C2p3" = "A1C2Files/CombinedPreds/A1C2-A1C2p3.pdb",
                            "A1C2p4" = "A1C2Files/CombinedPreds/A1C2-A1C2p4.pdb"
    )
    
    viewer <- NGLVieweR(selected_file) %>%
      addRepresentation("ribbon", param = list(name = "selec1", sele = ":A", color = "#d3d3d3")) %>%
      addRepresentation("ribbon", param = list(name = "selec2", sele = ":C", color = "#90ee90"))
    
    # Toggle spinning
    if (input$spin) {
      viewer <- viewer %>%
        setSpin(TRUE)
    } else {
      viewer <- viewer %>%
        setSpin(FALSE)
    }
    
    # Check for the button click and change colors of the specified amino acid sequence
    observeEvent(input$colorButton, {
      sequence <- toupper(input$sequenceInput)  # Convert input to uppercase
      color <- amino_acid_colors[[sequence]]
      
      if (!is.null(color)) {
        viewer <- viewer %>%
          colorResidue(sequence, color = color)
      }
    })
    
    # Toggle button event handler to hide/show chain :A
    observeEvent(input$toggleButton, {
      chain_A_visible(!chain_A_visible())
      if (chain_A_visible()) {
        viewer <- viewer %>%
          addRepresentation("ribbon", param = list(name = "selec1", sele = ":A", color = "#d3d3d3"))
      } else {
        viewer <- viewer %>%
          removeRepresentation("ribbon", name = "selec1")
      }
    })
    
    viewer %>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0)
  })
  
  # Add a reactive to load and display the selected CSV file
  selected_csv_data <- reactive({
    csv_file <- switch(input$pdbFile,
                       "A1C2p1" = "A1C2Files/CSV/A1F2Trial1.csv",
                       "A1C2p2" = "A1C2Files/CSV/A1F2Trial2.csv",  
                       "A1C2p3" = "A1C2Files/CSV/A1F2Trial3.csv",
                       
    )
    
    # Read the CSV file
    csv_data <- read.csv(csv_file)
    
    # Display the CSV content as a DataTable
    datatable(csv_data)
  })
  
  # Render the selected CSV data
  output$csvTable <- renderDataTable({
    selected_csv_data()
  })
}

shinyApp(ui, server)
}

