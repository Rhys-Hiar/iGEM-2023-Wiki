library(shiny)
library(NGLVieweR)
# Define a color mapping for amino acids


ui <- fluidPage(
  titlePanel("NGL Viewer with Features"),
  sidebarLayout(
    selectInput("prediction", "select the pred", choices = c("A1C2", "A1F2")),
    sidebarPanel(
      conditionalPanel(
        condition = "input.prediction == 'A1C2'",
        # Dropdown menu to select the PDB file
        selectInput("pdbFile", "Select PDB File", choices = c("A1", "A1C2p1", "A1C2p2", "A1C2p3", "A1C2p4")),
        # Checkbox to toggle spinning
        checkboxInput("spin", "Enable Spin", value = TRUE),
        # Text input to enter the amino acid sequence
        textInput("sequenceInput", "Enter Amino Acid Sequence", "AAMNNNVVKKLL"),
        # Button to apply the color to the sequence
        actionButton("colorButton", "Color Sequence")
      ),
      conditionalPanel(
        condition = "input.prediction = 'A1F2'",
        # Dropdown menu to select the PDB file
        selectInput("pdbFile", "Select PDB File", choices = c("A1", "A1F2p1", "A1F2p2", "A1F2p3", "A1F2p4")),
        # Checkbox to toggle spinning
        checkboxInput("spin", "Enable Spin", value = TRUE),
        # Text input to enter the amino acid sequence
        textInput("sequenceInput", "Enter Amino Acid Sequence", "AAMNNNVVKKLL"),
        # Button to apply the color to the sequence
        actionButton("colorButton", "Color Sequence")
      )
    ),
    
    mainPanel(
      NGLVieweROutput("viewer")
    )
  )
)

server <- function(input, output, session) {
  output$viewer <- renderNGLVieweR({
    selected_file <- switch(input$pdbFile,
                            "A1" = "A1C2Files/Individuals/A1C2-A1.pdb",
                            "A1C2p1" = "A1C2Files/CombinedPreds/A1C2-A1C2p1.pdb",  # Replace with the actual file path
                            "A1C2p2" = "A1C2Files/CombinedPreds/A1C2-A1C2p2.pdb",  # Replace with the actual file path
                            "A1C2p3" = "A1C2Files/CombinedPreds/A1C2-A1C2p3.pdb",  # Replace with the actual file path
                            "A1C2p4" = "A1C2Files/CombinedPreds/A1C2-A1C2p4.pdb",
                            "A1F2p4" = "A1F2Files/CombinedPreds/A1F2-A1F2p1.pdb",# Replace with the actual file path
    )
    
    viewer <- NGLVieweR(selected_file) %>%
      addRepresentation("cartoon",
                        param = list(name = "cartoon1", color = "#d3d3d3") #sele = ":A"
      ) %>% 
      
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
    
    viewer %>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0)
  })
}

shinyApp(ui, server)
