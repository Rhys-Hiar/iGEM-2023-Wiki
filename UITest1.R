library(shiny)
library(NGLVieweR)
library(DT)

UITest1 <- fluidPage(
  titlePanel("NGL Viewer with Features"),
  
  sidebarLayout(
    sidebarPanel(
      # Dropdown menu to select the PDB file
      selectInput("pdbFile", "Select PDB File", choices = c("A1C2p1", "A1C2p2", "A1C2p3", "A1C2p4", "A1C2p5")),
      # Checkbox to toggle spinning
      radioButtons("animate", label = "Animation",
                   choices = c("None", "Spin", "Rock"), selected = "None"),
      checkboxInput("spin", "Enable Spin", value = TRUE),
      # Text input to enter the amino acid sequence
      textInput("sequenceInput", "Enter Amino Acid Sequence", "AAMNNNVVKKLL"),
      # Button to toggle chain :A visibility
      actionButton("toggleButton", "Toggle Chain :A"),
      # Button to toggle a, b, c, BS1, and BS2
      checkboxInput("seqButt", "Toggle sequences"), 
    ),
    
    mainPanel(
      NGLVieweROutput("viewer"),
      # Add space for the CSV content
      div(
        style = "height: 300px; overflow-y: scroll;",
        h1("sel_csv_head"),
        dataTableOutput("csvTable")
      ),
    )
  )
)

