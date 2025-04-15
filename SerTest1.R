library(shiny)
library(NGLVieweR)
library(DT)


SerTest1 <- function(input, output, session) {
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
    
    # Toggle viewing the selected sequences a, b, c, BS1, and BS2,
    if (input$seqButt) {
      viewer <- NGLVieweR(selected_file)%>%
        addRepresentation("ribbon", param = list(name = "selec1a", sele = ":A", color = "#d3d3d3")) %>%
        addRepresentation("ribbon", param = list(name = "selec2b", sele = ":C", color = "#90ee90")) %>%
        addRepresentation("ribbon", param = list(name = "seleca", sele = ":A and 59-77", color = "#ff0000"))%>%
        addRepresentation("ribbon", param = list(name = "selecb", sele = ":A and 99-119", color = "#ffff00"))%>%
        addRepresentation("ribbon", param = list(name = "selecc", sele = ":A and 140-159", color = "#ffc0cb"))%>%
        addRepresentation("ribbon", param = list(name = "selecbs1", sele = ":C and 59-77", color = "#ff7f00"))%>%
        addRepresentation("ribbon", param = list(name = "selecbs2", sele = ":C and 59-77", color = "#a020f0"))
      
        }
    })

  
  # Add a reactive to load and display the selected CSV file
  selected_csv_data <- reactive({
    csv_file <- switch(input$pdbFile,
                       "A1C2p1" = "A1C2Files/CSV/A1F2Trial1.csv",  # Replace with the actual file path
                       "A1C2p2" = "A1C2Files/CSV/A1F2Trial2.csv",  # Replace with the actual file path
                       "A1C2p3" = "A1C2Files/CSV/A1F2Trial3.csv",
                       "A1C2p4" = "A1C2Files/CSV/A1F2Trial4.csv", # Replace with the actual file path 
                       "A1C2p5" = "A1C2Files/CSV/A1F2Trial5.csv",
                       )
    
    # Read the CSV file
    csv_data <- read.csv(csv_file, header = TRUE, sep = ",")
    
    # Display the CSV content as a DataTable
    datatable(csv_data)
  })
  
  # Render the selected CSV data
  output$csvTable <- renderDataTable({
    selected_csv_data()
  })
  sel_csv_head <- reactive({
    output$csv_head <- switch(input$pdbFile,
                              "A1C2p1" = "trial 1",  # Replace with the actual file path
                              "A1C2p2" = "t2",  # Replace with the actual file path
                              "A1C2p3" = "t3",
                              "A1C2p4" = "t4", # Replace with the actual file path
                              "A1C2p5" = "t5",
    )
  })
 
}

