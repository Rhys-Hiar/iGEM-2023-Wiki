
library(shiny)
library(htmltools)
library(NGLVieweR)
library(shinyWidgets)
library(data.table)
library(DT)
#html_contents <- readLines("HTMLTESTFORWIKI.html")

ui <- fluidPage(
  column(12, align="center", titlePanel("U of L Drylab!")),
  mainPanel(
    setBackgroundColor("#f3fbf4ff"),
    #f3fbf4ff is the colour hex code for the website
    tabsetPanel(type = "tabs",
                tabPanel("Introduction",
                         #Here's the intro info: 
                         p("Hi there! Welcome to the Drylab portion of the Unitversity of Lethbridge's wiki. Here we'll show you the approach that we took to implementing modeling into the Clubsquared project! You'll be able to see every protein we modeled and even interact with them! 

You'll also be able to learn how we ran these simulations with a guided turorial for you outlining our project at the end! 

But first, let's explain our approach... 
", align = "center"),
                         
                         #Here's the first part of the body
                         p("So, for our detection system, we had two proteins of interest that we would want to have a designed protein to bind to: 
- PbEL04 
- PRO1 
<INSERT PBEL 04 and PRO1>", align = "center"),
                         #INSERTING PBEL04 AND PRO1 FROM NGL VIEWER
                         div(
                           div(style = "border: 1px solid #ccc; padding: 10px;",
                               h1("A1 Location w/ binding sites"),
                               NGLVieweROutput("A1")),
                           div(style = "border: 1px solid #ccc; padding: 10px;",
                               h1("B1 Location w/ binding sites"),
                               NGLVieweROutput("B1")),
                         ),
                         #Introduction to epitope sequences
                         p("Both of these proteins were, according to our literature reiew, present in the early stages of expression in clubroot. For both of these proteins our wetlab members discovered epitope sequneces with large potentials as binding sites:
PbEL's binding sites
<DISPLAY BIDNING SITES FOR PBEL> 
PRO1s binding sites
<DISPLAY BINDING SITES FOR PRO>"),
                         
                         div(
                           div(style = "border: 1px solid #ccc; padding: 10px;",
                               h1("A1 Location"),
                               NGLVieweROutput("PbEpi")),
                         ),
                         p("For each of these potential binding sites, our team then created complimentary opposing charged amino acid sequences meant to bind to each of those binding sites. These binding sequences, which we'll abbrevaite to BS's, were then added onto a mouse antibody backbone and we generated a few different kinds of chimeric proteins: 
- PbEL04 specific proteins (chimeric proteins that'll only bind to PbEL04)
- PRO1 specific proteins (chimeric proteins that'll only bind to PRO1)
- Bispecific proteins (chimeric proteins that'll bidn to both PbEL04 and PRO1)"),
                         p("Each of these proteins will have either 
- One or two PbEL BS's for PbEL04 specific proteins
- One or two PRO1 BS's for PRO1 specific proteins
- A combination of PbEL and PRO1 BS's for bispecific proteins"),
                         p("So Let's see these binding seqences!
<INSERT BS1> <INSERT BS2> <INSERT BS3> <INSERT BS4> "),
                         div(
                           div(style = "border: 1px solid #ccc; padding: 10px;",
                               h1("BS1ribbon w/ ball and stick rep"),
                               NGLVieweROutput("BS1")),
                           div(style = "border: 1px solid #ccc; padding: 10px;",
                               h1("BS2ribbon w/ ball and stick rep"),
                               NGLVieweROutput("BS2")),
                           div(style = "border: 1px solid #ccc; padding: 10px;",
                               h1("BS3ribbon w/ ball and stick rep"),
                               NGLVieweROutput("BS3")),
                           div(style = "border: 1px solid #ccc; padding: 10px;",
                               h1("BS4ribbon w/ ball and stick rep"),
                               NGLVieweROutput("BS4")),
                         ),
                         p("So if we insert these sequences onto a mouse antibody backbone, then add an additional fluorescent tag to the chimeric protein, we could theoretically detect when P. Brassicae is infecting a field. As the chimeric proteins will bind to PbEL04 or PRO1, and we can detect the fluoresence!"),
                         actionButton("launchButton", "Launch Another Shiny App")
                ),
                tabPanel("Data Viewer", 
                         sidebarLayout(
                           sidebarPanel(
                             # Dropdown menu to select the PDB file
                             selectInput("pdbfile", "Select PDB File", choices = c("A1C2p1", "A1C2p2", "A1C2p3", "A1C2p4")),
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
                ),
    ),
  ),
)



server <- function(input, output) {
  #HERE'S THE VIEWER TRIAL SERVER INFO
  #HERE'S THE DATA FOR A1
  output$A1 <- renderNGLVieweR({
    NGLVieweR("PDBFilesA/A1.pdb") %>%
      addRepresentation("cartoon",
                        param = list(name = "cartoon", color = "#d3d3d3")
      ) %>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  observeEvent(input$add, {
    NGLVieweR_proxy("A1") %>%
      addSelection(isolate(input$type),
                   param =
                     list(
                       name = "A1sel1",
                       sele = isolate(input$selection),
                       colorValue = isolate(input$color)
                     )
      )
  })
  observeEvent(input$remove, {
    NGLVieweR_proxy("A1") %>%
      removeSelection("A1sel1")
  })
  
  #HERE'S THE DATA FOR B1
  output$B1 <- renderNGLVieweR({
    NGLVieweR("PDBFilesA/B1.pdb") %>%
      addRepresentation("cartoon",
                        param = list(name = "cartoon", color = "#dda0dd")
      ) %>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  observeEvent(input$add, {
    NGLVieweR_proxy("B1") %>%
      addSelection(isolate(input$type),
                   param =
                     list(
                       name = "B1sel1",
                       sele = isolate(input$selection),
                       colorValue = isolate(input$color)
                     )
      )
  })
  observeEvent(input$remove, {
    NGLVieweR_proxy("B1") %>%
      removeSelection("B1sel1")
  })
  
  #DEFINING THE A1BArev RESIDUE COLOURS
  #A1BArev_residue_colors <- rep("#d3d3d3", viewer$structure$size)  # Initialize with white color
  #residue_colors[59:77] <- "#FF0000"  # Color residues 1-20 red
  #residue_colors[99:119] <- "#FFFF00" 
  #residue_colors[140:159] <- "#FFc0cb"# Color residues 21-40 blue
  #HERE'S A1 BINDING AREAS REVEALED:
  
  #Here's the work to uploading an image
  output$June21 <- renderImage({
    list(src = "images/June21.png", width = "50%") # Replace with your image file path
  }, deleteFile = FALSE)
  
  output$PbEpi <- renderNGLVieweR({
    NGLVieweR("PDBFilesA/A1.pdb") %>%
      addRepresentation("ribbon", param = list(name = "cartoonPbelEpi", color = "#d3d3d3")) %>%
      addRepresentation("ribbon", param = list(name = "seleca", sele = "59-77", color = "#ff0000"))%>%
      addRepresentation("ribbon", param = list(name = "selecb", sele = "99-119", color = "#ffff00"))%>%
      addRepresentation("ribbon", param = list(name = "selecc", sele = "140-159", color = "#ffc0cb")) %>%
      addRepresentation("ball+stick", param = list(name = "selecab+s", sele = "59-77", colorScheme = "element", colorvalue = "#ff0000")) %>%
      addRepresentation("ball+stick", param = list(name = "selecbb+s", sele = "99-119", colorScheme = "element", colorvalue = "#ffff00")) %>%
      addRepresentation("ball+stick", param = list(name = "seleccb+s", sele = "140-159", colorScheme = "element", colorvalue = "#ffc0cb")) %>%
      
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  output$BS1 <- renderNGLVieweR({
    NGLVieweR("PDBFilesA/BS1-4/BS1.pdb") %>% 
      addRepresentation("ribbon", param = list(name = "cartoonBS1", color = "#ff7f00")) %>% 
      addRepresentation("ball+stick", param = list(name = "cartoonBS1b+s", colorScheme = "element", colorValue = "#ff7f00"))%>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  output$BS2 <- renderNGLVieweR({
    NGLVieweR("PDBFilesA/BS1-4/BS2.pdb") %>% 
      addRepresentation("ribbon", param = list(name = "cartoonBS2", color= "#a020f0"))%>%
      addRepresentation("ball+stick", param = list(name = "cartoonBS2b+s", colorScheme = "element", colorvalue= "#a020f0"))%>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  
  output$BS3 <- renderNGLVieweR({
    NGLVieweR("PDBFilesA/BS1-4/BS3.pdb") %>% 
      addRepresentation("ribbon", param = list(name = "cartoonBS4", color = "#a0522d")) %>% 
      addRepresentation("ball+stick", param = list(name = "cartoonBS1b+s", colorScheme = "element", colorValue = "#a0522d"))%>% 
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  output$BS4 <- renderNGLVieweR({
    NGLVieweR("PDBFilesA/BS1-4/BS4.pdb") %>% 
      addRepresentation("ribbon", param = list(name = "cartoonBS4", color= "#bc8f8f"))%>%
      addRepresentation("ball+stick", param = list(name = "cartoonBS2b+s", colorScheme = "element", colorvalue= "#bc8f8f")) %>%
      stageParameters(backgroundColor = input$backgroundColor) %>%
      setQuality("high") %>%
      setFocus(0) %>%
      setSpin(TRUE)
  })
  
  observeEvent(input$runScriptButton, {
    
  })
  chain_A_visible <- reactiveVal(TRUE)
  
  output$viewer <- renderNGLVieweR({
    selected_file <- switch(input$pdbFile,
                            "A1C2p1" = "A1C2Files/CombinedPreds/A1C2-A1C2p1.pdb",
                            "A1C2p2" = "A1C2Files/CombinedPreds/A1C2-A1C2p2.pdb",
                            "A1C2p3" = "A1C2Files/CombinedPreds/A1C2-A1C2p3.pdb",
                            "A1C2p4" = "A1C2Files/CombinedPreds/A1C2-A1C2p4.pdb", 
                            "A1F2p1" = "A1F2Files/CombinedPreds/A1F2-A1F2p1.pdb"
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
                       "A1C2p4" = "A1C2Files/CSV/A1F2Trial3.csv", 
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



