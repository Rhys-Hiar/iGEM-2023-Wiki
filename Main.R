setwd("/Users/rhyshiar/iGEM2023wiki")
if (interactive()) {
library(shiny)
source('UITest1.R', local = TRUE)
source('SerTest1.R', local = TRUE)

shinyApp(
  ui = UITest1,
  server = SerTest1
)
}

