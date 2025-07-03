library(shiny)
library(shinyalert)
library(shinyjs)
library(shinyWidgets)
library(shinythemes)
library(shinyFiles)
library(shinydashboard)
library(shinycssloaders)
library(openxlsx)
library(DT)
library(tidycomm)
library(dplyr)
library(stringr)
library(stringi)
library(survival)
library(survminer)
library(ggplot2)
library(plotly)
library(shinyBS) # bsModal
library(shinyhelper)
library(digest)
library(pdftools)
library(zip)
library(reactable)


library(reticulate)
use_condaenv("./.venv/")


options(dplyr.summarise.inform = FALSE)

### --- MODULES ---

modules_tabs <- c("global", "helper", "ui", "server")
for(m in modules_tabs){
    print(paste("Loading server module: ", m, sep=''))
    source(paste("modules/", m , ".R", sep=''), local = TRUE, encoding = "UTF-8")
}


### --- APP ---
shinyApp(ui, server)

