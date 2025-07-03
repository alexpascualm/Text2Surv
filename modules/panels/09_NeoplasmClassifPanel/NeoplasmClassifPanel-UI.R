Neoplasm.Classif.data.panel_UI <- ui <- fluidPage(
 
 
  conditionalPanel(
    condition = "output.fileUploaded",
    fluidRow(
      
      headerPanel("Clasificador de neoplasias"),
      
      
      actionButton("NeoplasmClassif_Buttom", "Infiera las neoplasias")
    )
  )
)