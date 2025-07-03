PDFs.data.panel_UI <- fluidPage(
  conditionalPanel(
    condition = "output.fileUploaded",
    
    #shinyDirButton('PDF_HCE_folder', 'Select a folder', 'Please select a folder', FALSE),
    
    #hr(),
    headerPanel("Lector de pdfs"),
    
    fileInput("PDF_HCEs_zip", label = "Selecciona un archivo ZIP:",accept = c(".zip")),
  )
  

  
  
  
)
