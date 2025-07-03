visualize.data.panel_UI <- fluidPage(
  conditionalPanel(
    condition = "output.fileUploaded",
    fluidRow(
      actionButton("modifyTable", label = "Modificar información del paciente", class = "btn-primary"),
      box(width = 12,
          reactableOutput("current_dataframe")
      ),
      downloadButton("downloadDataExcel", "Excel"),
      # downloadButton("downloadDataINDEF", "INDEF")
    ),
  )
)