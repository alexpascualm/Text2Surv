
load.data.panel_UI <- fluidPage(
  headerPanel("Carga de datos"),
  fileInput('file', 'Archivo seleccionado',
            multiple=FALSE, accept = c(".xlsx", ".csv")) %>% 
    helper(type = "inline",
           title = "Formatos aceptados:",
           content = c("XLSX y CSV (con ';' como separador). Decimales separados por ','"),
           size = "s")
)
  