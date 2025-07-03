
output$load.data.panel <- renderUI({
  if (file_uploaded()){
    shinyalert("Aviso", "Ya existen datos cargados de una sesión anterior. 
               Si importa nuevos datos, los anteriores se sobreescribiran", type = "warning")
  }
  
  load.data.panel_UI
})