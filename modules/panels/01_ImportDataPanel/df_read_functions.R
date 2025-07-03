# Read function for check import data and show alerts
read_df_check <- reactive({
  tryCatch({
    file_ext <- tools::file_ext(input$file$name)
    
    if(file_ext == "xlsx"){ 
      tryCatch({
        df <- read.xlsx(input$file$datapath, sheet=1, detectDates = T,
                        skipEmptyCols = T, sep.names = "_", fillMergedCells = T)
          
      }, error=function(cond){
        print(cond)
        df <- read.xlsx(input$file$datapath, sheet=1, detectDates = F,
                        skipEmptyCols = T, sep.names = "_", fillMergedCells = T)
      })
        
    } else if (file_ext == "csv") {
      df <- read.csv(input$file$datapath, header = T, sep = ";", dec = ",",
                     fill = T)
    }
    
    colnames(df) <- toupper(make.unique(colnames(df), sep="_"))
    colnames(df) <- gsub("[()/º%]", "", colnames(df))
    colnames(df) <- gsub("[./º()]", "_", colnames(df))
    colnames(df) <- gsub("\\._", "_", colnames(df))
    colnames(df) <- gsub("__", "_", colnames(df))
    colnames(df) <- gsub("_$", "", colnames(df))
    colnames(df) <- stri_trans_general(str = colnames(df), id = "Latin-ASCII")
    
    if (user_input$hospital == digest("h_leon")){
      df <- process_data_leon(df)
    }
    
    
    # Borra el resgistro de columnas agregadas
    grouped_variable_list(NA)
    update_variables('var_list')
    
    nombre_carpeta <- paste0("./data/", user_input$hospital)
    if (!file.exists(nombre_carpeta)) {
      # Si no existe, crea la carpeta
      dir.create(nombre_carpeta)
    }
    saveRDS(NA, paste0(nombre_carpeta, "/last_session_grouped_variable.rds"))
    
    shinyalert("Datos Importados", "El fichero de datos ha sido importado correctamente!", type = "success")
    
    return(df)
  }, error=function(cond){
    print(cond)
    shinyalert("Error", paste0("No se ha podido importar el fichero correctamente. ERROR: ", 
                               cond), type = "error")
  })
})

# Read function for import and update data
read_df <- reactive({
  if(!file_uploaded()) return(NULL)
  
  file_ext <- tools::file_ext(input$file$name)
    
  if(file_ext == "xlsx"){ 
    tryCatch({
      df <- read.xlsx(input$file$datapath, sheet=1, detectDates = T,
                      skipEmptyCols = T, sep.names = "_", fillMergedCells = T)
        
    }, error=function(cond){
      print(cond)
      df <- read.xlsx(input$file$datapath, sheet=1, detectDates = F,
                      skipEmptyCols = T, sep.names = "_", fillMergedCells = T)
    })
      
  } else if (file_ext == "csv") {
    df <- read.csv(input$file$datapath, header = T, sep = ";", dec = ",",
                   fill = T)
  }
  
  colnames(df) <- toupper(make.unique(colnames(df), sep="_"))
  colnames(df) <- gsub("[()/º%]", "", colnames(df))
  colnames(df) <- gsub("[./º()]", "_", colnames(df))
  colnames(df) <- gsub("\\._", "_", colnames(df))
  colnames(df) <- gsub("__", "_", colnames(df))
  colnames(df) <- gsub("_$", "", colnames(df))
  colnames(df) <- stri_trans_general(str = colnames(df), id = "Latin-ASCII")
  
  if (user_input$hospital == digest("h_leon")){
    df <- process_data_leon(df)
  }
  
  print("### UPLOADED DATASET"); print((input$file)); print(dim(df))
  
  active_df(df); filter_df(NULL)
  save_df()
  
  return(df)
})