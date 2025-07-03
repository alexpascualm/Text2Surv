
# Función para obtener un valor o NA si ocurre un error
get_value_or_na <- function(result, index1,index2) {
  tryCatch(
    result[[index1]][index2],
    error = function(e) NA
  )
}

# Función para procesar cada archivo PDF de Leon

procesar_pdf_Leon <- function(pdf_file) {
  # Extraer el texto del PDF
  texto <- pdf_text(pdf_file)
  
  
  # DNI
  patron_DNI <- "Pasaporte: (\\d{8}[A-Z])"
  resultado_DNI <- regmatches(texto, regexec(patron_DNI, texto))
  DNI <- get_value_or_na(resultado_DNI, 1,2)
  
  # HCE
  patron_HCE <- "Nº Historia Clínica: (\\d+)"
  resultado_HCE <- regmatches(texto, regexec(patron_HCE, texto))
  HCE <- get_value_or_na(resultado_HCE, 1,2)
  
  # Nombre y apellidos
  patron_NombreApellidos <- "Nombre y Apellidos: ([A-Za-záéíóúüñÁÉÍÓÚÜÑ]+) ([A-Za-záéíóúüñÁÉÍÓÚÜÑ]+) ([A-Za-záéíóúüñÁÉÍÓÚÜÑ]+)"
  resultado_NombreApellidos <- regmatches(texto, regexec(patron_NombreApellidos, texto))
  
  
  
  NOMBRE <- get_value_or_na(resultado_NombreApellidos, 2,2)
  Apellido1 <- get_value_or_na(resultado_NombreApellidos, 2,3)
  Apellido2 <- get_value_or_na(resultado_NombreApellidos, 2,4)
  
  # SEXO 
  patron_Sexo <- "Sexo: ([A-Za-záéíóúüñÁÉÍÓÚÜÑ]+)"
  resultado_Sexo <- regmatches(texto, regexec(patron_Sexo, texto))
  Sexo <- get_value_or_na(resultado_Sexo, 1,2)
  
  # FECHA NACIMIENTO
  patron_FechaNacimiento <- "Fecha nacimiento: \\b(\\d{2}/\\d{2}/\\d{4})\\b"
  resultado_FechaNacimiento <- regmatches(texto, regexec(patron_FechaNacimiento, texto))
  FechaNacimiento <- get_value_or_na(resultado_FechaNacimiento, 1,2)
  
  # EDAD
  patron_EDAD <- "Edad: (\\d+)"
  resultado_EDAD <- regmatches(texto, regexec(patron_EDAD, texto))
  EDAD <- get_value_or_na(resultado_EDAD, 1,2)
  
  return(list("DNI"=DNI,"NHCE" = HCE, "Nombre" = NOMBRE, "Apellido1" = Apellido1, "Apellido2" = Apellido2, "Sexo" = Sexo, "Fecha_nacimiento" = FechaNacimiento, "Edad" = EDAD))
}

check_pdfs_leon <- function(pdf_files){
  
  # Cargamos df y añadimos columna para comprobar el estado
  df <- get_active_df()
  
 
  
  if(!("DNI" %in% names(df)) ){
    DNI <- rep(NA, nrow(df))
    
    df <- cbind(DNI, df)
    df$DNI <- as.character(df$DNI)
  }
  
  if(!("ESTADO_PDF" %in% names(df)) ){
    ESTADO_PDF <- rep("Previo", nrow(df))
    
    df <- cbind(ESTADO_PDF, df)
    df$ESTADO_PDF <- as.character(df$ESTADO_PDF)
    
  }
  
 
  
  # Se cambian a "Previo" aquellos pacientes que no necesitaban validación y antes eran considerados "Nuevo" o "Consolidado" // TO DO
  
  # df$ESTADO_PDF <- ifelse(df$ESTADO_PDF == "Nuevo", "Previo", df$ESTADO_PDF)
  # df$ESTADO_PDF <- ifelse(df$ESTADO_PDF == "Consolidado", "Previo", df$ESTADO_PDF)
  # 
  
  # if(!("TEXTO_PDF" %in% names(df)) ){
  #   TEXTO_PDF <- rep(NA, nrow(df))
  # 
  #   df <- cbind(df, TEXTO_PDF)
  #   df$TEXTO_PDF <- as.character(df$TEXTO_PDF)
  # }
  
  

  Numero_Consolidados <- 0
  Numero_Comprobados <- 0
  Numero_Añadidos <- 0
  
  # Conversion de factor a character para añadir nuevos valores
  
  df$NHC_CAULE <- as.character(df$NHC_CAULE)
  df$NOMBRE <- as.character(df$NOMBRE)
  df$APELLIDO_1 <- as.character(df$APELLIDO_1)
  df$APELLIDO_2 <- as.character(df$APELLIDO_2)
  
  for(file in pdf_files){
  
    Campos_PDF <- procesar_pdf_Leon(file)
   
    if (Campos_PDF$NHCE %in% df$'NHC_CAULE'){
      print("Encontrado")
      print(Campos_PDF$NHCE)
      
      index_fila <- which(df$NHC_CAULE == Campos_PDF$NHCE)
      
      index_fila_no_validado_nuevo <- -1
      
      
      # Si hay dos NHCE iguales (En principio solo sucede si se han subido PDFs antes y existía discrepancia)
      # (Si el cliente sube datos con NHCE repetidos debería gestionarse antes)
      
      if(length(index_fila)>1){
        

        index_fila_no_validado_nuevo <- ifelse(df[index_fila[2],]$ESTADO_PDF=="Sin validar - Nuevo",index_fila[2],index_fila[1])
   
        index_fila <-  ifelse(df[index_fila[1],]$ESTADO_PDF=="Sin validar - Antiguo",index_fila[1],index_fila[2])
       
   
      }
    
      
      Discrepancia <- FALSE
      
    
      if(!is.na(Campos_PDF$Apellido1)){
        if(df[index_fila,]$APELLIDO_1!=Campos_PDF$Apellido1){
          Discrepancia <- TRUE
          
        }
      }
      
      if(!is.na(Campos_PDF$Apellido2)){
        if(df[index_fila,]$APELLIDO_2!=Campos_PDF$Apellido2){
          Discrepancia <- TRUE
         
        }
      } 
      
      if(!is.na(Campos_PDF$Nombre)){
        if(df[index_fila,]$NOMBRE!=Campos_PDF$Nombre){
          Discrepancia <- TRUE
          
        }
      }  
      
      
      if(!is.na(Campos_PDF$Sexo)){
        if(df[index_fila,]$SEXO!=ifelse(Campos_PDF$Sexo=="HOMBRE","M","F")){
          Discrepancia <- TRUE
        
        }
      }
      
      if(!is.na(Campos_PDF$Edad)){
        if(df[index_fila,]$EDAD!=as.numeric(Campos_PDF$Edad)){
          Discrepancia <- TRUE
          
        }
        
      }
      
    
      
      if(!is.na(Campos_PDF$Fecha_nacimiento)){
        if(df[index_fila,]$FECHA_NAC!=as.Date(Campos_PDF$Fecha_nacimiento,format="%d/%m/%Y")){
          Discrepancia <- TRUE
          
        }
      }
      
      
      
      # if(!is.na(Campos_PDF$DNI)){
      #   if(df[index_fila,]$DNI!=Campos_PDF$DNI){
      #     Discrepancia <- TRUE
      #     
      #   }
      # }
      
      
      
      if(Discrepancia){
        
        df[index_fila,]$ESTADO_PDF <- "Sin validar - Antiguo"
        
        # Si no se habia añadido el paciente por validar anteriormente
        if(index_fila_no_validado_nuevo==-1){
          
          
          df <- df %>% add_row()
          
          
          df[nrow(df),] <- df[index_fila,]
          
          df[nrow(df),]$ESTADO_PDF <- "Sin validar - Nuevo" 
          
          df[nrow(df),]$DNI <-  Campos_PDF$DNI
          df[nrow(df),]$NHC_CAULE <-  Campos_PDF$NHCE
          df[nrow(df),]$APELLIDO_1 <-  Campos_PDF$Apellido1
          df[nrow(df),]$APELLIDO_2  <-  Campos_PDF$Apellido2
          df[nrow(df),]$NOMBRE <-  Campos_PDF$Nombre
          df[nrow(df),]$SEXO <-  ifelse(Campos_PDF$Sexo=="HOMBRE","M","F")
          df[nrow(df),]$FECHA_NAC <- as.Date(Campos_PDF$Fecha_nacimiento,format="%d/%m/%Y")
          df[nrow(df),]$EDAD <- as.numeric(Campos_PDF$Edad)
          
          
          
          
        # Si ya se habia añadido el paciente por validar anteriormente 
        }else{
          
          
          df[index_fila_no_validado_nuevo,]$ESTADO_PDF <- "Sin validar - Nuevo" 
          
          df[index_fila_no_validado_nuevo,]$DNI <-  Campos_PDF$DNI
          df[index_fila_no_validado_nuevo,]$NHC_CAULE <-  Campos_PDF$NHCE
          df[index_fila_no_validado_nuevo,]$APELLIDO_1 <-  Campos_PDF$Apellido1
          df[index_fila_no_validado_nuevo,]$APELLIDO_2  <-  Campos_PDF$Apellido2
          df[index_fila_no_validado_nuevo,]$NOMBRE <-  Campos_PDF$Nombre
          df[index_fila_no_validado_nuevo,]$SEXO <-  ifelse(Campos_PDF$Sexo=="HOMBRE","M","F")
          df[index_fila_no_validado_nuevo,]$FECHA_NAC <- as.Date(Campos_PDF$Fecha_nacimiento,format="%d/%m/%Y")
          df[index_fila_no_validado_nuevo,]$EDAD <- as.numeric(Campos_PDF$Edad)
        } 
        
       
        # Existen datos en conflicto
        conflict_data(1)
        
        print("Comprobado-Con cambios")
        Numero_Comprobados <- Numero_Comprobados + 1
        
        
      }else{
        
        # Borrar fila antigua de existir (Validar Nuevo) y resta 1 a Numero_Comprobados 
        
        if(index_fila_no_validado_nuevo!=-1){
          df <- df[-index_fila_no_validado_nuevo, ]
          
          
          Numero_Comprobados <- Numero_Comprobados - 1
          
          print("Fila a validar anterior eliminada")
        }
        
        df[index_fila,]$DNI <- Campos_PDF$DNI
        
        df[index_fila,]$ESTADO_PDF <- "Consolidado"
        
        
        
       
        
        print("Comprobado-Sin cambios")
        
        
        Numero_Consolidados <- Numero_Consolidados +1
        
      }
      
      
      
    }else{
      
      
      print("No encontrado")
      print(Campos_PDF$NHCE)
      
     
      df <- df %>% add_row()
     
      df[nrow(df),]$ESTADO_PDF <- "Nuevo" 
     
      df[nrow(df),]$DNI <-  Campos_PDF$DNI
      df[nrow(df),]$NHC_CAULE <-  Campos_PDF$NHCE
      df[nrow(df),]$APELLIDO_1 <-  Campos_PDF$Apellido1
      df[nrow(df),]$APELLIDO_2  <-  Campos_PDF$Apellido2
      df[nrow(df),]$NOMBRE <-  Campos_PDF$Nombre
      df[nrow(df),]$SEXO <-  ifelse(Campos_PDF$Sexo=="HOMBRE","M","F")
      df[nrow(df),]$FECHA_NAC <- as.Date(Campos_PDF$Fecha_nacimiento,format="%d/%m/%Y")
      df[nrow(df),]$EDAD <- as.numeric(Campos_PDF$Edad)
    
      
      
     
      
      Numero_Añadidos <- Numero_Añadidos+1
      
      print("Importado")
    }
  }
  
  shinyalert("A raiz de los pdf subidos:", 
             paste0("Se han comprobado sin cambios ",Numero_Consolidados," pacientes.","
                    Se han comprobado con cambios ",ifelse(Numero_Comprobados<0,0,Numero_Comprobados)," pacientes.","
                    Se han añadido ",Numero_Añadidos, " pacientes nuevos."),
             type = "info")
  
  
  
  active_df(df)
  save_df()
  load_df()
  filter_df(filter_not_validated_patients(df))
  

}

# Subida de PDFS a traves de ZIP

observeEvent(input$PDF_HCEs_zip,{
  
  zip_path <- input$PDF_HCEs_zip$datapath
  
  # Directorio temporal para descomprimir el ZIP
  temp_dir <- tempdir()
  
  # Descomprimir el archivo ZIP en el directorio temporal
  unzip(zip_path, exdir = temp_dir)

  
  # Obtener la lista de archivos PDF descomprimidos
  pdf_files <- list.files(temp_dir, pattern = "\\.pdf$", full.names = TRUE)
  


 
  if (user_input$hospital == digest("h_leon")){
    
    
     
    check_pdfs_leon(pdf_files)
  }
  
  
  # # Eliminar el directorio temporal y sus archivos
  # unlink(temp_dir, recursive = TRUE)
})





# # Subida de PDFS a traves de folder
# 
# shinyDirChoose(input, 'PDF_HCE_folder',roots = c(home = 'C:'), filetypes = NULL)
# 
# 
#  observeEvent(input$PDF_HCE_folder, {
#    
#   req(input$PDF_HCE_folder)
#    
#   selected_folder <- parseFilePaths(roots = c(home = 'C:'), input$PDF_HCE_folder)
#   
#   print(selected_folder$datapath)
#    
#   pdf_files <- list.files(path = selected_folder$datapath, pattern = "\\.pdf$", full.names = TRUE)
# 
# 
#     if (user_input$hospital == digest("h_leon")){
#       check_pdfs_leon(pdf_files)
#     }
# 
#  })

output$read.pdf.panel<- renderUI({

  PDFs.data.panel_UI
})

