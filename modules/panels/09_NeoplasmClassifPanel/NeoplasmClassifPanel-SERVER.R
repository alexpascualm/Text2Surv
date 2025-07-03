output$Neoplasm.Classif.data.panel <- renderUI({
  Neoplasm.Classif.data.panel_UI
})

#### VERSION OLD #### MODIFICAR

observeEvent(input$NeoplasmClassif_Buttom, {

 
  withProgress(message="Infiriendo neoplasias", value=100,{
    
    #Cargamos el script correspondiente
    reticulate::source_python('PLN_Classificator/neoplasm_classification_process.py')

    df <- get_active_df()
    # df_filtered <- get_df()
    
     
    # Mejorar para que tome valores del Texto_PDF si existe y no es nulo

    if (user_input$hospital == digest("h_leon")){
      if(!("TEXTO_PDF" %in% names(df)) ){
        Texto_Paciente <- paste(df$DCO, df$DCO_GRUPOS, sep ="")
      }else{
        Texto_Paciente <- paste(df$DCO, df$DCO_GRUPOS, sep ="")
      }
    }
    
    
    Python_df <- data.frame(ID = df$NHC_CAULE, Texto =Texto_Paciente)
    
   
    ## Tipo de modelo a utilizar
    UsedModel<-"ML"

    if (UsedModel=="ML"){
     
      clasificador <- py$NeoplasmClassification("./PLN_Classificator/","ML","Not Null?")
      
    }else if(UsedModel=="RNN"){

      clasificador <- py$NeoplasmClassification("./PLN_Classificator/","RNN","word2vec")

    }else if(UsedModel=="fastText"){
    
      clasificador <- py$NeoplasmClassification("./PLN_Classificator/","fastText","Not Null?")
      

    }
    
     df_Neoplasias<-clasificador$run(Python_df)
     
     
     df$NEOPLASIA <- df_Neoplasias$Neoplasia_Predicha
  
     # Update df
     active_df(df)
     save_df()
     load_df()
     filter_df(filter_not_validated_patients(df))
     
     
  })
  
  shinyalert("Inferencia finalizada","Se ha inferido una probable neoplasia para cada paciente",
             type = "success")
  
})

