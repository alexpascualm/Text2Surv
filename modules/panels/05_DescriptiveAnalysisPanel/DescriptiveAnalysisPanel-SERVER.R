#######################
### UNIVARIATE ANALYSIS
#######################

number <- reactiveValues(pacients = 0)
output$text <- renderText({HTML(paste0("<h3>", number$pacients,"</h3>"))})

output$univariate_plot <- renderPlotly({
  req(get_df(), input$univariate_var, currentVarTypes$uni)
  
  validate(
    need(input$univariate_var != NONE_SELECTED_CONST,
         "Seleccione alguna variable")
  )
 
  
  df <- get_df()
  
 
  number$pacients <- nrow(df)
  # print("UNI - PLOT")
  
  
  X_AXIS_LABEL <- if(input$default_graph_param_x_label_uni == "Por Defecto")
    input$univariate_var else input$graph_param_x_label_uni
  Y_AXIS_LABEL <- if(input$default_graph_param_y_label_uni == "Por Defecto")
    "Count" else input$graph_param_y_label_uni
  TITLE_LABEL <- if(input$default_graph_param_title_label_uni == "Por Defecto")
    "" else input$graph_param_title_label_uni
  
  
  # Numeric
  if(currentVarTypes$uni == NUMERIC_TYPE){
    fig <- plot_ly(x = df[[input$univariate_var]], type = "histogram", nbinsx = 30) %>% 
      layout(bargap=0.05, 
             xaxis = list(title = X_AXIS_LABEL),
             yaxis = list(title = Y_AXIS_LABEL, gridwidth = 1),
             title = TITLE_LABEL
      )
  }
  # Categoric
  else if(currentVarTypes$uni == CATEGORIC_TYPE) {
    var_height <- 400
    
    if (user_input$hospital == digest("h_leon")){
      if (input$univariate_var %in% c("ORIGEN_DE_LA_INTERCONSULTA", "PACIENTES_SIN_CITA", "TIPO_ALTA", "COMENTARIOS")){
        var_height <- 500
      }
      if (input$univariate_var %in% c("DCO_GRUPOS", "DCO")){
        var_height <- 1000
      }
    }
    
    df <- get_df() %>%
      group_by_(input$univariate_var) %>%
      summarise(count = n())
    
    fig <- plot_ly(y = df$count, x = df[[input$univariate_var]],
                   type = 'bar',
                   marker = list(color=create_highlight_colors(df, "count"), 
                                 line = list(width=1, color="black")),
                   hovertemplate = '<b>%{x}:</b> %{y}', height=var_height
    )
    
    
    fig <- fig %>% layout(
      xaxis = list(title = X_AXIS_LABEL),
      yaxis = list(title = Y_AXIS_LABEL, gridwidth = 1),
      title = TITLE_LABEL
    )
  } 
  # Date
  else if(currentVarTypes$uni == DATE_TYPE){
    fig <- ggplot(df, aes_string(input$univariate_var)) + geom_freqpoly() + 
      labs(title = TITLE_LABEL, x = X_AXIS_LABEL, y = Y_AXIS_LABEL)
    
    fig <- ggplotly(fig)
  }
  
  fig
})

output$univariate_analysis <- renderDT({
  req(get_df(), input$univariate_var, currentVarTypes$uni)
  
  validate(
    need(input$univariate_var != NONE_SELECTED_CONST,
         "Seleccione alguna variable")
  )
  
  df <-  get_df()
  # print("UNI - TABLE")
  # Numeric
  if(currentVarTypes$uni == NUMERIC_TYPE){
    
    df$Values <- df[[input$univariate_var]]
    df <- df %>%
      tab_frequencies(Values)
    
    df$percent <- df$percent*100
    df$cum_percent <- df$cum_percent*100
    
    df <- df[,-c(4,5)]
    
    df[[1]] <- as.character(df[[1]])
    df[[1]][ which(is.na(df[[1]])) ] <- "NA"
    colnames(df) <- SUMMARY_DF_COLNAMES_CATEGORIC
  } 
  # Categoric
  else if(currentVarTypes$uni == CATEGORIC_TYPE){
    df$Values <- df[[input$univariate_var]]
    df <- df %>%
      tab_frequencies(Values)
    
    df$percent <- df$percent*100
    df$cum_percent <- df$cum_percent*100
    
    df <- df[,-c(4,5)]
    
    df[[1]] <- as.character(df[[1]])
    df[[1]][ which(is.na(df[[1]])) ] <- "NA"
    colnames(df) <- SUMMARY_DF_COLNAMES_CATEGORIC
  } 
  # Date
  else if(currentVarTypes$uni == DATE_TYPE){
    summ <- summary(df[[input$univariate_var]])
    
    n_na <- sum(is.na(df[[input$univariate_var]]))
    
    df <- do.call(rbind, list(as.character(summ)))
    
    colnames(df) <- c("Min", "1st Qu.", "Median", "Mean", "3rd Qu.", "Max")
    return(
      datatable(df, options = list(pageLength = 25, autoWidth = FALSE, 
                                   scrollX = TRUE))
    )
  }
  
  numeric_cols <- colnames(df)[sapply(df, is.numeric)]
  
  datatable(df,
            extensions = "Buttons",
            options = list(pageLength = 25, autoWidth = FALSE, 
                           dom = 'lfrtBip', buttons = c('copy', 'excel'),
                           scrollX = TRUE)) %>% 
    formatRound(numeric_cols[-1], 3)
})

output$Analize.data.panel_Descriptive <- renderUI({
  update_variables('univariate')
  analize.data.panel_Descriptive_UI
})