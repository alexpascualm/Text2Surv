#######################
### SURVIVAL ANALYSIS
#######################

output$survival_plot <- renderPlot({
  req(get_df(), input$survival_event, input$survival_event_value)
  error_control <- FALSE
  
  print("### TEST A ::"); print(input$survival_switch_calculate_time); print(input$survival_time)
  validate(
    need(input$survival_event != NONE_SELECTED_CONST,
         "Seleccione alguna variable de evento"),
    need(xor(
      (input$survival_switch_calculate_time == FALSE && input$survival_time != NONE_SELECTED_CONST),
      input$survival_switch_calculate_time == TRUE
    ), "Seleccione alguna variable para el tiempo")
  )
  print("### TEST B ::"); print(input$survival_switch_calculate_time); print(input$survival_time)
  
  df <- get_df()
  # print("SURV - PLOT")
  df$status <- df[[input$survival_event]]
  df$status <- ifelse(df[[input$survival_event]]==input$survival_event_value, 1, 0)
  
  if(input$survival_switch_calculate_time){
    time_scale <- get_time_scale(input$survival_time_scale)
    time_event <- elapsed_time(df[[input$survival_time_end]], df[[input$survival_time_init]], time_scale)
    time_censor <- elapsed_time(df[[input$survival_time_censor]], df[[input$survival_time_init]], time_scale)
    
    print(time_scale); print(time_event); print(time_censor)
    
    idx <- which(df$status != 1)
    
    print(time_censor[idx])
    
    df$time <- time_event
    df$time[idx] <- time_censor[idx]
  }
  else {
    df$time <- df[[input$survival_time]]
  }
  
  
  X_AXIS_LABEL <- if(input$default_graph_param_x_label_surv == "Por Defecto")
    "Tiempo de Seguimiento" else input$graph_param_x_label_surv
  Y_AXIS_LABEL <- if(input$default_graph_param_y_label_surv == "Por Defecto")
    paste0("Probabilidad de ", input$survival_event) else input$graph_param_y_label_surv
  TITLE_LABEL <- if(input$default_graph_param_title_label_surv == "Por Defecto")
    NULL else input$graph_param_title_label_surv
  
  
  if(input$survival_strata != NONE_SELECTED_CONST){
    var_height <- 400
    
    if (user_input$hospital == digest("h_leon")){
      if (input$survival_strata %in% c("ESTADIO", "MEDICO", "ORIGEN_DE_LA_INTERCONSULTA", "TIPO_ALTA")){
        var_height <- 800
      }
      if (input$survival_strata %in% c("DCO_GRUPOS", "DCO", "FECHA_PROXIMA_CITA", "COMENTARIOS")){
        var_height <- 1400
      }
    }
    
    df$strata <- as.factor(df[[input$survival_strata]])
    
    curr_vars <- c("time", "status", "strata")
    df <- complete_cases_df(df, curr_vars)
    
    tryCatch({
      fit <- survfit(Surv(time, status) ~ strata, df)
      df_surv <- surv_summary(fit, df)
      surv_diff <- survdiff(Surv(time, status) ~ strata, data = df)
      surv_diff_pval <- 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
      
      plot_variables$surv_log_rank  <- data.frame(
        "Prueba" = "Log-Rank Test",
        "P Value" = signif(surv_diff_pval, 6),
        "P-Value Significance" = get_significance_level(surv_diff_pval))
      
      g <- ggsurvplot_df(df_surv,
                         censor = FALSE,
                         conf.int = input$switch_confidence_interval,
                         xlab = X_AXIS_LABEL, #global_plot_options$xlab,
                         ylab = Y_AXIS_LABEL, #global_plot_options$ylab,
                         title = TITLE_LABEL,
                         ggtheme = global_plot_theme,
                         legend = "bottom",
                         legend.labs = levels(df$strata),
                         legend.title = "",
                         height = var_height
      )
      
      df_drop <- droplevels(df_surv)
      
      plot_variables$surv_risk_table <- ggrisktable(fit, df_drop, color="strata",
                                                    xlab = "Tiempo de Seguimiento",#global_plot_options$xlab,
                                                    font.size=3,
                                                    legend.title = "", y.text = F,
                                                    risk.table.title ="Número de pacientes en riesgo")
      
      colnames(df_surv) <- c("Tiempo", "N Riesgo", "N Evento", "N Censurado", 
                             "Supervivencia", "Error Estandar",
                             "IC inferior 95%", "IC superior 95%", "Strata")
      error_control <- TRUE
    }, error=function(cond){
      print(cond)
      shinyalert("Error", "Solo un grupo para mostrar. Por favor, revise la variable de estratificación.", type = "error")
    })
    
  } else {
    curr_vars <- c("time", "status")
    df <- complete_cases_df(df, curr_vars)
    
    fit <- survfit(Surv(time, status) ~ 1, df)
    df_surv <- surv_summary(fit, df)
    
    g <- ggsurvplot_df(df_surv,
                       palette = "blue",
                       legend = "none",
                       censor = FALSE,
                       conf.int = input$switch_confidence_interval,
                       xlab = X_AXIS_LABEL, #global_plot_options$xlab,
                       ylab = Y_AXIS_LABEL, #global_plot_options$ylab,
                       title = TITLE_LABEL,
                       ggtheme = global_plot_theme
    )
    
    plot_variables$surv_risk_table <- ggrisktable(fit, df, 
                                                  xlab = "Tiempo de Seguimiento",#global_plot_options$xlab, 
                                                  font.size=3,
                                                  legend.title = "", ylab  = "", y.text = F,
                                                  color ="blue", risk.table.title ="Número de pacientes en riesgo")
    
    colnames(df_surv) <- c("Tiempo", "N Riesgo", "N Evento", "N Censurado", 
                           "Supervivencia", "Error Estandar",
                           "IC inferior 95%", "IC superior 95%")
    error_control <- TRUE
  }
  if (error_control) {
    df_surv <- df_surv[order(df_surv[["Tiempo"]]),]
    row.names(df_surv) <- NULL
    plot_variables$surv_summary_df <- df_surv
    
    g
  }
})

output$risk_table <- renderPlot({
  req(plot_variables$surv_risk_table)
  plot_variables$surv_risk_table
})

output$survival_comparison <- renderDT({
  req(plot_variables$surv_log_rank)
  if(input$survival_strata != NONE_SELECTED_CONST){
    
    # print("SURV - TABLE")
    df <- plot_variables$surv_log_rank
    colnames(df) <- c("Prueba", "P-Value", "Significancia P-Value")
    
    datatable(df, 
              extensions = "Buttons",
              options = list(autoWidth = FALSE, 
                             dom = 'lfrtBip', buttons = c('copy', 'excel'),
                             scrollX = TRUE))
  }
  
})


output$survival_analysis <- renderDT({
  req(plot_variables$surv_summary_df)
  
  # print("SURV - TABLE")
  df <- plot_variables$surv_summary_df
  
  numeric_cols <- colnames(df)[sapply(df, is.numeric)]
  
  datatable(df,
            extensions = "Buttons",
            options = list(pageLength = 25, autoWidth = FALSE, 
                           dom = 'lfrtBip', buttons = c('copy', 'excel'),
                           scrollX = TRUE)) %>% 
    formatRound(numeric_cols[-c(1, 2, 3, 4)], 4)
})


output$Analize.data.panel_Survival <- renderUI({
  update_variables('survival')
  analize.data.panel_Survival_UI
})