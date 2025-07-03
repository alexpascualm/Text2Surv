read_variables_not_selected <- reactive({
  if (!global_hidden_vars() &&
      file.exists(paste0(
        "data/",
        user_input$hospital,
        "/last_session_hidden_vars.rds"
      ))) {
    aux_selected_vars <-
      readRDS(paste0(
        "data/",
        user_input$hospital,
        "/last_session_hidden_vars.rds"
      ))
    
    global_univariate_categoric_not(aux_selected_vars$univariate_categoric_not)
    global_univariate_numeric_not(aux_selected_vars$univariate_numeric_not)
    global_univariate_dates_not(aux_selected_vars$univariate_dates_not)
    global_bivariate_category_not(aux_selected_vars$bivariate_category_not)
    global_bivariate_numeric_not(aux_selected_vars$bivariate_numeric_not)
    global_survival_status_not(aux_selected_vars$survival_status_not)
    global_survival_time_not(aux_selected_vars$survival_time_not)
    global_survival_strat_not(aux_selected_vars$survival_strat_not)
  } else {
    if (user_input$hospital == digest("h_leon")) {
      global_univariate_categoric_not(c("COMENTARIOS", "NOMBRE", "APELLIDO_1", "APELLIDO_2"))
      global_univariate_numeric_not(c("N_ORDEN", "POBLACION"))
      global_univariate_dates_not(c("FECHA_RECEPCION_EN_ONCO", "FECHA_1ª_CONSULTA_ONCOLOGIA", 
                                    "FECHA_PROXIMA_CITA", "FECHA_ULTIMA_CITA"))
      global_bivariate_category_not(c("COMENTARIOS", "NOMBRE", "APELLIDO_1", "APELLIDO_2"))
      global_bivariate_numeric_not(c("N_ORDEN"))
      global_survival_status_not(c("SEXO"))
      global_survival_time_not(c("EDAD", "N_ORDEN", "POBLACION", "TIEMPO_DEMORA", "TIEMPO_ESPERA"))
      global_survival_strat_not(c("ORIGEN_DE_LA_INTERCONSULTA", "PACIENTES_SIN_CITA", 
                                  "SITUACION", "TIPO_ALTA", "ESTADO_SG", "COMENTARIOS", "NOMBRE", 
                                  "APELLIDO_1", "APELLIDO_2"))
    }
  }
})

save_variables_not_selected <- reactive({
  if (global_hidden_vars()) {
    aux_selected_vars <-
      list(
        univariate_categoric_not = global_univariate_categoric_not(),
        univariate_numeric_not = global_univariate_numeric_not(),
        univariate_dates_not = global_univariate_dates_not(),
        bivariate_category_not = global_bivariate_category_not(),
        bivariate_numeric_not = global_bivariate_numeric_not(),
        survival_status_not = global_survival_status_not(),
        survival_time_not = global_survival_time_not(),
        survival_strat_not = global_survival_strat_not()
      )
    
    saveRDS(
      aux_selected_vars,
      paste0(
        "data/",
        user_input$hospital,
        "/last_session_hidden_vars.rds"
      )
    )
  }
})