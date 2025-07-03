initialize_univariate <- reactive({
  # Univariate variables
  uni_categoric_cols <- global_categoric_cols()
  uni_numeric_cols <- global_numeric_cols()
  uni_date_cols <- global_date_cols()
  
  for (name_var in global_univariate_categoric_not()) {
    uni_categoric_cols <- uni_categoric_cols[uni_categoric_cols != name_var]
  }
  for (name_var in global_univariate_numeric_not()) {
    uni_numeric_cols <- uni_numeric_cols[uni_numeric_cols != name_var]
  }
  for (name_var in global_univariate_dates_not()) {
    uni_date_cols <- uni_date_cols[uni_date_cols != name_var]
  }
  
  uni_var_choices(list(Ninguna = NONE_SELECTED_CONST,
                       "Categóricas" = uni_categoric_cols, 
                       "Numéricas" = uni_numeric_cols, 
                       "Fechas" = uni_date_cols))
})

initialize_bivariate <- reactive({
  bi_categoric_cols <- global_categoric_cols()
  bi_numeric_cols <- global_numeric_cols()
  
  for (name_var in global_bivariate_category_not()) {
    bi_categoric_cols <- bi_categoric_cols[bi_categoric_cols != name_var]
  }
  for (name_var in global_bivariate_numeric_not()) {
    uni_numeric_cols <- bi_numeric_cols[bi_numeric_cols != name_var]
  }
  
  bi_var_choices(list(Ninguna = NONE_SELECTED_CONST,
                      "Categóricas" = bi_categoric_cols, 
                      "Numéricas" = bi_numeric_cols))
})

initialize_survival <- reactive({
  survival_possible_event_cols <- global_possible_event_cols()
  survival_categoric_cols <- global_categoric_cols()
  survival_numeric_cols <- global_numeric_cols()
  
  survival_categoric_cols <- survival_categoric_cols[survival_categoric_cols != "ESTADO_SG"]
  
  for (name_var in global_survival_status_not()) {
    survival_possible_event_cols <- survival_possible_event_cols[survival_possible_event_cols != name_var]
  }
  for (name_var in global_survival_time_not()) {
    survival_numeric_cols <- survival_numeric_cols[survival_numeric_cols != name_var]
  }
  for (name_var in global_survival_strat_not()) {
    survival_categoric_cols <- survival_categoric_cols[survival_categoric_cols != name_var]
  }
  
  surv_strata_cols(c(NONE_SELECTED_CONST, survival_categoric_cols))
  surv_numeric_cols(survival_numeric_cols)
  surv_event_cols(survival_possible_event_cols)
})