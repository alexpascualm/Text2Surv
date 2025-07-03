#############################
# Variable - Select Changed Event
observeEvent(input$univariate_var, {
  if( is.factor(get_df()[[input$univariate_var]]) ) currentVarTypes$uni = CATEGORIC_TYPE
  else if( is.numeric(get_df()[[input$univariate_var]]) ) currentVarTypes$uni = NUMERIC_TYPE
  else if ( is.date(get_df()[[input$univariate_var]]) ) currentVarTypes$uni = DATE_TYPE
  else currentVarTypes$uni = NULL
})


observeEvent(input$bivariate_ind, {
  if( is.factor(get_df()[[input$bivariate_ind]]) ) currentVarTypes$bi_ind = CATEGORIC_TYPE
  else if( is.numeric(get_df()[[input$bivariate_ind]]) ) currentVarTypes$bi_ind = NUMERIC_TYPE
  else if ( is.date(get_df()[[input$bivariate_ind]]) ) currentVarTypes$bi_ind = DATE_TYPE
  else currentVarTypes$bi_ind = NULL
})


observeEvent(input$bivariate_dep, {
  if( is.factor(get_df()[[input$bivariate_dep]]) ) currentVarTypes$bi_dep = CATEGORIC_TYPE
  else if( is.numeric(get_df()[[input$bivariate_dep]]) ) currentVarTypes$bi_dep = NUMERIC_TYPE
  else if ( is.date(get_df()[[input$bivariate_dep]]) ) currentVarTypes$bi_dep = DATE_TYPE
  else currentVarTypes$bi_dep = NULL
})


observeEvent(input$survival_event, {
  req(input$survival_event)
  vals <- unique(na.omit(get_df()[[input$survival_event]]))
  updateSelectizeInput(session, 'survival_event_value', choices = vals,
                       server = FALSE, selected = vals[[2]])
})


observeEvent(input$multivar_dep, {
  req(input$multivar_dep)
  vals <- unique(na.omit(get_df()[[input$multivar_dep]]))
  updateSelectizeInput(session, 'multivar_event_value', choices = vals,
                       server = FALSE, selected = vals[[2]])
})