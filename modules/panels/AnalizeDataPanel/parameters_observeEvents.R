#######################
### UNIVARIATE
#######################

observeEvent(input$default_graph_param_x_label_uni, {
  if(input$default_graph_param_x_label_uni == "Por Defecto"){
    shinyjs::disable("graph_param_x_label_uni")
  } else {
    shinyjs::enable("graph_param_x_label_uni")
  }
})

observeEvent(input$default_graph_param_y_label_uni, {
  if(input$default_graph_param_y_label_uni == "Por Defecto")
    shinyjs::disable("graph_param_y_label_uni")
  else 
    shinyjs::enable("graph_param_y_label_uni")
})

observeEvent(input$default_graph_param_title_label_uni, {
  if(input$default_graph_param_title_label_uni == "Por Defecto")
    shinyjs::disable("graph_param_title_label_uni")
  else 
    shinyjs::enable("graph_param_title_label_uni")
})

#######################
### BIVARIATE
#######################

observeEvent(input$default_graph_param_x_label_biv, {
  if(input$default_graph_param_x_label_biv == "Por Defecto"){
    shinyjs::disable("graph_param_x_label_biv")
  } else {
    shinyjs::enable("graph_param_x_label_biv")
  }
})

observeEvent(input$default_graph_param_y_label_biv, {
  if(input$default_graph_param_y_label_biv == "Por Defecto")
    shinyjs::disable("graph_param_y_label_biv")
  else 
    shinyjs::enable("graph_param_y_label_biv")
  
})

observeEvent(input$default_graph_param_title_label_biv, {
  if(input$default_graph_param_title_label_biv == "Por Defecto")
    shinyjs::disable("graph_param_title_label_biv")
  else 
    shinyjs::enable("graph_param_title_label_biv")
  
})


#######################
### SURVIVAL
#######################

observeEvent(input$default_graph_param_x_label_surv, {
  if(input$default_graph_param_x_label_surv == "Por Defecto"){
    shinyjs::disable("graph_param_x_label_surv")
  } else {
    shinyjs::enable("graph_param_x_label_surv")
  }
})

observeEvent(input$default_graph_param_y_label_surv, {
  if(input$default_graph_param_y_label_surv == "Por Defecto")
    shinyjs::disable("graph_param_y_label_surv")
  else 
    shinyjs::enable("graph_param_y_label_surv")
  
})

observeEvent(input$default_graph_param_title_label_surv, {
  if(input$default_graph_param_title_label_surv == "Por Defecto")
    shinyjs::disable("graph_param_title_label_surv")
  else 
    shinyjs::enable("graph_param_title_label_surv")
  
})