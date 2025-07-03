
output$filter.data.panel <- renderUI({
  update_variables('filter')
  filter.data.panel_UI
})

first_filter <- reactiveValues(push = FALSE)

user_filterVar <- reactiveValues(push = FALSE)

output$filterVar <- reactive({
  if (user_filterVar$push) { return(1) }
  else { return(0) }
})

outputOptions(output, 'filterVar', suspendWhenHidden = FALSE)

#############################
### Filter Dataset

current_filters <- reactiveValues(groups = list(), inputs = list(), types = list(), active=list(), save=list(),
                                  min_val = list(), max_val = list(), factor_val = list())

output$optional <- renderUI({
  df <-  get_active_df()
  opt_var <- sort(choices_v())
  
  l_groups <- lapply(opt_var, function(var){
    filter_group <- paste0("Filtro de ", var)
    
    # Get filters id
    filter_group_id <- paste0("filter_row_", var)
    filter_input_id <- paste0("filter_input_", var)
    
    current_filters$groups[var] <- filter_group_id
    current_filters$inputs[var] <- filter_input_id
    
    if(is.numeric(df[[var]])){
      curr_input_labels <- c("Mínimo","Máximo")
      curr_input_ids <-  paste0(filter_input_id, "_", c("min","max"))
      current_filters$types[var] <- NUMERIC_TYPE
      
      min_val <- min(df[[var]], na.rm = TRUE)
      max_val <- max(df[[var]], na.rm = TRUE)
      
      if (is.null(current_filters$save[[var]])) {
        active_min <- min_val
        active_max <- max_val
      } else {
        active_min <- current_filters$min_val[[var]]
        active_max <- current_filters$max_val[[var]]
      }
      
      fluidRow(
        id=filter_group_id, 
        box(width = 12, title=filter_group, 
          splitLayout(
            numericInput( inputId = curr_input_ids[1], label = curr_input_labels[1],
                          value=active_min, min = min_val, max = max_val, width = "75%"),
            numericInput( inputId = curr_input_ids[2], label = curr_input_labels[2],
                          value=active_max, min = min_val, max = max_val, width = "75%")
          )
        )
      )
    }
    # Date Filter
    else if(is.date(df[[var]])){
      curr_input_labels <- c("Inicio","Fin")
      curr_input_ids <-  paste0(filter_input_id, "_", c("min","max"))
      current_filters$types[var] <- DATE_TYPE
      
      min_val <- min(df[[var]], na.rm = TRUE)
      max_val <- max(df[[var]], na.rm = TRUE)
      
      if (is.null(current_filters$save[[var]]) || current_filters$save[[var]] == FALSE) {
        active_min <- min_val
        active_max <- max_val
      } else {
        active_min <- current_filters$min_val[[var]]
        active_max <- current_filters$max_val[[var]]
      }
      
      fluidRow(
        id=filter_group_id,
        box(width = 12, title=filter_group, 
          splitLayout(
            dateInput( inputId = curr_input_ids[1], label = curr_input_labels[1],
                       value=active_min, min = min_val, max = max_val, width = "75%"),
            dateInput( inputId = curr_input_ids[2], label = curr_input_labels[2],
                       value=active_max, min = min_val, max = max_val, width = "75%")
          )
        )
      )
    }
    # Categoric Filter
    else {
      current_filters$types[var] <- CATEGORIC_TYPE
      current_unique_vals <- sort(unique(as.character(df[[var]])))
      
      if (is.null(current_filters$save[[var]])) {
        fluidRow(
          id=filter_group_id,
          box(width = 12, title=filter_group, 
            pickerInput(
              inputId = filter_input_id, label = NULL,
              choices = current_unique_vals, width = "75%",
              multiple = TRUE, options = pickerOptions(
                actionsBox = TRUE,
                noneSelectedText = "Ninguna opción seleccionada")
            )
          )
        )
      } else {
        active_factor <- current_filters$factor_val[[var]]
        fluidRow(
          id=filter_group_id,
          box(width = 12, title=filter_group, 
            pickerInput(
              inputId = filter_input_id, label = NULL,
              choices = current_unique_vals, width = "75%",
              selected = active_factor,
              multiple = TRUE, options = pickerOptions(
                actionsBox = TRUE,
                noneSelectedText = "Ninguna opción seleccionada")
            )
          )
        )
      }
    }
  })
})

observeEvent(input$filter_update_btn,{
  n_filters <- length(input$filter_variables_multi_input)
  remove_filters <- names(current_filters$groups)[!names(current_filters$groups) %in% input$filter_variables_multi_input]
  
  for (del_filter in remove_filters) {
    filter_group_id <- current_filters$groups[[del_filter]]
    shinyjs::hide(selector = paste0("#", filter_group_id), asis = TRUE)
  }
  
  if(n_filters > 0){
    shinyjs::show("filter_apply_btn")
    user_filterVar$push <- TRUE
    first_filter$push <- TRUE
  } else {
    shinyjs::hide("filter_apply_btn")
    active_filters(FALSE)
    user_filterVar$push <- FALSE
  }
})

observeEvent(input$filter_variables_multi_input,{
  n_filters <- length(input$filter_variables_multi_input)
  
  remove_filters <- names(current_filters$groups)[!names(current_filters$groups) %in% input$filter_variables_multi_input]
  
  for (del_filter in remove_filters) {
    filter_group_id <- current_filters$groups[[del_filter]]
    shinyjs::hide(selector = paste0("#", filter_group_id), asis = TRUE)
    
    current_filters$active[del_filter] <- NULL
    current_filters$save[del_filter] <- NULL
    current_filters$min_val[del_filter] <- NULL
    current_filters$max_val[del_filter] <- NULL
    current_filters$factor_val[del_filter] <- NULL
  }
  
  add_filters <- input$filter_variables_multi_input
  
  for (add_var in add_filters) {
    if (is.null(current_filters$active[[add_var]])) {
      filter_group_id <- current_filters$groups[[add_var]]
      print("ADD")
      print(filter_group_id)
      shinyjs::show(selector = paste0("#", filter_group_id), asis = TRUE)
      current_filters$active[add_var] <- TRUE
    }
  }
  
  if(n_filters > 0){
    shinyjs::show("filter_apply_btn")
    user_filterVar$push <- TRUE
    first_filter$push <- TRUE
    
  } else {
    df <- get_active_df()
    if (!is.null(df)) {
      filter_df(filter_not_validated_patients(df))
    }
    shinyjs::hide("filter_apply_btn")
    active_filters(FALSE)
    user_filterVar$push <- FALSE
    
    # Avisa de que se han eliminado todos los filtros, solo si se ha realizado algun filtro previamente
    if(first_filter$push){
      shinyalert("Aviso", "Actualmente el conjunto de datos no se encuentra filtrado", type = "info")
    }
  }
  
}, ignoreNULL = FALSE)

observeEvent(input$filter_apply_btn, {
  req(input$filter_variables_multi_input)
  
  df <- get_active_df()
  df <- filter_not_validated_patients(df)
  
  for(filter_var in input$filter_variables_multi_input){
    current_input <- current_filters$inputs[[filter_var]]
    current_type <- current_filters$types[[filter_var]]
    
    # Update vars.
    current_filters$save[[filter_var]] <- TRUE
    
    print("-- FILTER --")
    print(current_input); print(current_type)
    
    if(current_type == CATEGORIC_TYPE){
      filter_values <- input[[current_input]]
      current_filters$factor_val[[filter_var]] <- filter_values
      
      print(filter_values)
      print( dim(df[df[[filter_var]] %in% filter_values,]) )
      
      df <- df[df[[filter_var]] %in% filter_values,]
    } else {
      current_input_ids <-  paste0(current_input, "_", c("min","max"))
      min_val <- input[[current_input_ids[1]]]
      max_val <- input[[current_input_ids[2]]]
      
      current_filters$min_val[[filter_var]] <- min_val
      current_filters$max_val[[filter_var]] <- max_val
      
      if(min_val > max_val) { 
        min_val <- min_val + max_val
        max_val <- min_val - max_val
        min_val <- min_val - max_val
      }
      
      df <- df[(df[[filter_var]] >= min_val & df[[filter_var]] <= max_val),]
      
      # Remove all NA's
      df <- df[rowSums(is.na(df)) != ncol(df),]
    }
  }
  print(df)
  control <- TRUE
  
  if(nrow(df) == 0){
    # ALERT
    print("NO ROWS")
    shinyalert("Aviso", "¡El conjunto de datos se quedaría vacío!
               No se ha aplicado el último filtrado.", type = "warning")
    
    ### PROVISIONAL ###
    
    # Recarga el dataframe antes de que se hubiese aplicado el filtro que lo deja vacio (Puede estar filtrado)
    df <-  get_df()
    control <- FALSE
    
    # Si el dataframe no se quedase vacio, se actualiza los filtros activos que se muestran
  }else{
    # Show active filters with text
    filtros_activos <- paste(input$filter_variables_multi_input, collapse = ",")
    
    output$Active_Filters_Uni <- renderText({
      filtros_activos
    })
    output$Active_Filters_Bi <- renderText({
      filtros_activos
    })
    output$Active_Filters_Multi <- renderText({
      filtros_activos
    })
    output$Active_Filters_Surv <- renderText({
      filtros_activos
    })
  }
  
  filter_variable_list(sort(input$filter_variables_multi_input))
  active_filters(TRUE)
  filter_df(df)
  
  # Gestionar cuando ESTADO_SG tenga valores nulos
  if (length(unique(df$ESTADO_SG)) == 1) {
    shinyalert("Aviso", "Debido al filtrado realizado, todos los pacientes presentan el mismo valor para ESTADO_SG.
                \nSi intenta realizar un análisis de supervivencia antes de corregir este error, la aplicación se cerrará.
                \nPor favor, revise el filtrado realizado.", type = "warning")
    control <- FALSE
  }
  
  if (control) {
    # Actualiza a la pestaña visualize
    updateTabsetPanel(session, "selector", selected = "Ver datos")
  }
})