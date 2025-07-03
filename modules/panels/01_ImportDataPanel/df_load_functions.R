# Load the dataframe
load_df <- reactive({
  df <- if(is.null(active_df())) read_df() else active_df()
  
  if(is.null(df)){ return (NULL) }
  print("### LOADED DF ###"); print(dim(df))
  
  # Convert Empty Strings to NA
  df[df==""]<-NA
  
  # Clear Dataset
  colnames(df) <- toupper(make.unique(colnames(df), sep="_"))
  colnames(df) <- gsub("[()/º%]", "", colnames(df))
  colnames(df) <- gsub("[./º()]", "_", colnames(df))
  colnames(df) <- gsub("\\._", "_", colnames(df))
  colnames(df) <- gsub("__", "_", colnames(df))
  colnames(df) <- gsub("_$", "", colnames(df))
  colnames(df) <- stringi::stri_trans_general(str = colnames(df), id = "Latin-ASCII")
  
  varnames <- colnames(df)
  
  possible_date_cols <- varnames[sapply(df, is.convertible.to.date)]
  print(possible_date_cols)
  
  if(length(possible_date_cols) > 0){
    df[possible_date_cols] <- lapply(df[possible_date_cols], function(x) {
      as.Date(as.character(x), format = '%d/%m/%Y')
    })
  }
  
  numeric_cols <- varnames[sapply(df, is.numeric)]
  date_cols <- varnames[sapply(df, is.date)]
  categoric_cols <- varnames[!sapply(df, is.numeric)]
  
  categoric_cols <- categoric_cols[!categoric_cols %in% date_cols]
  
  print(categoric_cols); print(date_cols)
  # print(colnames(df))
  
  if(length(categoric_cols) >= 2){
    
    cat_all_unique_bool <- lengths(lapply(df[,categoric_cols], function(x) unique(x[!is.na(x)]))) == colSums(!is.na(df[,categoric_cols]))
    cat_all_unique_col <- names(cat_all_unique_bool)[cat_all_unique_bool]
    categoric_cols <- categoric_cols[!categoric_cols %in% cat_all_unique_col]
    
    many_vals_cols <- colnames(df)[ lengths(lapply(df, unique)) >= nrow(df)*95/100  ]
    print(many_vals_cols); print(categoric_cols[ categoric_cols %in% many_vals_cols ])
    categoric_cols <- categoric_cols[!categoric_cols %in% many_vals_cols]
    
  } else if (length(categoric_cols) == 1){
    if( length( unique( df[[categoric_cols]][ !is.na(df[[categoric_cols]]) ] ) ) == sum(!is.na(df[,categoric_cols])) )
      categoric_cols <- character(0)
  }
  
  if(length(categoric_cols) > 0){
    df[categoric_cols] <- lapply(df[categoric_cols], as.factor)
  }
  
  possible_event_cols <- colnames(df)[ lengths(lapply(df, function(x) {unique(na.omit(x))} )) == 2 ]
  
  #############################
  ### Update Global Variables
  global_possible_event_cols(sort(possible_event_cols))
  global_categoric_cols(sort(categoric_cols))
  global_numeric_cols(sort(numeric_cols))
  global_date_cols(sort(date_cols))
  
  #############################
  # Initialize Analysis variables
  read_variables_not_selected()
  initialize_univariate()
  initialize_bivariate()
  initialize_survival()
  choices_v(sort(c(numeric_cols, categoric_cols, date_cols)))
  
  #############################
  # Initialize Grouped variables
  if(is.null(grouped_variable_list()) && file.exists(paste0("data/", user_input$hospital, "/last_session_grouped_variable.rds"))) {
    aux_group_vars <- readRDS(paste0("data/", user_input$hospital, "/last_session_grouped_variable.rds"))
    grouped_variable_list(aux_group_vars)
  }
  
  update_variables('all')
  
  plot_variables$cox_fit <- NULL
  plot_variables$surv_summary_df <- NULL
  plot_variables$surv_risk_table <- NULL
  
  if (user_input$hospital == digest("h_leon")){
    # Change data frame class
    if ("NOMBRE" %in% colnames(df)) {
      df$NOMBRE <- as.character(df$NOMBRE)
    }
    if ("APELLIDO_1" %in% colnames(df)) {
      df$APELLIDO_1 <- as.character(df$APELLIDO_1)
    }
    if ("APELLIDO_2" %in% colnames(df)) {
      df$APELLIDO_2 <- as.character(df$APELLIDO_2)
    }
    if ("FECHA_ULTIMA_CITA" %in% colnames(df)) {
      df$FECHA_ULTIMA_CITA <-
        as.Date(df$FECHA_ULTIMA_CITA, format = "%Y-%m-%d")
    }
    if ("FECHA_PROXIMA_CITA" %in% colnames(df)) {
      df$FECHA_PROXIMA_CITA <-
        as.Date(df$FECHA_PROXIMA_CITA, format = "%Y-%m-%d")
    }
    if ("COMENTARIOS" %in% colnames(df)) {
      df$COMENTARIOS <- as.character(df$COMENTARIOS)
    }
  }
  
  # Borramos las filas que no aporten valor (menos de 6 columnas sin valor nulo)
  df <- df[rowSums(!is.na(df)) >= 6, ]
  
  active_df(df)
  
  return( df )
})

# Get the active dataframe or load one
get_active_df <- reactive({
  print("### GET ACTIVE DF ###")
  
  df <- if (is.null(active_df()) || is.null(choices_v())) load_df() else active_df()
  
  if( is.null(active_df()) ){
    print("LOAD DF"); print(dim(df))
  } else {
    print("ACTIVE DF"); print(dim(df))
  }
  
  return(df)
})

# Get the active dataframe or the filtered one
get_df <- reactive({
  print("### GET DF ###")
  
  df <- if (is.null(filter_df()) || is.null(choices_v())) get_active_df() else filter_df()
  
  if( is.null(filter_df()) ){
    print("GET ACTIVE DF"); print(dim(df))
  } else {
    print("FILTER DF"); print(dim(df))
  }
  
  # Filter possible "Sin-validar" patients
  df <- filter_not_validated_patients(df)
  
  return( df )
})



# Save and Load session dataframe
save_df <- reactive({
  df <- get_active_df()
  
  nombre_carpeta <- paste0("./data/", user_input$hospital)
  
  
  if (!file.exists(nombre_carpeta)) {
   
    # Si no existe, crea la carpeta
    dir.create(nombre_carpeta)
   
  }
  
  # Save a single object to a file
  saveRDS(df, paste0(nombre_carpeta,"/last_session.rds"))
})



load_RData_df <- reactive({
  print("Reading RData")
  if (file.exists(paste0("data/", user_input$hospital, "/last_session.rds"))) {
    # Restore a single object
    df <- readRDS(paste0("data/", user_input$hospital, "/last_session.rds"))
    
    file_uploaded(TRUE)
    active_df(df)
    filter_df(filter_not_validated_patients(df))
    
    if(("ESTADO_PDF" %in% names(df)) ){
      if(nrow(df[df$ESTADO_PDF %in% c('Sin validar - Antiguo','Sin validar - Nuevo'),])!=0){
        conflict_data(1)
      }
    }
  }
})