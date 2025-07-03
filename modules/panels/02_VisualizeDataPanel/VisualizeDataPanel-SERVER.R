output$visualize.data.panel <- renderUI({
  visualize.data.panel_UI
})

#############################

### View Dataset

output$current_dataframe <- renderReactable({
  req(get_df())
  
  df <-  get_df()

  if(!("ESTADO_PDF" %in% names(df)) ){
    reactable(
      data = df,
      rownames = FALSE,
      compact = TRUE,
      # for minimum row height
      filterable = FALSE,
      # for individual column filters
      striped = TRUE,
      # banded rows
      resizable = TRUE,
      # for resizable column widths
      sortable = TRUE,
      searchable = TRUE,
      defaultPageSize = 10,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(1, 5, 10, 25, 50, 100, 250),
      highlight = TRUE,
      outlined = FALSE,
      #defaultSorted = c("NHC_CAULE"),
      onClick = "select",
      selection = "single"
    )
    
  }else{
    reactable(
      data = df,
      rownames = FALSE,
      compact = TRUE,
      # for minimum row height
      filterable = FALSE,
      # for individual column filters
      striped = TRUE,
      # banded rows
      resizable = TRUE,
      # for resizable column widths
      sortable = TRUE,
      searchable = TRUE,
      defaultPageSize = 10,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(1, 5, 10, 25, 50, 100, 250),
      highlight = TRUE,
      outlined = FALSE,
      onClick = "select",
      selection = "single",
      columns = list(
        ESTADO_PDF = colDef(
          style =  JS(
            "function(rowInfo) {
                                if (rowInfo.values['ESTADO_PDF'] == 'Consolidado') {
                                  return { backgroundColor: 'green', color: 'white', fontWeight: 600}
                                }else if (rowInfo.values['ESTADO_PDF'] == 'Nuevo') {
                                  return { backgroundColor: 'blue', color: 'white', fontWeight: 600 }
                                }else if (rowInfo.values['ESTADO_PDF'] == 'Sin validar - Antiguo') {
                                  return { backgroundColor: 'orange', color: 'white', fontWeight: 600 }
                                }else if (rowInfo.values['ESTADO_PDF'] == 'Sin validar - Nuevo') {
                                  return { backgroundColor: 'orange', color: 'white', fontWeight: 600 }
                                } else {
                                  return { backgroundColor: 'grey', color: 'white', fontWeight: 600 }
                                }
                              }"
          )
        )
      ),
      defaultSorted = c("ESTADO_PDF")#, "NHC_CAULE")
    )
  }
 
 
})

#### Modify Table

current_modify_params <- reactiveValues(base_var=NULL, base_var_type = NULL, input_ids=NULL)

selected <- reactiveValues(modify=NULL)
observe({
  selected$modify <- getReactableState("current_dataframe", "selected")
})

observeEvent(input$modifyTable, {
  showModal(
    modalDialog(
      title = 'Modificar información del paciente',
      uiOutput("modify_bttn"),
      footer = div(
        actionButton("modify_save_bttn", "Guardar", class = "btn-primary"),
        modalButton('Salir')
      )
    )
  )
})

output$modify_bttn <- renderUI({
  req(get_df())
  
  if (is.null(selected$modify)) {
    fluidRow(
      column(11, align="left",
             HTML("<b>Ningún paciente seleccionado.</b>"),
             HTML("Por favor, seleccione un paciente pinchando en el botón radial que se encuentra al inicio de cada entrada de la tabla.")
      )
    )
  } else {
    df <-  get_df()
    df_selection <- df[selected$modify, ]
    df_selection <- subset(df_selection, select = -which(names(df_selection) %in% c("ESTADO_PDF", "N_ORDEN", "NHC_CAULE")))
    
    # Only for León Hospital, because ESTADO_SG is calculated on the basis of other variable.
    if (user_input$hospital == digest("h_leon")){
      df_selection <- subset(df_selection, select = -which(names(df_selection) %in% c("ESTADO_SG")))
    }
    
    columns_types <- lapply(df_selection, class)
    n_groups <- as.numeric(length(colnames(df_selection)))
    group_ids <- colnames(df_selection)
    
    input_group_ids <- paste0("input_new_var_group_", LETTERS[1:n_groups]) # GROUP INPUT ID (BASE)
    input_group_names <- paste0(input_group_ids, "_name") # GROUP NAME
    
    current_modify_params$base_var <- group_ids
    current_modify_params$base_var_type <- columns_types
    current_modify_params$input_ids <- input_group_ids
    
    l_groups <- lapply(1:n_groups, function(i){
      if (columns_types[i] == "numeric") {
        fluidRow(
          column(10, offset = 1, align="left",
                 numericInput( inputId = input_group_ids[i], label = group_ids[i], 
                              value = df_selection[1, group_ids[i]], width = "100%")
          )
        )
      } else if (columns_types[i] == "Date") {
        fluidRow(
          column(10, offset = 1, align="left",
                 dateInput( inputId = input_group_ids[i], label = group_ids[i], 
                               value = df_selection[1, group_ids[i]], width = "100%")
          )
        )
      } else if (columns_types[i] == "factor") {
        current_values <- sort(get_active_df()[[group_ids[i]]])
        values_to_group <- unique(as.character(current_values))
        
        # Empty factor, is NA
        if (is.na(df_selection[group_ids[i]])) {
          fluidRow(
            column(10, offset = 1, align="left",
                   pickerInput(
                     inputId = input_group_ids[i], label = group_ids[i],
                     choices = values_to_group, width = "100%",
                     multiple = TRUE,
                     selected = df_selection[1, group_ids[i]],
                     options = pickerOptions(
                       maxOptions = 1,
                       noneSelectedText = "Ninguna opción seleccionada")
                   )
            )
          )
        } else {
          fluidRow(
            column(10, offset = 1, align="left",
              pickerInput(
                inputId = input_group_ids[i], label = group_ids[i],
                choices = values_to_group, width = "100%",
                multiple = FALSE, 
                selected = df_selection[1, group_ids[i]]
              )
            )
          )
        }
      } else {
        fluidRow(
          column(10, offset = 1, align="left",
                 textInput( inputId = input_group_ids[i], label = group_ids[i], 
                            value = df_selection[1, group_ids[i]], width = "100%")
          )
        )
      }
    })
  }
})

### Save modifications


observeEvent(input$modify_save_bttn, {
  df <-  get_df()
  new_row <- df[selected$modify, ]
  
  # Get current_modify_params as a list
  modify_params <- reactiveValuesToList(current_modify_params)
  
  # If its not an empty patient
  if (!is.null(current_modify_params$base_var)) {
    for (i in 1:length(current_modify_params$base_var)) {
      base_var <- modify_params$base_var[i]
      current_input <- modify_params$input_ids[i]
      new_value <- input[[current_input]]
      
      # Null variable is not allowed, unless it is the original value
      if (!is.null(new_value)) {
        new_row[base_var] <- new_value
        
        # Only for León Hospital, because ESTADO_SG is calculated on the basis of other variable.
        if (user_input$hospital == digest("h_leon")){
          if (base_var == "TIPO_ALTA") {
            if (new_value == "EXITUS") {
              new_row["ESTADO_SG"] <- 1
            } else {
              new_row["ESTADO_SG"] <- 0
            }
          }
        }
      }
    }
  }
  
  # Save new row in filter_df
  df[selected$modify, ] <- new_row
  filter_df(df)
  
  # Get and save df in active_df
  df <-  get_active_df()
  df[which(df$NHC_CAULE == new_row[1, "NHC_CAULE"]), ] <- new_row
  active_df(df)
  filter_df(filter_not_validated_patients(df))
  save_df()
  
  # Close modal
  removeModal()
})

### Download table

output$downloadDataExcel <- downloadHandler(
  filename = function() {
    "excel_data.xlsx"
  },
  content = function(file) {
    write.xlsx(get_df(), file, row.names = FALSE)
  }
)

# Download txt for INDEF

output$downloadDataINDEF <- downloadHandler(
  
  
  filename = function() {
    paste("Solicitud_INDEF.txt")
  },
  content = function(file) {
    df <- get_df()
    
    #ifelse(is.na(df$SEXO),"",ifelse(df$SEXO=="M"))
    
    fecha_nac_str <- paste(as.integer(format(df$FECHA_NAC, "%d")), as.integer(format(df$FECHA_NAC, "%m")), as.integer(format(df$FECHA_NAC, "%Y")), sep = "|")
    
    text <- paste(df$NOMBRE,df$APELLIDO_1,df$APELLIDO_2,df$SEXO,fecha_nac_str,ifelse(is.na(df$DNI),"",df$DNI),ifelse(is.na(df$DNI),"|","D|"),sep="|")

    
    writeLines(paste(text, collapse = "\n"), file)
  }
)