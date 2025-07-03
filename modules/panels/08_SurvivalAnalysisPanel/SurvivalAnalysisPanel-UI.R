source("modules/global.R", local = TRUE, encoding = "UTF-8")


analize.data.panel_Survival_UI <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h2("Análisis supervivencia "),
      conditionalPanel(
        condition = "output.fileUploaded",
        
       
        
        conditionalPanel('output.activeFilters', 
          hr(),
          h4("Filtros activos:"),
          
          textOutput("Active_Filters_Surv") %>%
                     helper(type = "inline",
                            title = "Variables con reglas de filtrado:",
                            content = c("Estas son las variables que tienen una regla de filtrado definida"),
                            size = "s") 
        ),
        
        hr(),
        
        #############################
        ### Survival Analysis Options
        #############################
        
        selectizeInput('survival_event', 'Variable con estado del paciente',
                       choices = NULL, multiple=FALSE) %>% 
          helper(type = "inline",
                 title = "Variable de Estudio (Evento)",
                 content = c("Acá aparecen las variables que tengan únicamente dos valores distintos."),
                 size = "s"),
        selectizeInput('survival_event_value', 'Valor que representa el evento',
                       choices = NULL, multiple=FALSE),
        materialSwitch("survival_switch_calculate_time","Calcular tiempo de seguimiento",
                       value = FALSE, status = "primary", right = TRUE)%>% 
          helper(type = "inline",
                 title = "Calcular tiempo de seguimiento",
                 content = c("Calcular el tiempo en meses entre la fecha inicial y la fecha de evento/censura. Verificar que la fecha esté en formato DD/MM/AAAA"),
                 size = "s"),
        conditionalPanel(
          condition = "!input.survival_switch_calculate_time",
          selectizeInput('survival_time', 'Tiempo de seguimiento',
                         choices = NULL, multiple=FALSE)
        ),
        conditionalPanel(
          condition = "input.survival_switch_calculate_time",
          pickerInput(inputId = "survival_time_scale", label = "Escala temporal", 
                      choices = c(TIME_SCALE_DAY, TIME_SCALE_MONTH, TIME_SCALE_YEAR)
          ),
          selectizeInput('survival_time_init', 'Fecha inicio',
                         choices = NULL, multiple=FALSE),
          selectizeInput('survival_time_end', 'Fecha evento',
                         choices = NULL, multiple=FALSE),
          selectizeInput('survival_time_censor', 'Fecha censurado',
                         choices = NULL, multiple=FALSE)
        ),
        selectizeInput('survival_strata', 'Variable de estratificación',
                       choices = NULL, multiple=FALSE),
        materialSwitch("switch_confidence_interval","Intervalo de confianza",
                       value = FALSE, status = "primary", right = TRUE),
        materialSwitch("switch_patient_risk", "Pacientes de riesgo",
                       value = FALSE, status = "primary", right = TRUE)
        
        , hr(), 
        div(
          style = "width:90%;text-align: center; margin-bottom: 10px",
          actionButton(
            inputId = "graphParamsButton_surv",
            label = "Parametros gráfica",
            style = "bordered", class = "btn-primary",
            icon = icon("cogs"), block = TRUE
          )
        )
      )
    ),
    
    mainPanel(
      useShinyjs(),
      conditionalPanel(
        condition = "output.fileUploaded",
        
        
        fluidRow(
          box(width = 7,
              h3("Supervivencia global (SG)"),
              withSpinner(plotOutput("survival_plot")),
              conditionalPanel(
                condition = "input.switch_patient_risk",
                withSpinner(plotOutput("risk_table", height=200))
              )
          ),
          box(width = 5,
              conditionalPanel(
                condition = "input.survival_strata != 'Ninguna'",
                h3("Comparación de curvas de supervivencia"),
                DTOutput('survival_comparison')
              ),
              h3("Descriptiva del tiempo de seguimiento"),
              DTOutput('survival_analysis')
          )
        )
      ),
      bsModal("modalGraphParam_surv", "Parámetros de las gráficas", "graphParamsButton_surv", #size="small",
              fluidRow(
                column(6,
                       radioGroupButtons(
                         inputId = "default_graph_param_x_label_surv",
                         label = "Eje X", justified = TRUE,
                         choices = c("Por Defecto", "Personalizado")
                       ),
                       style = "text-align: center; margin-bottom: 10px"
                ),
                column(6,
                       shinyjs::disabled(textInput("graph_param_x_label_surv", label = NULL)),
                       style = "text-align: center; margin-top: 25px"
                )
              ),
              fluidRow(
                column(6,
                       radioGroupButtons(
                         inputId = "default_graph_param_y_label_surv",
                         label = "Eje Y", justified = TRUE,
                         choices = c("Por Defecto", "Personalizado")
                       ),
                       style = "text-align: center; margin-bottom: 10px"
                ),
                column(6,
                       shinyjs::disabled(textInput("graph_param_y_label_surv", label = NULL)),
                       style = "text-align: center; margin-top: 25px"
                )
              ),
              fluidRow(
                column(6,
                       radioGroupButtons(
                         inputId = "default_graph_param_title_label_surv",
                         label = "Title", justified = TRUE,
                         choices = c("Por Defecto", "Personalizado")
                       ),
                       style = "text-align: center; margin-bottom: 10px"
                ),
                column(6,
                       shinyjs::disabled(textInput("graph_param_title_label_surv", label = NULL)),
                       style = "text-align: center; margin-top: 25px"
                )
              )
      )
    )
  ),
  
  includeScript("www/js/mainFunctions.js")
)
