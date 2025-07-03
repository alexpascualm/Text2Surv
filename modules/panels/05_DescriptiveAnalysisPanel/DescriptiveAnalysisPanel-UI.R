source("modules/global.R", local = TRUE, encoding = "UTF-8")

analize.data.panel_Descriptive_UI <- fluidPage(
  tags$head(tags$style("#container * {display: inline;}")),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h2("Análisis descriptiva"),
      conditionalPanel(
        condition = "output.fileUploaded",
        
        conditionalPanel('output.activeFilters', 
                         hr(),
                         h4("Filtros activos:"),
                         
                         textOutput("Active_Filters_Uni") %>%
                           helper(type = "inline",
                                  title = "Variables con reglas de filtrado:",
                                  content = c("Estas son las variables que tienen una regla de filtrado definida"),
                                  size = "s") 
        ),
        
        hr(),
        
        selectizeInput(
          'univariate_var', 'Variable de estudio',
          choices = NULL, multiple = FALSE
        ),
        hr(),
        div(
          style = "width:90%;text-align: center; margin-bottom: 10px",
          actionButton(
            inputId = "graphParamsButton_uni",
            label = "Parametros gráfica",
            style = "bordered", class = "btn-primary",
            icon = icon("cogs"), block = TRUE
          )
        )
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "output.fileUploaded",
        
        fluidRow(
          column(
            width = 7,
            h3("Gráfica descriptiva "),
            withSpinner(plotlyOutput("univariate_plot"))
          ),
          column(
            width = 5,
            div(id="container", h3("Total pacientes:"), uiOutput(outputId = "text")),
            DTOutput('univariate_analysis')
          )
        )
      ),
      bsModal("modalGraphParam_uni", "Parámetros de las gráficas", "graphParamsButton_uni", #size="small",
              fluidRow(
                column(6,
                  radioGroupButtons(
                    inputId = "default_graph_param_x_label_uni",
                    label = "Eje X", justified = TRUE,
                    choices = c("Por Defecto", "Personalizado")
                  ),
                  style = "text-align: center; margin-bottom: 10px"
                ),
                column(6,
                  shinyjs::disabled(textInput("graph_param_x_label_uni", label = NULL)),
                  style = "text-align: center; margin-top: 25px"
                )
              ),
              fluidRow(
                column(6,
                  radioGroupButtons(
                    inputId = "default_graph_param_y_label_uni",
                    label = "Eje Y", justified = TRUE,
                    choices = c("Por Defecto", "Personalizado")
                  ),
                  style = "text-align: center; margin-bottom: 10px"
                ),
                column(6,
                  shinyjs::disabled(textInput("graph_param_y_label_uni", label = NULL)),
                  style = "text-align: center; margin-top: 25px"
                )
              ),
              fluidRow(
                column(6,
                  radioGroupButtons(
                    inputId = "default_graph_param_title_label_uni",
                    label = "Title", justified = TRUE,
                    choices = c("Por Defecto", "Personalizado")
                  ),
                  style = "text-align: center; margin-bottom: 10px"
                ),
                column(6,
                  shinyjs::disabled(textInput("graph_param_title_label_uni", label = NULL)),
                  style = "text-align: center; margin-top: 25px"
                )
              )
      )
    )
  ),
  
  includeScript("www/js/mainFunctions.js")
)
