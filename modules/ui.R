


ui <- shinyUI(
  fluidPage(
    # Authentication dashboard
    conditionalPanel(
      condition = "output.authorization == 0", # Cambiar a 0 para activar
      
      fluidRow(
        column(4, offset = 4,
               br(), br(), br(), br(),
               uiOutput("uiLogin"),
               uiOutput("pass")
        )
      )
    ),
    # Dashboard when access is granted
    conditionalPanel(
      condition = "output.authorization == 1", # Cambiar a 1 para activar
      
      # ------------------------
        navbarPage(
          ##  Set Window Title
          windowTitle = "ICB_AnalysisTool",
          #  Add logo and link it to the SEOM website.
          title =
            HTML(
              "Text2Surv"
            ),
        
          theme = shinytheme("cerulean"), # Modificar el color (De toda la app, provisional)
          header = tagList(
            
            tags$head(tags$style(HTML(".navbar {max-height: 35px;}"))) # Modificar el alto del navbar
          ),
         
          ##  Set ID
          id = "selector",
          ##  Set custom colour and collapse options.
          inverse = TRUE, collapsible = TRUE,
          
          ##  "About" tab.
          # tabPanel("Sobre nosotros",
          #          uiOutput('about.panel')),
          
          
          ##  "Fichero" tab.
          navbarMenu("Archivo",
                     tabPanel("Importar hoja de datos",
                              uiOutput('load.data.panel')),
                     tabPanel("Leer PDFs",
                              uiOutput('read.pdf.panel')),
                     # tabPanel("Importar hoja INDEF",
                     #          uiOutput("load.INDEF.data"))
          ),
          ## Row operations tab
          navbarMenu("Datos",
                     tabPanel("Ver datos",
                              uiOutput("visualize.data.panel")),
                     
                     # tabPanel("Validar datos",icon = icon("circle-exclamation"),
                     #          uiOutput("validate.data.panel")),
                     
                   

                     # tabPanel("Añadir/Borrar variables",
                     #          uiOutput('group.data.panel')),
                     tabPanel("Filtrar datos",
                              uiOutput('filter.data.panel')),
                     # tabPanel("Ocultar variables",
                     #          uiOutput('deselect.data.panel')),                  
          ),
          ## "Analize" tab.
          navbarMenu("Análisis",
                     tabPanel("Descriptiva",
                              uiOutput("Analize.data.panel_Descriptive")),
                     # tabPanel("Asociación/Correlación",
                     #          uiOutput("Analize.data.panel_Correlation")),
                     # tabPanel("Multivariante",
                     #         uiOutput("Analize.data.panel_Multivariant")),
                     tabPanel("Supervivencia",
                              uiOutput("Analize.data.panel_Survival"))
                   
          ),
          navbarMenu("Avanzado",
                     tabPanel("Clasificador Neoplasias",
                              uiOutput("Neoplasm.Classif.data.panel")),
                    
                     )
        )
    )
  )
)