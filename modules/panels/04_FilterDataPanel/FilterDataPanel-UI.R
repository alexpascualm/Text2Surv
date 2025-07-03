filter.data.panel_UI <- fluidPage(
  shinyjs::useShinyjs(),
  conditionalPanel(
    condition = "output.fileUploaded",
    
    fluidRow(
      box(width = 12,
        box(width = 4,
            # "Filter"
            pickerInput(
              inputId = "filter_variables_multi_input",
              label = "Variables a filtrar:",
              choices = NULL,
              multiple = TRUE, options = pickerOptions(
                actionsBox = TRUE,
                noneSelectedText = "Ninguna opción seleccionada")
            ),
            br(),
            actionButton("filter_update_btn", "Actualizar filtros",
                         class="btn-primary",
                         icon=icon("gears")),
            br(),
            br(),
            HTML("<b>Nota informativa</b>. Al principio se muestran todas las variables a modo de información.
                 Por favor, <b>pulse en primer lugar el botón 'Actualizar filtros'</b>. Si en alguna ocasión también se
                 muestran filtros que no han sido seleccionados, por favor, pulse este botón de nuevo."),
        ),
        box(width = 8,
            uiOutput('optional'),
            actionButton("filter_apply_btn", "Aplicar filtros",
                       class="byn-primary",
                       icon=icon("filter")),
            uiOutput('filter.variables.panel')
        )
      )
    )
  )
)