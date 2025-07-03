source("modules/global.R", local = TRUE, encoding = "UTF-8")

### --- SERVER ---
server <- function(input, output, session) {
  desc = read.dcf("www/DESCRIPTION")
  values.lite.version = desc[, "Version"][[1]]
  
  get.lite.version = reactive({
    values.lite.version
  })
  
  ##----------------------##
  ##   0. "Login" Module  ##
  ##----------------------##
  # Funciones de server dedicadas al login de la app
  source("modules/panels/login-SERVER.R", local = TRUE, encoding = "UTF-8")
  

  
  ### --- GENERAL
  observe_helpers()
  
  file_uploaded <- reactiveVal(FALSE)
  bivar_summary <- reactiveVal(FALSE)
  new_var_create <- reactiveVal(FALSE)
  active_filters <- reactiveVal(FALSE)
  conflict_data <- reactiveVal(FALSE)
  
  # Variables for filter and group variables
  choices_v <- reactiveVal(NULL)
  grouped_variable_list <- reactiveVal(NULL)
  filter_variable_list <- reactiveVal(NULL)
  
  # Variables for analysis
  uni_var_choices <- reactiveVal(NULL)
  bi_var_choices <- reactiveVal(NULL)
  surv_strata_cols <- reactiveVal(NULL)
  surv_numeric_cols <- reactiveVal(NULL)
  surv_event_cols <- reactiveVal(NULL)
  
  # Variables to "hide" from analysis
  global_hidden_vars <- reactiveVal(FALSE)
  global_univariate_categoric_not <- reactiveVal(NULL)
  global_univariate_numeric_not <- reactiveVal(NULL)
  global_univariate_dates_not <- reactiveVal(NULL)
  global_bivariate_category_not <- reactiveVal(NULL)
  global_bivariate_numeric_not <- reactiveVal(NULL)
  global_survival_status_not <- reactiveVal(NULL)
  global_survival_time_not <- reactiveVal(NULL)
  global_survival_strat_not <- reactiveVal(NULL)
  
  # Global df variables
  global_possible_event_cols <- reactiveVal(NULL)
  global_categoric_cols <- reactiveVal(NULL)
  global_numeric_cols <- reactiveVal(NULL)
  global_date_cols <- reactiveVal(NULL)
  
  study_type_ids <- c(STUDY_TYPE_UNIVAR, STUDY_TYPE_SURVIVAL)
  study_type_names <- c("Descriptiva", "Supervivencia")     
  study_type_choices <- setNames(study_type_ids, study_type_names)
  
  vartype_choices <- c( CATEGORIC_TYPE, NUMERIC_TYPE, DATE_TYPE ) #, "Texto")
  
  currentVarTypes <- reactiveValues(uni = NULL, bi_dep = NULL, bi_ind = NULL,
                                    surv_time = NULL, surv_date_ini = NULL, 
                                    surv_date_event = NULL, surv_date_censor = NULL,
                                    surv_event = NULL, surv_censor = NULL)
  
  updateSelectizeInput(session, 'current_study', choices = study_type_choices,
                       server = FALSE, selected = NULL)
  
  plot_variables <- reactiveValues(cox_fit = NULL, surv_summary_df = NULL, 
                                   surv_risk_table = NULL, surv_log_rank = NULL)
  
  global_plot_theme <- theme_bw()
  global_plot_theme$panel.background <- element_rect(fill = "transparent",colour = "red");
  global_plot_theme$plot.background <-  element_rect(fill = "transparent",colour = NA);
  global_plot_theme$legend.background <- element_blank()
  
  ### --- REACTIVE DATASET
  # 
  observeEvent(input$file, {
    # shinyjs::show("current_study")
    file_uploaded(TRUE)
    active_df(NULL)
    read_df_check()
    filter_variable_list(NULL)
  })
  
  output$fileUploaded <- reactive({ file_uploaded() })
  output$bivarSummary <- reactive({ bivar_summary() })
  output$newVarCreate <- reactive({ new_var_create() })
  output$activeFilters <- reactive({ active_filters() })
  output$conflictData <- reactive({ conflict_data() })
  
  outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)
  outputOptions(output, "bivarSummary", suspendWhenHidden = FALSE)
  outputOptions(output, "newVarCreate", suspendWhenHidden = FALSE)
  outputOptions(output, "activeFilters", suspendWhenHidden = FALSE)
  outputOptions(output, "conflictData", suspendWhenHidden = FALSE)
  
  ##### Solo para prueba
  conflict_data(0)
  #####
  
  active_df <- reactiveVal(NULL)
  filter_df <- reactiveVal(NULL)
  conflict_df <- reactiveVal(NULL)
  
  #######################
  ### READ/LOAD/SAVE DATAFRAME
  #######################
  
  source("modules/panels/01_ImportDataPanel/ProcesamientoLeon.R", local = TRUE, encoding = "UTF-8")
  
  # Read the dataframe
  source("modules/panels/01_ImportDataPanel/df_read_functions.R", local = TRUE, encoding = "UTF-8")
  
  # Get active dataframe, load one and save/restore last session
  source("modules/panels/01_ImportDataPanel/df_load_functions.R", local = TRUE, encoding = "UTF-8")
  
  #######################
  ### ANALIZE EVENTS
  #######################
  
  # Read and Save variables
  source("modules/panels/AnalizeDataPanel/variables_read_save.R", local = TRUE, encoding = "UTF-8")
  
  # Initialize variables
  source("modules/panels/AnalizeDataPanel/variables_initialize.R", local = TRUE, encoding = "UTF-8")
  
  # Update selectizeInputs
  source("modules/panels/AnalizeDataPanel/variables_updates.R", local = TRUE, encoding = "UTF-8")
  
  # Variable observe events
  source("modules/panels/AnalizeDataPanel/variables_observeEvents.R", local = TRUE, encoding = "UTF-8")
  
  # Graph parameters
  source("modules/panels/AnalizeDataPanel/parameters_observeEvents.R", local = TRUE, encoding = "UTF-8")
  
  #######################
  ### DATA MANIPULATION
  #######################
  
  # Import Dataset
  source("modules/panels/01_ImportDataPanel/importDataPanel-UI.R", local = TRUE, encoding = "UTF-8")
  source("modules/panels/01_ImportDataPanel/importDataPanel-SERVER.R", local = TRUE, encoding = "UTF-8")
  
  
  source("modules/panels/01_ImportDataPanel/01.1_PDFsDataPanel/PDFsDataPanel-UI.R", local = TRUE, encoding = "UTF-8")
  source("modules/panels/01_ImportDataPanel/01.1_PDFsDataPanel/PDFsDataPanel-SERVER.R", local = TRUE, encoding = "UTF-8")

  
  # Visualize Dataset
  source("modules/panels/02_VisualizeDataPanel/VisualizeDataPanel-UI.R", local = TRUE, encoding = "UTF-8")
  source("modules/panels/02_VisualizeDataPanel/VisualizeDataPanel-SERVER.R", local = TRUE, encoding = "UTF-8")
  

  # Filter Dataset
  source("modules/panels/04_FilterDataPanel/FilterDataPanel-UI.R", local = TRUE, encoding = "UTF-8")
  source("modules/panels/04_FilterDataPanel/FilterDataPanel-SERVER.R", local = TRUE, encoding = "UTF-8")
  

  
  #######################
  ### ANALYSIS EXPLORE
  #######################
  
  # Descriptive analysis
  source("modules/panels/05_DescriptiveAnalysisPanel/DescriptiveAnalysisPanel-UI.R", local = TRUE, encoding = "UTF-8")
  source("modules/panels/05_DescriptiveAnalysisPanel/DescriptiveAnalysisPanel-SERVER.R", local = TRUE, encoding = "UTF-8")
  
  
  
  # Survival analysis
  source("modules/panels/08_SurvivalAnalysisPanel/SurvivalAnalysisPanel-UI.R", local = TRUE, encoding = "UTF-8")
  source("modules/panels/08_SurvivalAnalysisPanel/SurvivalAnalysisPanel-SERVER.R", local = TRUE, encoding = "UTF-8")
  
  #######################
  ### ANALYSIS EXPLORE
  #######################
  
  # Neoplasm Classification
  source("modules/panels/09_NeoplasmClassifPanel/NeoplasmClassifPanel-UI.R", local = TRUE, encoding = "UTF-8")
  source("modules/panels/09_NeoplasmClassifPanel/NeoplasmClassifPanel-SERVER.R", local = TRUE, encoding = "UTF-8")
  
}

