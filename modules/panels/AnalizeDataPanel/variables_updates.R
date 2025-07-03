# Function to get reactive input names
getInputs <- function(pattern){
  reactives <- names(reactiveValuesToList(input))
  reactives[grep(pattern,reactives)]
}

update_choices_v <- function() {
  choices_v(sort(c(global_numeric_cols(), global_categoric_cols(), global_date_cols())))
}

update_variables <- function(functionality_type) {
  if (file_uploaded()) {
    
    #############################
    ### Update Filter Variables Modal
    if (functionality_type == 'filter' ||  functionality_type == 'all') {
      updatePickerInput(session, "filter_variables_multi_input", choices = sort(choices_v()),
                          selected = filter_variable_list())
    }
    
    #############################
    ### Update Create Variable Modal
    else if (functionality_type == 'group' ||  functionality_type == 'all') {
      updateSelectizeInput(session, 'new_var_base', choices = sort(choices_v()),
                           server = FALSE, selected = choices_v()[[1]])
    }
    
    else if (functionality_type == 'var_list' ||  functionality_type == 'all') {
      updateSelectizeInput(session, "new_var_list", choices = sort(grouped_variable_list()))
    }
    
    #############################
    ## Univariate Analysis Options
    else if (functionality_type == 'univariate' ||  functionality_type == 'all') {
      updateSelectizeInput(session, 'univariate_var', choices = uni_var_choices(),
                           server = FALSE, selected = 'SEXO')
    }
    
    #############################
    ### Bivariate Analysis Options
    else if (functionality_type == 'bivariate' ||  functionality_type == 'all') {
      aux_status <- NONE_SELECTED_CONST
      # Checks if a column named ESTADO_SG is present in the data set.
      if ('ESTADO_SG' %in% bi_var_choices()$Categóricas) {
        aux_status <- 'ESTADO_SG'
      }
      
      updateSelectizeInput(session, 'bivariate_ind', choices = bi_var_choices(),
                           server = FALSE, selected = NONE_SELECTED_CONST)
      updateSelectizeInput(session, 'bivariate_dep', choices = bi_var_choices(),
                           server = FALSE, selected = aux_status)
    }
    
    #############################
    ### Multivariate Analysis Options
    else if (functionality_type == 'multivar' ||  functionality_type == 'all') {
      if(length(global_possible_event_cols()) > 0){
        aux_status <- NONE_SELECTED_CONST
        aux_superv <- NONE_SELECTED_CONST
        
        # Checks if a column named ESTADO_SG is present in the data set.
        if ('ESTADO_SG' %in% global_possible_event_cols()) {
          aux_status <- 'ESTADO_SG'
        }
        # Checks if a column named TIEMPO_SG is present in the data set.
        if ('TIEMPO_SG' %in% global_numeric_cols()) {
          aux_superv <- 'TIEMPO_SG'
        }
        
        updateSelectizeInput(session, 'multivar_dep', choices = global_possible_event_cols(),
                             server = FALSE, selected = aux_status)
        updateSelectizeInput(session, 'multivar_ind', choices = bi_var_choices(),
                             server = FALSE, selected = NULL)
        updateSelectizeInput(session, 'multivar_time', choices = global_numeric_cols(),
                             server = FALSE, selected = aux_superv)
      }
      ### Calculate Time Selectize Options
      if(length(global_date_cols()) >= 2){
        updateSelectizeInput(session, 'multivar_time_init', choices = global_date_cols(),
                             server = FALSE, selected = global_date_cols()[[1]])
        updateSelectizeInput(session, 'multivar_time_end', choices = global_date_cols(),
                             server = FALSE, selected = global_date_cols()[[2]])
        updateSelectizeInput(session, 'multivar_time_censor', choices = global_date_cols(),
                             server = FALSE, selected = global_date_cols()[[2]])
        shinyjs::show("multivar_switch_calculate_time")
      } else {
        shinyjs::hide("multivar_switch_calculate_time")
      }
    }
    
    #############################
    ### Survival Analysis Options
    else if (functionality_type == 'survival' ||  functionality_type == 'all') {
      if(length(global_possible_event_cols()) > 0){
        aux_status <- NONE_SELECTED_CONST
        aux_superv <- NONE_SELECTED_CONST
        
        # Checks if a column named ESTADO_SG is present in the data set.
        if (user_input$hospital == digest("h_leon")){
          aux_status <- 'ESTADO_SG'
          aux_superv <- 'TIEMPO_SG'
          list_surv_time <- surv_numeric_cols()
          list_surv_event <- surv_event_cols()
          list_surv_strata <- surv_strata_cols()
        }
        else {
          list_surv_time <- surv_numeric_cols()
          list_surv_event <- surv_event_cols()
          list_surv_strata <- surv_strata_cols()
          
          if ('ESTADO_SG' %in% list_surv_strata) {
            aux_status <- 'ESTADO_SG'
            list_surv_strata <- list_surv_strata[list_surv_strata != "ESTADO_SG"]
          }
          # Checks if a column named TIEMPO_SG is present in the data set.
          if ('TIEMPO_SG' %in% list_surv_time) {
            aux_superv <- 'TIEMPO_SG'
          }
        }
        
        updateSelectizeInput(session, 'survival_event', choices = c(NONE_SELECTED_CONST, list_surv_event),
                             server = FALSE, selected = aux_status)
        updateSelectizeInput(session, 'survival_time', choices = c(NONE_SELECTED_CONST, list_surv_time),
                             server = FALSE, selected = aux_superv)
        updateSelectizeInput(session, 'survival_strata', choices = c(NONE_SELECTED_CONST, list_surv_strata),
                             server = FALSE, selected = NONE_SELECTED_CONST)
      }
      ### Calculate Time Selectize Options
      if(length(global_date_cols()) >= 2){
        updateSelectizeInput(session, 'survival_time_init', choices = global_date_cols(),
                             server = FALSE, selected = global_date_cols()[[1]])
        updateSelectizeInput(session, 'survival_time_end', choices = global_date_cols(),
                             server = FALSE, selected = global_date_cols()[[2]])
        updateSelectizeInput(session, 'survival_time_censor', choices = global_date_cols(),
                             server = FALSE, selected = global_date_cols()[[2]])
        shinyjs::show("survival_switch_calculate_time")
      }else {
        shinyjs::hide("survival_switch_calculate_time")
      }
    } else {
      print("ERROR AT UPDATE VARIABLES")
    }
  }
}