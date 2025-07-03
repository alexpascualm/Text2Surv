process_data_leon <- function(df) {
  aux_status <- c()
  
  for (i in 1:nrow(df)) {
    # for-loop over rows
    if ("DCO" %in% colnames(df)) {
      if (grepl(".-", df$DCO[i], fixed = TRUE)) {
        end <- unlist(gregexpr('.-', df$DCO[i]))[1] + 3
        df$DCO[i] <- str_sub(df$DCO[i], end,-1)
      } else {
        df$DCO[i] <- str_sub(df$DCO[i], 9,-1)
      }
    }
    
    if ("TIPO_ALTA" %in% colnames(df)) {
      aux_s <- 0
      if (!is.na(df$TIPO_ALTA[i])) {
        if (df$TIPO_ALTA[i] == "EXITUS") {
          aux_s <- 1
        }
      }
      aux_status <- append(aux_status, aux_s)
    }
    
    if ("TIEMPO_ESPERA" %in% colnames(df)) {
      aux_numbers <- as.numeric(unlist(regmatches(
        df$TIEMPO_ESPERA[i],
        gregexpr("[[:digit:]]+", df$TIEMPO_ESPERA[i])
      )))
      if (length(aux_numbers) == 3) {
        df$TIEMPO_ESPERA[i] <-
          aux_numbers[1] * 365 + aux_numbers[2] * 30 + aux_numbers[3]
      } else {
        df$TIEMPO_ESPERA[i] <- 0
      }
    }
    
    if ("TIEMPO_DEMORA" %in% colnames(df)) {
      df$TIEMPO_DEMORA <- as.numeric(gsub("\\D", "", df$TIEMPO_DEMORA))
    }
    
    if ("SUPERVIVENCIA" %in% colnames(df)) {
      aux_numbers <- as.numeric(unlist(regmatches(
        df$SUPERVIVENCIA[i],
        gregexpr("[[:digit:]]+", df$SUPERVIVENCIA[i])
      )))
      if (length(aux_numbers) == 3) {
        df$TIEMPO_SG[i] <-
          aux_numbers[1] * 365 + aux_numbers[2] * 30 + aux_numbers[3]
      } else {
        df$TIEMPO_SG[i] <- 0
      }
    }
  }
  
  if ("TIPO_ALTA" %in% colnames(df)) {
    df$ESTADO_SG <- aux_status
    df$ESTADO_SG <- as.character(df$ESTADO_SG)
  }
  if ("TIEMPO_ESPERA" %in% colnames(df)) {
    df$TIEMPO_ESPERA <- as.numeric(df$TIEMPO_ESPERA)
  }
  if ("SUPERVIVENCIA" %in% colnames(df)) {
    df$TIEMPO_SG <- as.numeric(df$TIEMPO_SG)
    df$SUPERVIVENCIA <- NULL
  }
  
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
  
  return(df)
}