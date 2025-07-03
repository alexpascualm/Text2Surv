### --- CONSTANTS ---

month_abv <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
               "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
l_month_abv <- list("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                    "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")

NONE_SELECTED_CONST <- "Ninguna"

NUMERIC_TYPE <- "Numérica"
CATEGORIC_TYPE <- "Categórica"
DATE_TYPE <- "Fecha"

STUDY_TYPE_UNIVAR <- "univariate"
STUDY_TYPE_BIVAR <- "bivariate"
STUDY_TYPE_MULTIVAR <- "multivariate"
STUDY_TYPE_SURVIVAL <- "survival"

TIME_SCALE_DAY <- "Días"
TIME_SCALE_MONTH <- "Meses"
TIME_SCALE_YEAR <- "Años"

# SUMMARY_DF_COLNAMES_NUMERIC <- c("N", "Mean", "Standard Dev",
#                                  "Median", #"Median Absolute Dev",
#                                  "Min", "Max", "Range", "# NA")
                                 # "Skewness", "Kurtosis", "Std Error")
SUMMARY_DF_COLNAMES_NUMERIC <- c("N", "Media", "Desviacion Std",
                                 "Mediana", "Min", "Max", "Rango", "# NA")

SUMMARY_DF_COLNAMES_CATEGORIC <- c("Valores", "N", "Porcentaje (%)") 
#                                  "N Acumualda", "Porcentaje Acumulado (%)")

SUMMARY_DF_COLNAMES_CATEGORIC_NUMERIC <- c("Grupo", "N", "Media", "Desviacion Std", 
                                           "Mediana", "Min", "Max", "Rango")

