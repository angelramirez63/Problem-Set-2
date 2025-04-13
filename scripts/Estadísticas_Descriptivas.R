#Estadísticas Descriptivas  ----------------------------------------------------
## Juan Pablo

setwd("~/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/stores")

#-------------------------------------------------------------------------------

rm(list = ls())

install.packages("xtable") #Exportar resultados a LaTEX
install.packages("knitr") #Para tablas categóricas

if(!require(pacman)) install.packages("pacman") ; require(pacman)


p_load(tidyverse,
       here,
       skimr,
       VIM,
       glmnet,    # Modelos de regresión regularizados (EN, Lasso y Ridge).
       caret,
       MLmetrics, # Calcular metricas
       MLeval,    # Evaluar modelos de clasificación
       Metrics, 
       ggplot2,
       dplyr,
       ranger,
       rio,
       xtable, 
       knitr
)

## Cargar datos ----------------------------------------------------------------

db <- readRDS('db_final.rds') %>% 
  as_tibble()

# Modelo -----------------------------------------------------------------------


#Crear base train

trainRF <- db %>% 
  filter(test == 0) %>% 
  select(-test) #Dejar por fuera la variable indicadora que clasifica a los datos como entrenamiento o prueba.


# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(trainRF))
missing_tab<-data.frame(
  Miss_val=missing_values
)
print(missing_tab)


#Convertir variables dicótomas en factores. Las variables de conteo y proporción no son convertidas a factor.

trainRF <- trainRF %>% 
  mutate(
    Pobre = factor(Pobre, levels = c(1,0), labels = c("Si", "No")),  #Dejar pobre como el primer nivel 
    cabecera = factor(cabecera, levels = c(1,2), labels = c("Cabecera", "Resto")), 
    prop_vivienda = factor(prop_vivienda, levels = c(1,2,3,4,5,6), 
                           labels = c("Propia_pagada", "Propia_pagando", 
                                      "Arriendo", "Usufructo", "Sin_titulo", "Otra")), 
    Dominio = as.factor(Dominio),
    Depto = factor(Depto, 
                   levels = c(5, 8, 11, 13, 15, 17, 18, 19, 20, 23, 25, 27, 41, 44, 47, 50, 
                              52, 54, 63, 66, 68, 70, 73, 76), 
                   labels = c("Antioquia", "Atlántico", "Bogotá", 
                              "Bolívar", "Boyacá", "Caldas", 
                              "Caquetá", "Cauca", "Cesar", "Córdoba",
                              "Cundinamarca", "Chocó", "Huila", 
                              "La Guajira", "Magdalena", "Meta", 
                              "Nariño", "Norte de Santander", 
                              "Quindío", "Risaralda", "Santander", 
                              "Sucre", "Tolima", "Valle del Cauca"))
  )


trainRF <- trainRF %>% mutate(
  p_cotiza_pension = ifelse(is.na(p_cotiza_pension == T), 0, p_cotiza_pension), 
  pensionado = ifelse(is.na(pensionado == T), 0, pensionado), 
  t_cotiza_pension = ifelse(is.na(t_cotiza_pension == T), 0, t_cotiza_pension)
)

#Seleccionar variables MÁS RELEVANTES en uso

vars <- trainRF %>% dplyr::select(Pobre,Dominio, Ncuartos, prop_vivienda, 
                             arriendo_hipotetico,
                             arriendo, Npersonas, Nper_unidad_gasto, 
                             linea_indigencia, linea_pobreza, factor_exp,
                             Depto, factor_ex_dep, t_prima_servicios, 
                             t_horas_trabajadas, t_cotiza_pension, mujer,
                             menor_15, edad, segur_subsidiado, 
                             P_Ed_Basica_primaria, P_Ed_Basica_secundaria, 
                             P_Ed_Media, P_Ed_Superior, grado_esc_promedio, 
                             t_tiempo_empresa, Ocupados, Inactivos, 
                             p_recibe_pagos_arriendo, p_recibe_ingresos_ad, 
                             p_ocupados, p_inactivos, p_prima_servicios, 
                             p_horas_trabajadas, p_cotiza_pension,
                             p_tiempo_empresa) 

#Revisar el tipo de cada variable para poder segregar entre categóricas y
#contínuas

str(vars)

table(trainRF$Dominio)

# Seleccionar variables numéricas
numeric_vars <- trainRF %>% 
  dplyr::select(Ncuartos, arriendo_hipotetico, arriendo,
         Npersonas, Nper_unidad_gasto, 
         linea_indigencia, linea_pobreza,
         factor_exp, factor_ex_dep, t_prima_servicios,
         t_horas_trabajadas, t_cotiza_pension, mujer,
         menor_15, edad, segur_subsidiado, P_Ed_Basica_primaria, 
         P_Ed_Basica_secundaria, P_Ed_Media, P_Ed_Superior, 
         grado_esc_promedio, t_tiempo_empresa, Ocupados, Inactivos, 
         p_recibe_pagos_arriendo,
         p_recibe_ingresos_ad, p_ocupados, p_inactivos, p_prima_servicios,
         p_horas_trabajadas, p_cotiza_pension, p_tiempo_empresa)

# Seleccionar variables categóricas

categ_vars <- trainRF %>% 
  dplyr::select(Pobre, Dominio, prop_vivienda, Depto)

#TABLAS ------------------------------------------------------------------------

# Generar tabla de estadística descriptiva para variables numéricas 
numeric_vars <- as.data.frame(numeric_vars)

stargazer(numeric_vars, type = "latex", 
          title = "Estadísticas Descriptivas de Variables Numéricas",
          digits = 3, summary.stat = c("mean", "sd", "min", "max", "median"),
          out = "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/views/tablavarsCONTINUAS.tex")

# Definir función para crear tablas de estadísticas descriptivas para variables categóricas

categorical_summary <- function(data, variable, labels, var_name) {
  freq_table <- table(data[[variable]])
  prop_table <- prop.table(freq_table)
  
  summary_table <- data.frame(
    Categoría = labels,
    Cuenta = as.numeric(freq_table),
    Proporción = round(as.numeric(prop_table), 4),
    Variable = var_name
  )
  
  return(summary_table)
}

# Crear tablas descriptivas para todas las variables categóricas

summary_table_pobre <- categorical_summary(categ_vars, "Pobre", 
                                           c("Pobre", "No Pobre"), 
                                           "Clasificación del hogar según ingresos")
summary_table_vivienda <- categorical_summary(categ_vars, "prop_vivienda", 
                                              c("Propia y pagada", "Propia, pagando", 
                                                "Arriendo / Subarriendo", "Usufructo", 
                                                "Posesión sin título", "Otra"), 
                                              "Tipo de vivienda del hogar")
summary_table_depto <- categorical_summary(
  categ_vars, "Depto",
  levels(categ_vars$Depto), "Departamento de residencia del hogar"  # Niveles del factor Depto
)

summary_table_dominio <- categorical_summary(
  categ_vars, "Dominio", 
  levels(categ_vars$Dominio), "Dominio de residencia del hogar"
)

# Combinar todas las tablas en una sola

summary_table_categorical <- bind_rows(
  summary_table_pobre,
  summary_table_vivienda,
  summary_table_depto,
  summary_table_dominio
) %>%
  dplyr::select(Variable, Categoría, Cuenta, Proporción)  # Reorder columns

# Mostrar tabla de estadísticas descriptivas para v. categóricas
latex_table <- kable(summary_table_categorical, 
      format = "latex",
      booktabs = TRUE,
      caption = "Tabla de Estadísticas Descriptivas - Variables Categóricas")

writeLines(latex_table, "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/views/tabla_variables_categoricas.tex")

# Otras Observaciones (Desbalance Clases) --------------------------------------

# Gráfico para visualizar la distribución de Pobres
ggplot(trainRF, aes(x = Pobre, fill = Pobre)) +
  geom_bar() + 
  theme_minimal() +
  scale_fill_manual(values = c("Si" = "orange", "No"= "blue")) +
  labs(x = "", y = "# de Personas")

# Revisar calsificación DANE

table(trainRF$Pobre)

trainRF<- trainRF %>% mutate(Pobre_hand=ifelse(Ingpcug<linea_pobreza,1,0))
table(trainRF$Pobre,trainRF$Pobre_hand)

train_hogares_clean<- train_hogares_clean %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<linea_pobreza*Nper_unidad_gasto,1,0))
table(train_hogares_clean$Pobre,train_hogares_clean$Pobre_hand_2)

#La clasificación del DANE es perfecta

