library(pacman)

p_load(tidyverse, # tidy-data
       rpart, # Recursive Partition and Regression Trees (To run Trees)
       caret ,  # for model training and tunning
       rpart.plot, ## for trees graphs
       Metrics, ## Evaluation Metrics for ML
       xtable
)  

rm(list = ls())

# Crear el directorio 
setwd("C:/Users/Adram/OneDrive - Universidad de los Andes/8 OCTAVO SEMESTRE/BDML/Problem-Set-2/stores")

# Cargamos base final
db <- readRDS("db_final.rds") %>% 
  as_tibble()

# Construimos bases train y test

# Construimos base train
db_train <- db %>% 
  filter(test == 0) %>% 
  select(-test)

db_train <- db_train %>% 
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

db_train <- db_train %>% mutate(
  p_cotiza_pension = ifelse(is.na(p_cotiza_pension == T), 0, p_cotiza_pension), 
  pensionado = ifelse(is.na(pensionado == T), 0, pensionado), 
  t_cotiza_pension = ifelse(is.na(t_cotiza_pension == T), 0, t_cotiza_pension)
)

## Construimos base de testeo
db_test <- db %>% 
  filter(test == 1) %>% 
  select(-test)

db_test <- db_test %>% 
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

db_test <- db_test %>% mutate(
  p_cotiza_pension = ifelse(is.na(p_cotiza_pension == T), 0, p_cotiza_pension), 
  pensionado = ifelse(is.na(pensionado == T), 0, pensionado), 
  t_cotiza_pension = ifelse(is.na(t_cotiza_pension == T), 0, t_cotiza_pension)
)

##----------Construcción del arbol----------##

## Usamos CV para hiperparametrizar el alfa que regula el tamaño del arbol

fiveStats <- function(...) {
  c(
    twoClassSummary(...),
    defaultSummary(...)
  )
}

## Para usar ROC) (u otras más) para tuning

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats, # nuestra función 
                    classProbs = TRUE, 
                    verbose=T,
                    savePredictions = T)


# especificamos la grilla de los alphas
grid <- expand.grid(cp = seq(0, 0.03, 0.001))

cv_tree <- train(Pobre~cabecera + Dominio + Ncuartos + Ncuartos_duermen + prop_vivienda + 
                   credit_vivienda_mes + arriendo_hipotetico + arriendo + Npersonas + 
                   Nper_unidad_gasto + linea_indigencia + linea_pobreza + factor_exp + Depto + 
                   factor_ex_dep + t_prima_servicios + t_prima_navidad + 
                   t_prima_vacaciones + t_bonificaciones_anuales + t_horas_trabajadas + 
                   t_cotiza_pension + t_empleo_secundario + t_horas_empleo_secundario + 
                   quiere_trabajar_mas + pensionado + t_trabaja_solo + t_microempresa + 
                   t_pequeña_empresa + t_mediana_empresa + t_gran_empresa + t_ingxhorasextra + 
                   t_primas + t_bonificaciones + t_subsalimentacion + t_substransporte + 
                   t_subsfamiliar + t_subseduc + t_alimentosextra + t_viviendapago + 
                   t_transporteempresa + t_ingresosextraespecie + mujer + menor_15 + mayor_60 + 
                   edad + segur_social + segur_subsidiado + P_Ed_Preescolar + P_Ed_Basica_primaria + 
                   P_Ed_Basica_secundaria + P_Ed_Media + P_Ed_Superior + grado_esc_promedio + 
                   t_tiempo_empresa + Ocupados + Desempleados + Inactivos + Pet + 
                   p_recibe_pagos_arriendo + p_recibe_ingresos_ad + p_ocupados + p_desempleados + 
                   p_inactivos + p_pet + p_prima_servicios + p_prima_navidad + p_prima_vacaciones + 
                   p_bonificaciones_anuales + p_horas_trabajadas + p_cotiza_pension + 
                   p_empleo_secundario + p_horas_empleo_secundario + p_trabaja_solo + 
                   p_microempresa + p_pequeña_empresa + p_mediana_empresa + p_gran_empresa + 
                   p_ingxhorasextra + p_primas + p_bonificaciones + p_subsalimentacion + 
                   p_substransporte + p_subsfamiliar + p_subseduc + p_alimentosextra + 
                   p_viviendapago + p_transporteempresa + p_ingresosextraespecie + p_tiempo_empresa,
                 data = db_train,
                 method = "rpart", 
                 trControl = ctrl, 
                 tuneGrid = grid, 
                 metric= "ROC"
)

cv_tree

# Convertir los resultados a tabla LaTeX
resultados_filtrados <- cv_tree$results[, c("cp", "ROC", "Sens", "Spec", "Accuracy")]

# Crear tabla LaTeX con xtable
xtable(resultados_filtrados,
       caption = "Métricas de desempeño del modelo árbol de decisión",
       digits = 3)
