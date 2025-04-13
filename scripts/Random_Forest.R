#Random Forest  ----------------------------------------------------------------
## Ángel y Juan Pablo

setwd("~/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/stores")

#-------------------------------------------------------------------------------

rm(list = ls())

install.packages("xtable") #Exportar resultados a LaTEX

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
       xtable
)

## Cargar datos ----------------------------------------------------------------

db <- readRDS('db_final.rds') %>% 
  as_tibble()

# Modelo -----------------------------------------------------------------------


head(db)

Pobre_num <- db$Pobre


#Crear base train

trainRF <- db %>% 
  filter(test == 0) %>% 
  select(-test) #Dejar por fuera la variable indicadora que clasifica a los datos como entrenamiento o prueba.

#Crear base test

testRF <- db %>% 
  filter(test == 1) %>% 
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

testRF <- testRF <- testRF %>% 
  mutate(
    Pobre = factor(Pobre, levels = c(1,0), labels = c("Si", "No")),  #Dejar pobre como el primer nivel 
    cabecera = factor(cabecera, levels = c(1,2), labels = c("Cabecera", "Resto")), 
    prop_vivienda = factor(prop_vivienda, levels = c(1,2,3,4,5,6), 
                           labels = c("Propia_pagada", "Propia_pagando", 
                                      "Arriendo", "Usufructo", "Sin_titulo", "Otra")), 
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

testRF <- testRF %>% mutate(
  p_cotiza_pension = ifelse(is.na(p_cotiza_pension == T), 0, p_cotiza_pension), 
  pensionado = ifelse(is.na(pensionado == T), 0, pensionado), 
  t_cotiza_pension = ifelse(is.na(t_cotiza_pension == T), 0, t_cotiza_pension)
)


#Revisar variables cuya varianza sea casi 0 y removerlas.

zero_var_check <- nearZeroVar(trainRF, saveMetrics = T, names = T)
zero_var_check <- zero_var_check %>% 
  filter(nzv == TRUE)

trainRF2 <- trainRF %>%  #Remover variables  con casi nzr (near zero variance)
  select(-credit_vivienda_mes, -t_bonificaciones_anuales, 
         -t_horas_empleo_secundario, -quiere_trabajar_mas, 
         -pensionado, -t_ingxhorasextra, -t_primas, -t_bonificaciones, 
         -t_subsalimentacion, -t_subseduc, -t_viviendapago, 
         -t_transporteempresa, -t_ingresosextraespecie, 
         -P_Ed_Preescolar, -p_desempleados, -p_bonificaciones_anuales, 
         -p_empleo_secundario, -p_horas_empleo_secundario, 
         -p_pequeña_empresa, -p_mediana_empresa, -p_ingxhorasextra, 
         -p_primas, -p_bonificaciones, -p_subsalimentacion, -p_subseduc,
         -p_alimentosextra, -p_viviendapago, -p_transporteempresa, 
         -p_ingresosextraespecie)

rm(zero_var_check)





#Random Forest 1 ---------------------------------------------------------------


set.seed(1112) #Se fija semilla para reproducibilidad

fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
    caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
  )
}

#Establecer grilla de validación cruzada que encuentra el rango de búsqueda de los hiperparámetros del modelo.

ctrl<- trainControl(method = "cv", 
                    number = 5, #Validación cruzada con 5 folds.
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)


mtry_grid<-expand.grid(mtry = c(6, 8, 10, 30), # Fijar m = {6, 8, 10, 30}. Se toma sqrt(30) como benchmark, y 30 incluye bagging.
                       min.node.size= c(1, 10, 100, 500, 1000), #Controla la complejidad (profundidad) del arbol
                       splitrule= 'gini') # Tomamos gini como splitrule.
mtry_grid



cv_RForest1 <- train(Pobre ~ Ncuartos + Ncuartos_duermen + prop_vivienda + arriendo_hipotetico +
                     arriendo + Npersonas+ Nper_unidad_gasto + linea_indigencia + linea_pobreza +
                      t_horas_trabajadas + t_trabaja_solo + t_microempresa + t_pequeña_empresa + t_mediana_empresa + 
                      t_gran_empresa + menor_15 + mayor_60 + mujer + edad + segur_social + segur_subsidiado + P_Ed_Superior + grado_esc_promedio + t_tiempo_empresa + 
                      Ocupados + Desempleados + Inactivos + Pet + p_horas_trabajadas + p_cotiza_pension,
                    data = trainRF, 
                    method = "ranger", # llamamos el paquete del metodo a utilizar
                    trControl = ctrl,
                    metric="F", # metrica a optimizar
                    tuneGrid = mtry_grid,
                    ntree=500,
                    na.action = na.pass
                    )

cv_RForest1 #Sugiere parámetros óptimos de mrty = 6, min node size = 1.

# Exportar resultados

tabla_CVRF1 <- xtable(cv_RForest1$results)
print(tabla_CVRF1, file = "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/views/cv_RForest1_results.tex", include.rownames = FALSE)


#Ahora corremos el mejor modelo:

set.seed(1112) #Se fija semilla para reproducibilidad

mejor_modelo1 <- ranger::ranger(Pobre ~ Ncuartos + Ncuartos_duermen + prop_vivienda + arriendo_hipotetico +
    arriendo + Npersonas+ Nper_unidad_gasto + linea_indigencia + linea_pobreza +
    t_horas_trabajadas + t_trabaja_solo + t_microempresa + t_pequeña_empresa + t_mediana_empresa + 
    t_gran_empresa + menor_15 + mayor_60 + mujer + edad + segur_social + segur_subsidiado + P_Ed_Superior + grado_esc_promedio + t_tiempo_empresa + 
    Ocupados + Desempleados + Inactivos + Pet + p_horas_trabajadas + p_cotiza_pension, #Se corre un RF clasificatorio con todas las variables disponibles creadas a partir de los datos a nivel persona, agrupados a nivel hogar.
  data = trainRF2,
  num.trees = 500, #Hiperparámetros fijados a partir de los óptimos encontrados en el RF anterior.
  mtry = 6,
  min.node.size = 1,
  importance = "impurity",
  metric = "F"
)

## Realizar predicciones en test
prediccionesRF1 <- predict(mejor_modelo1, data = testRF)$predictions

## Crear un dataframe con el id y las predicciones
resultadosRF1 <- data.frame(
  id = testRF$id,
  prediccion = prediccionesRF1
)

# Convertir predicciones a 0 y 1
pred_binarias <- ifelse(prediccionesRF1 == "Si", 1, 0)

# Crear el dataframe final
resultadosRF1 <- data.frame(
  id = testRF$id,
  prediction = pred_binarias
)

# Guardar el archivo CSV
write.csv(resultadosRF1, "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/stores/Predicciones/RF1_m6_minnode1.csv", row.names = FALSE)






#Random Forest 2 ---------------------------------------------------------------

fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
    caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
  )
}

set.seed(1112) #Se fija semilla para reproducibilidad


# Escogemos grilla y método

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=T,
                    savePredictions = T)

mtry_grid<-expand.grid(mtry =c(6, 8, 9, 10),
                       min.node.size = 1, #controla la complejidad del arbol
                       splitrule= 'gini') # tomamos gini como splitrule 

# Calculamos hiperparámetros
cv_RForest2 <- train(Pobre~cabecera + Dominio + Ncuartos + Ncuartos_duermen + prop_vivienda + 
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
                    data = trainRF, 
                    method = "ranger", # llamamos el paquete del metodo a utilizar
                    trControl = ctrl,
                    metric="F", # metrica a optimizar
                    tuneGrid = mtry_grid,
                    ntree=500)

cv_RForest2 #mrty óptimo de 6

# Exportar resultados

# Filtrar solo las columnas que te interesan
cols_interes <- c("mtry", "min.node.size", "splitrule", "ROC", "Sens", "Spec", "Accuracy")
cv_RForest2_filtrado <- cv_RForest2$results[, cols_interes]

# Crear la tabla
tabla_CVRF2 <- xtable(cv_RForest2_filtrado)

# Imprimir con más decimales (4 en este caso)
print(tabla_CVRF2,
      file = "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/views/cv_RForest2_results.tex",
      include.rownames = FALSE,
      digits = c(0, 2, 2, 0, 4, 4, 4, 4))  # ajustado para las columnas seleccionadas

#Correr ranger 

mejor_modelo2 <- ranger::ranger(
  Pobre~cabecera + Dominio + Ncuartos + Ncuartos_duermen + prop_vivienda + 
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
  data = trainRF,
  num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
  mtry= 6,   # N. var aleatoriamente seleccionadas en cada partición
  min.node.size  = 1, ## Numero minimo de observaciones en un nodo
  importance="impurity") 

mejor_modelo2

## Realizar predicciones en test
prediccionesRF2 <- predict(mejor_modelo2, data = testRF)$predictions

## Crear un dataframe con el id y las predicciones
resultadosRF2 <- data.frame(
  id = testRF$id,
  prediccion = prediccionesRF2
)

write.csv(resultadosRF2, "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/stores/Predicciones/RF2_m6_minnode1.csv", row.names = FALSE)





#Random Forest 3 ---------------------------------------------------------------

#Importancia de variables 

variables_importantes_RF2 = mejor_modelo2$variable.importance

datos_impRF2 <- data.frame(variables = names(variables_importantes_RF2), importance = variables_importantes_RF2)


ggplot(datos_impRF2, aes(x = reorder(variables, importance) , y =importance )) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Importancia con `ranger` para RF2 ", x = "Importance", y="Variable") +
  theme_minimal() +
  coord_flip() 

#PARA ESTE MODELO, FILTRAMOS PARA VARIABLES CON MÁS DE 500 DE IMPORTANCIA

# Filtrar las variables con importancia mayor a 500
variables_muy_importantes <- subset(datos_impRF2, importance > 500)

# Mostrar
print(variables_muy_importantes)


# Se usó CV-K5 para optimizar el hiperparámetro de variables escogidas
# entre 6, 8, 9 o 10 con un mínimo de observaciones por hoja igual a 1

set.seed(0879)

# Escogemos grilla y método
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=T,
                    savePredictions = T)

mtry_grid<-expand.grid(mtry =c(6, 8, 9, 10),
                       min.node.size = 1, #controla la complejidad del arbol
                       splitrule= 'gini') # tomamos gini como splitrule 

# Calculamos hiperparámetros
cv_RForest3 <- train(Pobre~Dominio + Ncuartos + Ncuartos_duermen + prop_vivienda + arriendo_hipotetico + 
                      arriendo + Npersonas + Nper_unidad_gasto + linea_indigencia + linea_pobreza + 
                      factor_exp + Depto + factor_ex_dep + t_prima_servicios + t_horas_trabajadas + 
                      t_cotiza_pension + quiere_trabajar_mas + t_trabaja_solo + t_microempresa + 
                      t_gran_empresa + t_substransporte + mujer + menor_15 + mayor_60 + edad + 
                      segur_social + segur_subsidiado + P_Ed_Basica_primaria + P_Ed_Basica_secundaria + 
                      P_Ed_Media + P_Ed_Superior + grado_esc_promedio + t_tiempo_empresa + Ocupados + 
                      Desempleados + Inactivos + Pet + p_recibe_pagos_arriendo + p_recibe_ingresos_ad + 
                      p_ocupados + p_desempleados + p_inactivos + p_pet + p_prima_servicios + 
                      p_horas_trabajadas + p_cotiza_pension + p_trabaja_solo + p_microempresa + 
                      p_gran_empresa + p_substransporte + p_tiempo_empresa, 
                    data = trainRF, 
                    method = "ranger", # llamamos el paquete del metodo a utilizar
                    trControl = ctrl,
                    metric="F", # metrica a optimizar
                    tuneGrid = mtry_grid,
                    ntree=500)

cv_RForest3

# Filtrar columnas de interés
cols_interes <- c("mtry", "min.node.size", "splitrule", "ROC", "Sens", "Spec", "Accuracy")
cv_RForest3_filtrado <- cv_RForest3$results[, cols_interes]

# Crear tabla xtable
tabla_CVRF3 <- xtable(cv_RForest3_filtrado)

# Exportar con mayor precisión decimal
print(tabla_CVRF3,
      file = "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/views/cv_RForest3_results.tex",
      include.rownames = FALSE,
      digits = c(0, 2, 2, 0, 4, 4, 4, 4))


# El hiperparámetro escogido es 10, como identificado por el CV anterior.

mejor_modelo3<- ranger::ranger(
  Pobre~p_ocupados + arriendo_hipotetico + edad + segur_subsidiado + menor_15 + 
    t_horas_trabajadas + p_recibe_pagos_arriendo + p_horas_trabajadas + 
    factor_exp + factor_ex_dep + linea_pobreza + linea_indigencia + 
    P_Ed_Superior + t_tiempo_empresa + p_tiempo_empresa + Nper_unidad_gasto + 
    Npersonas + arriendo + Depto + p_cotiza_pension + grado_esc_promedio + 
    Dominio + Ncuartos + mujer + prop_vivienda + t_cotiza_pension + 
    P_Ed_Basica_primaria + t_prima_servicios + p_prima_servicios + 
    p_inactivos + p_recibe_ingresos_ad + P_Ed_Basica_secundaria + 
    Inactivos + P_Ed_Media + segur_social + Ocupados,
  data = trainRF,
  num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
  mtry= 10,   # N. var aleatoriamente seleccionadas en cada partición
  min.node.size  = 1, ## Numero minimo de observaciones en un nodo
  importance="impurity")

mejor_modelo3


## Realizar predicciones en test
prediccionesRF3 <- predict(mejor_modelo3, data = testRF)$predictions

## Crear un dataframe con el id y las predicciones
resultadosRF3 <- data.frame(
  id = testRF$id,
  prediccion = prediccionesRF3
)

write.csv(resultadosRF3, "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/stores/Predicciones/RF3_m10_minnode1.csv", row.names = FALSE)






#Random Forest 4 ---------------------------------------------------------------

#Importancia de variables 

#PARA ESTE MODELO, FILTRAMOS PARA VARIABLES CON MÁS DE 250 DE IMPORTANCIA

# Filtrar las variables con importancia mayor a 250
variables_muy_importantes <- subset(datos_impRF2, importance > 250)

# Mostrar
print(variables_muy_importantes)

#Validación cruzada con variables más importantes

fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
    caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
  )
}

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

mtry_grid<-expand.grid(mtry =c(6,8,9,10), 
                       min.node.size= 1, #controla la complejidad del arbol
                       splitrule= 'gini') # tomamos gini como splitrule 
mtry_grid

set.seed(1112) #Se fija semilla para reproducibilidad


cv_RForest4 <- train(Pobre ~   Ncuartos + Ncuartos_duermen + prop_vivienda + 
                       arriendo_hipotetico + arriendo + Npersonas + Nper_unidad_gasto + 
                       linea_indigencia + linea_pobreza + factor_exp + Depto + factor_ex_dep + 
                       t_prima_servicios + t_horas_trabajadas + t_cotiza_pension + Dominio +
                       quiere_trabajar_mas + t_trabaja_solo + t_microempresa + t_gran_empresa + 
                       t_substransporte + mujer + menor_15 + mayor_60 + edad + segur_social + 
                       segur_subsidiado + P_Ed_Basica_primaria + P_Ed_Basica_secundaria + 
                       P_Ed_Media + P_Ed_Superior + grado_esc_promedio + t_tiempo_empresa + 
                       Ocupados + Desempleados + Inactivos + Pet + p_recibe_pagos_arriendo + 
                       p_recibe_ingresos_ad + p_ocupados + p_desempleados + p_inactivos + 
                       p_pet + p_prima_servicios + p_horas_trabajadas + p_cotiza_pension + 
                       p_trabaja_solo + p_microempresa + p_gran_empresa + p_substransporte + 
                       p_tiempo_empresa, 
                    data = trainRF, 
                    method = "ranger", # llamamos el paquete del metodo a utilizar
                    trControl = ctrl,
                    metric="F", # metrica a optimizar
                    tuneGrid = mtry_grid,
                    ntree=1000)

cv_RForest4 #mrty óptimo = 10

# Exportar resultados

# Filtrar columnas relevantes
cols_interes <- c("mtry", "min.node.size", "splitrule", "ROC", "Sens", "Spec", "Accuracy")
cv_RForest4_filtrado <- cv_RForest4$results[, cols_interes]

# Crear tabla xtable
tabla_CVRF4 <- xtable(cv_RForest4_filtrado)

# Exportar tabla con precisión de 4 decimales
print(tabla_CVRF4,
      file = "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/views/cv_RForest4_results.tex",
      include.rownames = FALSE,
      digits = c(0, 2, 2, 0, 4, 4, 4, 4))

#Correr ranger

set.seed(1112) #Se fija semilla para reproducibilidad

mejor_modelo4 <- ranger::ranger(
  Pobre ~ Ncuartos + Ncuartos_duermen + prop_vivienda + 
    arriendo_hipotetico + arriendo + Npersonas + Nper_unidad_gasto + 
    linea_indigencia + linea_pobreza + factor_exp + Depto + factor_ex_dep + 
    t_prima_servicios + t_horas_trabajadas + t_cotiza_pension + 
    quiere_trabajar_mas + t_trabaja_solo + t_microempresa + t_gran_empresa + 
    t_substransporte + mujer + menor_15 + mayor_60 + edad + segur_social + 
    segur_subsidiado + P_Ed_Basica_primaria + P_Ed_Basica_secundaria + 
    P_Ed_Media + P_Ed_Superior + grado_esc_promedio + t_tiempo_empresa + 
    Ocupados + Desempleados + Inactivos + Pet + p_recibe_pagos_arriendo + 
    p_recibe_ingresos_ad + p_ocupados + p_desempleados + p_inactivos + 
    p_pet + p_prima_servicios + p_horas_trabajadas + p_cotiza_pension + 
    p_trabaja_solo + p_microempresa + p_gran_empresa + p_substransporte + 
    p_tiempo_empresa, #Se corre un RF clasificatorio con todas las variables disponibles creadas a partir de los datos a nivel persona, agrupados a nivel hogar.
  data = trainRF,
  num.trees = 1000, #Hiperparámetros fijados a partir de los óptimos encontrados en el RF anterior.
  mtry = 10,
  min.node.size = 1,
  importance = "impurity",
)

mejor_modelo4

## Realizar predicciones en test
prediccionesRF4 <- predict(mejor_modelo4, data = testRF)$predictions

## Convertir de "Si"/"No" a 1/0
predicciones_binarias <- ifelse(prediccionesRF4 == "Si", 1, 0)

## Crear un dataframe con el id y las predicciones binarias
resultadosRF4 <- data.frame(
  id = testRF$id,
  prediccion = predicciones_binarias
)

write.csv(resultadosRF4, "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/stores/Predicciones/RF4_m10_minnode1.csv", row.names = FALSE)
















#Random Forest 5 ---------------------------------------------------------------

#El siguiente modelo RF reduce los parámetros de la regresión a estimar a través de Elastic Net.

set.seed(1112) # Se fija semilla para reproducibilidad 


fitControl <- trainControl( 
  method = "cv",
  number = 5,
  classProbs = TRUE,
  savePredictions = T) ##  5 fold CV

lambda <- 10^seq(1, -4, length = 100)  # Genera una secuencia de valores de lambda para la regularización

#Sin variable dominio


model_form1 <- train(Pobre ~ cabecera  + Ncuartos + Ncuartos_duermen + prop_vivienda + 
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
  data = trainRF,
  metric = 'F1',
  method = 'glmnet',
  family = "binomial",
  trControl = fitControl,
  na.action = na.pass,
  tuneGrid = expand.grid(expand.grid('alpha'= seq(0,1, 0.01), 
                                     lambda=lambda)
      )
)
