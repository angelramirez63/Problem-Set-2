library(pacman)

p_load(tidyverse,
       caret,
       glmnet,
       smotefamily
)

rm(list = ls())

# Crear el directorio 
setwd("C:/Users/Adram/OneDrive - Universidad de los Andes/8 OCTAVO SEMESTRE/BDML/Problem-Set-2/stores")
rm(wd)

# Cargamos base final
db <- readRDS("db_final.rds") %>% 
  as_tibble()

##---------- Construcción de bases ----------##

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

## ## Construimos base de testeo
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

##---------- Estimación ---------##

## Random Forest - Número 1


#---------------ESTIMACIONES FALTANTES--------------------------##

fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
    caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
  )
}

set.seed(0879)

## Random Forest - Número 2

# Se obtuvo la importancia de cada variable medida por el GINI
# Se filtraron las variables con una importancia mayor a 1000 ()
# El desempeño disminuyó

mejor_modelo <- ranger::ranger(
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
  data = db_train,
  num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
  mtry= 6,   # N. var aleatoriamente seleccionadas en cada partición
  min.node.size  = 1, ## Numero minimo de observaciones en un nodo
  importance="impurity") 
mejor_modelo

# Encontramos la importancia de las variables

imp<-mejor_modelo$variable.importance
imp2<- data.frame(variables= names(imp),
                  importance= imp)

ggplot(imp2, aes(x = reorder(variables, importance) , y =importance )) +
  geom_bar(stat = "identity", fill = "red") +
  labs(title = "Importancia con `ranger` ", x = "Importance", y="Variable") +
  theme_minimal() +
  coord_flip() 

# Se filtraron las variables con una importanica mayor a 500 ()

variables_importantes <- names(imp[imp > 500])
variables_importantes

# Se usó CV-K5 para optimizar el hiperparámetro de variables escogidas
# entre 6, 8, 9 o 10 con un mínimo de observaciones por hoja igual a 1

set.seed(0879)

# Escogemos grilla y método
ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)

mtry_grid<-expand.grid(mtry =c(6,8, 9, 10),
                       min.node.size = 1, #controla la complejidad del arbol
                       splitrule= 'gini') # tomamos gini como splitrule 

# Calculamos hiperparámetros
cv_RForest <- train(Pobre~p_ocupados + arriendo_hipotetico + edad + segur_subsidiado + menor_15 + 
                      t_horas_trabajadas + p_recibe_pagos_arriendo + p_horas_trabajadas + 
                      factor_exp + factor_ex_dep + linea_pobreza + linea_indigencia + 
                      P_Ed_Superior + t_tiempo_empresa + p_tiempo_empresa + Nper_unidad_gasto + 
                      Npersonas + arriendo + Depto + p_cotiza_pension + grado_esc_promedio + 
                      Dominio + Ncuartos + mujer + prop_vivienda + t_cotiza_pension + 
                      P_Ed_Basica_primaria + t_prima_servicios + p_prima_servicios + 
                      p_inactivos + p_recibe_ingresos_ad + P_Ed_Basica_secundaria + 
                      Inactivos + P_Ed_Media + segur_social + Ocupados, 
                    data = db_train, 
                    method = "ranger", # llamamos el paquete del metodo a utilizar
                    trControl = ctrl,
                    metric="F", # metrica a optimizar
                    tuneGrid = mtry_grid,
                    ntree=500)

cv_RForest

# El hiperparámetro escogido es 9 el número de variables escogidas

mejor_modelo_r<- ranger::ranger(
  Pobre~p_ocupados + arriendo_hipotetico + edad + segur_subsidiado + menor_15 + 
    t_horas_trabajadas + p_recibe_pagos_arriendo + p_horas_trabajadas + 
    factor_exp + factor_ex_dep + linea_pobreza + linea_indigencia + 
    P_Ed_Superior + t_tiempo_empresa + p_tiempo_empresa + Nper_unidad_gasto + 
    Npersonas + arriendo + Depto + p_cotiza_pension + grado_esc_promedio + 
    Dominio + Ncuartos + mujer + prop_vivienda + t_cotiza_pension + 
    P_Ed_Basica_primaria + t_prima_servicios + p_prima_servicios + 
    p_inactivos + p_recibe_ingresos_ad + P_Ed_Basica_secundaria + 
    Inactivos + P_Ed_Media + segur_social + Ocupados,
  data = db_train,
  num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
  mtry= 9,   # N. var aleatoriamente seleccionadas en cada partición
  min.node.size  = 1, ## Numero minimo de observaciones en un nodo
  importance="impurity")

mejor_modelo_r

## Random Forest - Número 3

# Con base en que un mayor número de arboles con una alta varianza entre
# cada arbol se realizó la mísma estimación pero con un mayor número de arboles

mejor_modelo<- ranger::ranger(
  Pobre~p_ocupados + arriendo_hipotetico + edad + segur_subsidiado + menor_15 + 
    t_horas_trabajadas + p_recibe_pagos_arriendo + p_horas_trabajadas + 
    factor_exp + factor_ex_dep + linea_pobreza + linea_indigencia + 
    P_Ed_Superior + t_tiempo_empresa + p_tiempo_empresa + Nper_unidad_gasto + 
    Npersonas + arriendo + Depto + p_cotiza_pension + grado_esc_promedio + 
    Dominio + Ncuartos + mujer + prop_vivienda + t_cotiza_pension + 
    P_Ed_Basica_primaria + t_prima_servicios + p_prima_servicios + 
    p_inactivos + p_recibe_ingresos_ad + P_Ed_Basica_secundaria + 
    Inactivos + P_Ed_Media + segur_social + Ocupados,
  data = db_train,
  num.trees= 600, ## Numero de bootstrap samples y arboles a estimar. Default 500  
  mtry= 9,   # N. var aleatoriamente seleccionadas en cada partición
  min.node.size  = 1, ## Numero minimo de observaciones en un nodo
  importance="impurity")

mejor_modelo

## Realizar predicciones en test
predicciones <- predict(mejor_modelo, data = db_test)$predictions

## Crear un dataframe con el id y las predicciones
resultados <- data.frame(
  id = db_test$id,
  prediccion = predicciones
)

write.csv(resultados, "RF_mtry_9_min.node_1_nu.mtrees600_.csv", row.names = FALSE)



# Observando un salto de importancia en 250 haremos la estimación
# con esas variables

variables_importantes <- names(imp[imp > 250])
variables_importantes

mejor_modelo_r_n<- ranger::ranger(
  Pobre~Dominio + Ncuartos + Ncuartos_duermen + prop_vivienda + arriendo_hipotetico + 
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
  data = db_train,
  num.trees= 600, ## Numero de bootstrap samples y arboles a estimar. Default 500  
  mtry= 9,   # N. var aleatoriamente seleccionadas en cada partición
  min.node.size  = 1, ## Numero minimo de observaciones en un nodo
  importance="impurity")
mejor_modelo_r_n

predicciones <- predict(mejor_modelo_r_n, data = db_test)$predictions

## Crear un dataframe con el id y las predicciones
resultados <- data.frame(
  id = db_test$id,
  prediccion = predicciones
)

write.csv(resultados, "RF_mtry_9_min.node_1_nu.mtrees600_.csv", row.names = FALSE)
