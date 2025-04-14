#Regresión y Elastic Net -------------------------------------------------------

#Preparación -------------------------------------------------------------------

#Paquetes
library(pacman)

p_load(
  rio,       # import/export data
  tidyverse, # tidy-data
  caret,     # For predictive model assessment
  leaps,     #for subset  model selection
  glmnet,    # Elastic net
  doParallel)

#Definir el directorio
wd <- here()
setwd(wd)
rm(wd)


#Cargar datos

db <- readRDS("stores/db_final.rds")

#Filtrar train y test

db_train <- db %>% filter(test==0)
db_test <- db %>% filter(test==1)
db_test <- db_test %>% select(-Pobre, -Ingpcug)


#Modelo ------------------------------------------------------------------------

# Usa todos menos uno de los núcleos
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

# Preparar las variables predictoras (X) y la variable dependiente (y)
X <- db_train[, c("Ncuartos", "Ncuartos_duermen", "prop_vivienda", "credit_vivienda_mes", 
                  "arriendo_hipotetico", "arriendo", "Npersonas", "Nper_unidad_gasto", "menor_15",
                  "mujer", "mayor_60", "segur_social", "P_Ed_Preescolar", "P_Ed_Basica_secundaria", 
                  "P_Ed_Basica_primaria", "P_Ed_Media", "P_Ed_Superior","grado_esc_promedio", 
                  "p_ocupados", "p_desempleados", "edad", "segur_subsidiado", "p_recibe_pagos_arriendo", 
                  "p_recibe_ingresos_ad", "p_prima_servicios", "p_prima_navidad", "p_prima_vacaciones",       
                  "p_bonificaciones_anuales", "p_horas_trabajadas", "p_ingresosextraespecie", 
                  "p_empleo_secundario", "p_horas_empleo_secundario", "p_trabaja_solo", "p_microempresa",
                  "p_pequeña_empresa", "p_mediana_empresa", "p_gran_empresa", "p_ingxhorasextra",                           
                  "p_bonificaciones", "p_subsalimentacion", "p_substransporte", "p_subsfamiliar",           
                  "p_primas", "p_subseduc", "p_alimentosextra", "p_viviendapago", 
                  "p_tiempo_empresa", "cabecera", "Depto", "t_horas_trabajadas", 
                  "t_tiempo_empresa", "p_inactivos")]

y <- db_train$Ingpcug  # La variable dependiente que es binaria 
pesos <- db_train$factor_exp

#Control
ctrl <- trainControl(method = "cv", number = 10)

#Grilla
tuneGrid<- expand.grid(alpha= seq(0.01, 1, 0.01), # between 0 and 1. 
                       lambda=seq(0.1, 3, 0.1) ) 
# Entrenar el modelo
modelo <- train(
  x = X,
  y = y,
  method = "glmnet",
  trControl = ctrl,
  tuneGrid = tuneGrid
)

#Error de predicción
pred_ing <- predict(modelo, newdata = X)

# Clasificar hogares como pobres o no pobres
clasificacion_pobreza <- ifelse(pred_ing < db_train$linea_pobreza, 1, 0)


# Crear la matriz de confusión
confusionMatrix(
  factor(clasificacion_pobreza, levels = c(1, 0)),  # predicción
  factor(db_train$Pobre, levels = c(1, 0)),       # valor real
  positive = "1"
)

#Predicción de la muestra de testeo

X_test <- db_test[, c("Ncuartos", "Ncuartos_duermen", "prop_vivienda", "credit_vivienda_mes", 
                       "arriendo_hipotetico", "arriendo", "Npersonas", "Nper_unidad_gasto", "menor_15",
                       "mujer", "mayor_60", "segur_social", "P_Ed_Preescolar", "P_Ed_Basica_secundaria", 
                       "P_Ed_Basica_primaria", "P_Ed_Media", "P_Ed_Superior","grado_esc_promedio", 
                       "p_ocupados", "p_desempleados", "edad", "segur_subsidiado", "p_recibe_pagos_arriendo", 
                       "p_recibe_ingresos_ad", "p_prima_servicios", "p_prima_navidad", "p_prima_vacaciones",       
                       "p_bonificaciones_anuales", "p_horas_trabajadas", "p_ingresosextraespecie", 
                       "p_empleo_secundario", "p_horas_empleo_secundario", "p_trabaja_solo", "p_microempresa",
                       "p_pequeña_empresa", "p_mediana_empresa", "p_gran_empresa", "p_ingxhorasextra",                           
                       "p_bonificaciones", "p_subsalimentacion", "p_substransporte", "p_subsfamiliar",           
                       "p_primas", "p_subseduc", "p_alimentosextra", "p_viviendapago", 
                       "p_tiempo_empresa", "cabecera", "Depto", "t_horas_trabajadas", 
                       "t_tiempo_empresa", "p_inactivos")]

pred_clase <- predict(modelo, newdata = X_test)

pred_clase <- as.data.frame(pred_clase)

id <- db_test %>% select(id)

prediccion_Enet <- cbind(id, pred_clase)
prediccion_Enet$prediccion <- ifelse(prediccion_Enet$pred_clase== "Pobre", 1, 0)
prediccion_Enet <- prediccion_Enet %>% select(-pred_clase)

write.csv(prediccion_Enet, "prediccion_Enet_3.csv", row.names = FALSE)
