#Logit y Elastic Net -----------------------------------------------------------

#Preparación -------------------------------------------------------------------

#Paquetes
library(pacman)

p_load(
  rio,       # import/export data
  tidyverse, # tidy-data
  caret,     # For predictive model assessment
  leaps,     #for subset  model selection
  glmnet,    # Elastic net
  doParallel, 
  yardstick, # Para usar F1 como mi métrica de selección
  here, 
  xtable, 
  kableExtra)

#Definir el directorio
wd <- here()
setwd(wd)
rm(wd)


#Cargar datos

db <- readRDS("stores/db_final.rds")

# Función para obtener polinomios de grado 4 y devolver un data.frame sin la columna original
crear_poly <- function(x, var_name, grado = 4) {
  poly_terms <- poly(x, degree = grado, raw = TRUE)
  poly_df <- as.data.frame(poly_terms)
  names(poly_df) <- paste0(var_name, "_", 1:grado)
  return(poly_df)
}

# Generar términos polinómicos para cada variable
poly_horas_trabajadas <- crear_poly(db$t_horas_trabajadas, "horas_trabaj")
poly_grado_esc <- crear_poly(db$grado_esc_promedio, "grado_esc")
poly_edad <- crear_poly(db$edad, "edad")
poly_tiempo_empresa <- crear_poly(db$t_tiempo_empresa, "tiempo_empresa")


# Agregar las nuevas variables a la base original
db <- cbind(db, poly_horas_trabajadas, poly_grado_esc, poly_edad, poly_tiempo_empresa)

#Filtrar train y test

db_train <- db %>% filter(test==0)
db_test <- db %>% filter(test==1)
db_test <- db_test %>% select(-Pobre, -Ingpcug)


#Logit---------------------------------------------------------------------------

#F1
f1_summary <- function(data, lev = NULL, model = NULL) {
  # Make sure the prediction and truth are factors
  data$pred <- factor(data$pred, levels = lev)
  data$obs <- factor(data$obs, levels = lev)
  
  # Compute F1 using yardstick
  f1 <- yardstick::f_meas_vec(truth = data$obs, estimate = data$pred, event_level = "second")
  
  out <- c(F1 = f1)
  return(out)
}

#Formula modelo logit 

db_train$Pobre_s <- factor(db_train$Pobre, levels = c(0,1), labels = c("NoPobre", "Pobre"))
formula_logit <- Pobre_s ~ p_ocupados + arriendo_hipotetico + edad + segur_subsidiado + menor_15 + 
  t_horas_trabajadas + p_recibe_pagos_arriendo + p_horas_trabajadas + 
  factor_exp + factor_ex_dep + linea_pobreza + linea_indigencia + 
  P_Ed_Superior + t_tiempo_empresa + p_tiempo_empresa + Nper_unidad_gasto + 
  Npersonas + arriendo + Depto + grado_esc_promedio + 
  Dominio + Ncuartos + mujer + prop_vivienda + 
  P_Ed_Basica_primaria + t_prima_servicios + p_prima_servicios + 
  p_inactivos + p_recibe_ingresos_ad + P_Ed_Basica_secundaria + 
  Inactivos + P_Ed_Media + segur_social + Ocupados

##Las variables provienen de las variables con mayor importancia en el modelo de Bosque Aleatorio

# Configurar validación cruzada
ctrl <- trainControl(
  method = "cv",
  number = 10,
  classProbs = FALSE,  # No necesitas probabilidades si solo vas a usar predicción directa
  summaryFunction = f1_summary,
  savePredictions = TRUE
)

#Modelo
logit <- train(
  formula_logit,
  data = db_train,
  method = "glm",
  family = "binomial",
  trControl = ctrl,
  metric = "F1"
)

logit$results

#Enet ---------------------------------------------------------------------------

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
                  "p_tiempo_empresa", "cabecera", "Depto", "menor_15", "arriendo", "Dominio", 
                  "t_horas_trabajadas", "t_tiempo_empresa", "p_inactivos", "Pet",
                  "edad_2", "edad_3","edad_4", "grado_esc_2", "grado_esc_3", "grado_esc_4", 
                  "horas_trabaj_2", "horas_trabaj_3", "horas_trabaj_4", "tiempo_empresa_2", 
                  "tiempo_empresa_3")]

y <- db_train$Pobre  # La variable dependiente que es binaria 
y <- factor(y, levels = c(1, 0), labels = c("Pobre", "NoPobre"))
pesos <- db_train$factor_exp


#Control
ctrl <- trainControl(method = "cv", number = 10, classProbs = FALSE, 
                     summaryFunction = f1_summary, savePredictions = TRUE)

#Grilla
tuneGrid<- expand.grid(alpha= seq(0.02, 1, 0.02), # between 0 and 1. 
                       lambda=seq(0.05, 2, 0.05) ) 
# Entrenar el modelo
Enet <- train(
  x = X,
  y = y,
  method = "glmnet",
  trControl = ctrl,
  weights = pesos,
  family = "binomial",  # Para regresión logística
  tuneLength = 10, 
  metric = "F1"
)

saveRDS(Enet, file = "stores/Enet_model.rds")


#Error de predicción
coef_Enet=coef(Enet$finalModel,  Enet$bestTune$lambda)
var_imp <- varImp(Enet, lambda = Enet$bestTune$lambda, scale = TRUE)

# Asegúrate de que las predicciones y observaciones sean factores
Enet$pred$obs <- factor(Enet$pred$obs, levels = c("Pobre", "NoPobre"))
Enet$pred$pred <- factor(Enet$pred$pred, levels = c("Pobre", "NoPobre"))

# Calcular varias métricas
metrics(data = Enet$pred, truth = obs, estimate = pred)

# Convert the results to LaTeX
# Ordenar por F1
tabla_ordenada <- Enet$results %>% arrange(desc(F1))

# Exportar en formato LaTeX con 6 decimales
kable(tabla_ordenada, format = "latex", digits = 6, booktabs = TRUE) %>%
  kable_styling(latex_options = c("hold_position"))


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
                      "p_tiempo_empresa", "cabecera", "Depto", "menor_15", "arriendo", "Dominio", 
                      "t_horas_trabajadas", "t_tiempo_empresa", "p_inactivos", "Pet",
                      "edad_2", "edad_3","edad_4", "grado_esc_2", "grado_esc_3", "grado_esc_4", 
                      "horas_trabaj_2", "horas_trabaj_3", "horas_trabaj_4", "tiempo_empresa_2", 
                      "tiempo_empresa_3")]

pred_clase <- predict(Enet, newdata = X_test)

pred_clase <- as.data.frame(pred_clase)

id <- db_test %>% select(id)

prediccion_Enet <- cbind(id, pred_clase)
prediccion_Enet$prediccion <- ifelse(prediccion_Enet$pred_clase== "Pobre", 1, 0)
prediccion_Enet <- prediccion_Enet %>% select(-pred_clase)

write.csv(prediccion_Enet, "prediccion_Enet_polys.csv", row.names = FALSE)
