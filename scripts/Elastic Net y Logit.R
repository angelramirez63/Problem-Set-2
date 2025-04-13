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
  doParallel)

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
variables <- c("Ncuartos", "Ncuartos_duermen", "prop_vivienda", 
               "arriendo_hipotetico", "arriendo", "Npersonas", "Nper_unidad_gasto", 
               "linea_indigencia", "linea_pobreza", "t_horas_trabajadas", 
               "t_trabaja_solo", "t_microempresa", "t_pequeña_empresa", 
               "t_mediana_empresa", "t_gran_empresa", "mujer", "menor_15", 
               "mayor_60", "edad", "segur_social", "segur_subsidiado", 
               "P_Ed_Superior", "grado_esc_promedio", "t_tiempo_empresa", 
               "Ocupados", "Desempleados", "Inactivos", "Pet", 
               "p_horas_trabajadas", "edad_2", "edad_3", "p_recibe_ingresos_ad",
               "edad_4", "grado_esc_2", "grado_esc_3", "grado_esc_4", 
               "horas_trabaj_2", "horas_trabaj_3", "horas_trabaj_4", 
               "tiempo_empresa_2", "tiempo_empresa_3",  "tiempo_empresa_4", 
               "Dominio", "P_Ed_Basica_secundaria", "P_Ed_Basica_primaria")

# Crear fórmula automáticamente

db_train$Pobre_s <- factor(db_train$Pobre, levels = c(0,1), labels = c("NoPobre", "Pobre"))
formula_logit <- as.formula(paste("Pobre_s ~", paste(variables, collapse = " + ")))

# Configurar validación cruzada
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                     summaryFunction = twoClassSummary, savePredictions = TRUE)


# Entrenar modelo logit con CV
modelo_cv <- train(
  formula_logit,
  data = db_train,
  method = "glm",
  family = "binomial",
  trControl = ctrl, 
  metric = "Accuracy"
)

modelo_cv$pred %>% mutate(correcto = pred == obs) %>%
  summarise(accuracy = mean(correcto))

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
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, 
                     summaryFunction = twoClassSummary)

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
  tuneLength = 20
)

#Error de predicción
pred_clase <- predict(modelo, newdata = X)
conf_1 <- confusionMatrix(pred_clase, y, positive = "Pobre")

#Fijar otro threshold

pred_prob <- predict(modelo, newdata = X, type = "prob")
threshold <- 0.45
pred_clase_custom <- ifelse(pred_prob$Pobre >= threshold, "Pobre", "NoPobre")
pred_clase_custom <- factor(pred_clase_custom, levels = c("Pobre", "NoPobre"))
conf_2 <- confusionMatrix(pred_clase_custom, y, positive = "Pobre")

#Predicción de la muestra de testeo

X_test <- db_test[, c("Ncuartos", "Ncuartos_duermen", "prop_vivienda", "credit_vivienda_mes", 
                  "arriendo_hipotetico", "arriendo", "Npersonas", "Nper_unidad_gasto",
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
