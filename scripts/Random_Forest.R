#Random Forest  ----------------------------------------------------------------
rm(list = ls())

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
       rio
)

## Ángel y Juan Pablo

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

trainRF$Pobre <- as.factor(trainRF$Pobre)
trainRF$Depto <- as.factor(trainRF$Depto)
trainRF$cabecera <- as.factor(trainRF$cabecera)
trainRF$prop_vivienda <- as.factor(trainRF$prop_vivienda)
trainRF$Dominio <- as.factor(trainRF$Dominio)

testRF$Pobre <- as.factor(testRF$Pobre)
testRF$Depto <- as.factor(testRF$Depto)
testRF$cabecera <- as.factor(testRF$cabecera)
testRF$prop_vivienda <- as.factor(testRF$prop_vivienda)
testRF$Dominio <- as.factor(testRF$Dominio)

#Definir control

fitControl <- trainControl(method = 'cv', number = 10)

#Ideas: Usar prop o conteo

#Random Forest 2 ---------------------------------------------------------------

set.seed(1112) #Se fija semilla para reproducibilidad

trainRF$Pobre <- factor(trainRF$Pobre,
                        levels = c(0, 1),
                        labels = c("NoPobre", "Pobre"))


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


mtry_grid<-expand.grid(mtry = c(8, 51), # Fijar m = {8,51}. Se toma sqrt(8) como benchmark, y 51 incluye bagging.
                       min.node.size= c(1, 5, 10, 50, 100, 200, 300, 500, 1000), #Controla la complejidad (profundidad) del arbol
                       splitrule= 'gini') # Tomamos gini como splitrule.
mtry_grid



cv_RForest <- train(Pobre ~ cabecera + Dominio + Ncuartos + Ncuartos_duermen + prop_vivienda + 
                      credit_vivienda_mes + arriendo_hipotetico + arriendo + Npersonas + 
                      Nper_unidad_gasto + linea_indigencia + linea_pobreza + factor_exp + Depto + 
                      factor_ex_dep  + tamaño_hogar + prima_servicios + 
                      prima_navidad + prima_vacaciones + bonificaciones_anuales + 
                      horas_empleo_principal + cotiza_pension + empleo_secundario + 
                      horas_empleo_secundario + quiere_trabajar_mas + pensionado + trabaja_solo + 
                      microempresa + pequeña_empresa + mediana_empresa + gran_empresa + 
                      dicotom_ingxhorasextra + dicotom_primas + dicotom_bonificaciones + 
                      dicotom_subsalimentacion + dicotom_substransporte + dicotom_subsfamiliar + 
                      dicotom_subseduc + dicotom_alimentosextra + dicotom_viviendapago + 
                      dicotom_transporteempresa + dicotom_ingresosextraespecie + mujer + 
                      menor_15 + mayor_60 + edad + segur_social + segur_subsidiado + 
                      educ_sup + tiempo_empresa, #Se corre un RF clasificatorio con todas las variables disponibles creadas a partir de los datos a nivel persona, agrupados a nivel hogar.
                    data = trainRF, 
                    method = "ranger", # llamamos el paquete del metodo a utilizar
                    trControl = ctrl,
                    metric="F1", # metrica a optimizar
                    tuneGrid = mtry_grid,
                    ntree=500,
                    na.action = na.pass
                    )

cv_RForest

#Random Forest 3 ---------------------------------------------------------------

set.seed(1112) #Se fija semilla para reproducibilidad

mejor_modelo <- ranger::ranger(
  Pobre ~ ., #Se corre un RF clasificatorio con todas las variables disponibles creadas a partir de los datos a nivel persona, agrupados a nivel hogar.
  data = trainRF,
  num.trees = 500, #Hiperparámetros fijados a partir de los óptimos encontrados en el RF anterior.
  mtry = 10,
  min.node.size = 10,
  importance = "impurity",
  metric = "F1"
)


## Realizar predicciones en test
predicciones <- predict(mejor_modelo, data = testRF)$predictions

## Crear un dataframe con el id y las predicciones
resultados <- data.frame(
  id = testRF$id,
  prediccion = predicciones
)

write.csv(resultados, "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/stores/predicciones_5.csv", row.names = FALSE)

#Random Forest 4 ---------------------------------------------------------------


