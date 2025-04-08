#============================ Boosting =========================================

#Alistar ambiente de trabajo ---------------------------------------------------

#Limpiar ambiente
rm(list = ls())
cat("\014")


#Cargar paquetes
#cargar adabag después(no lo he podido cargar)
require("pacman")

p_load(
  tidyverse, # tidy-data
  skimr, #Handle missing values
  caret ,  # for model training and tunning
  Metrics, # Evaluation Metrics for ML
  here, # Makes paths to files easier
  adabag, #Adaptative Boosting
  Metrics, # Métricas para evaluar modelos
  MLmetrics, # Calcular metricas
  MLeval,    # Evaluar modelos de clasificación
  glmnet #Modelos de regularización 
)  


#Definir el directorio
wd <- here()
setwd(wd)
rm(wd)

#Cargar datos 

db_final <- readRDS("stores/db_final.rds")

#Datos de entrenamiento
train <- db_final %>% 
         filter(test == 0) %>% 
         select(-test)

train <- train %>% 
              filter(is.na(t_cotiza_pension) == F) #Remover missing values de la variable pensión
        

#Datos de testeo
test <- db_final %>%
        filter(test == 1) %>% 
        select(-test)

test <- test %>% 
  filter(is.na(t_cotiza_pension) == F) #Remover missing values de la variable pensión


rm(db_final)


#Variables  catergorícas como factores  ----------------------------------------

train <- train %>% 
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

test <- test <- test %>% 
         mutate(
           Pobre = factor(Pobre, levels = c(1,0), labels = c("Si", "No")), #Dejar pobre como el primer nivel 
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


##Nombres variables ---------####

train_skim <-skim(train)
variables <- train_skim %>% select(skim_variable)
rm(train_skim)

#Tratar desbalance de clases ---------------------------------------------------


##Caracteriza desbalance-----####


#Base entrenamiento

table(train$Pobre)
#(Pobre == 0) corresponde a alrededor de 80% de las observaciones
#(Pobre == 1) corresponde a alrededor de 20% de las observaciones
#Coclusiones de cada 10 observaciones 8 son no pobres y 20 son pobres -> hay un desbalance moderado 


##Estrategia para corregir el desbalance --------#####

#Hibrido: up y down sample 

#(i) primero upsmaple la clase minoritaria usando upsample -> dejarla en 66048 (usando SMOTE)
#(ii) segundo downsample la clase mayoritaria usando downsample -> dejarla en 92355
#(iii) unir las dos clases 


#Usar pesos en las observaciones 



#Método de validación cruzada (k-fold cv)---------------------------------------


##Métricas de interés ------------ ####

fiveStats <- function(...) {
  c(
    caret::twoClassSummary(...), # Returns ROC, Sensitivity, and Specificity
    caret::defaultSummary(...)  # Returns RMSE and R-squared (for regression) or Accuracy and Kappa (for classification)
  )
}

##Validación cruzada (5-fold cv) --------------####
#Usamos 5 folds porque es un número en nuestra regla del pulgar y además divide la 
#muestra en este caso en 5 grupos de exactamente el mismo tamaño 

ctrl<- trainControl(method = "cv",
                    number = 5,
                    summaryFunction = fiveStats,
                    classProbs = TRUE,
                    verbose=FALSE,
                    savePredictions = T)



#Estimar Elastic-Net para identificar variables --------------------------------

set.seed(91519) # important set seed. 



##Hiperparámetros -----------####

#En este caso los hiperparámetros que vamos a escoger son 
#    (i) lambda: complejidad del modelo. Controlo que tan que van a encoger los coeficientes 
#    (ii) alpha: mezcla entre la penalidad de laso (alpha == 1) y la de ridge (alpha = 0 )



ElasticNet_grid <- expand_grid( 
                    lambda = seq(0, 0.1, by = 0.01),
                    alpha = seq(0, 1, by = 0.01)
                                )


lambda <- 10^seq(1, -4, length = 100)  # Genera una secuencia de valores de lambda para la regularización
ElasticNet_mini_grid <- expand.grid("alpha" = seq(0,1,by=0.25), lambda = lambda) 


  

  
##Entrenamiento del módelo ---------####



###Elastic Net 1 #### 



elastic_net <- train(
                      Pobre ~ Dominio + cabecera + prop_vivienda + Depto + Ncuartos + 
                        Ncuartos_duermen + credit_vivienda_mes + arriendo_hipotetico + 
                        arriendo + Npersonas + Nper_unidad_gasto + linea_indigencia + 
                        linea_pobreza + factor_exp + factor_ex_dep + t_prima_servicios + 
                        t_prima_navidad + t_prima_vacaciones,  # Construcción de la fórmula del modelo
                        method = "glmnet",  # Usa glmnet para regresión con regularización (EN)
                        data = train,  # Usa los datos de entrenamiento
                        family = "binomial",  # Es un modelo logístico (para clasificación binaria)
                        tuneGrid = ElasticNet_mini_grid,  # Especifica la grilla de hiperparámetros
                        preProcess = c("center", "scale")  # Normaliza las variables predictoras,
                        
)

###Elastic Net 2 #### 


elastic_net_2 <- train(
                      Pobre ~ Dominio + Ncuartos_duermen + arriendo + linea_pobreza + t_bonificaciones_anuales + t_microempresa + menor_15 + segur_subsidiado 
                      + cabecera + prop_vivienda + mayor_60 + P_Ed_Superior + Desempleados + credit_vivienda_mes + edad + t_cotiza_pension + p_ocupados
                      + p_gran_empresa + p_trabaja_solo + mujer + segur_social + Ncuartos,  # Construcción de la fórmula del modelo
                        method = "glmnet",  # Usa glmnet para regresión con regularización (EN)
                        trControl = ctrl,
                        data = train,  # Usa los datos de entrenamiento
                        family = "binomial",  # Es un modelo logístico (para clasificación binaria)
                        tuneGrid = ElasticNet_mini_grid,  # Especifica la grilla de hiperparámetros
                        preProcess = c("center", "scale")  # Normaliza las variables predictoras,

)




#Estimar el módelo (AdaBoost) --------------------------------------------------
set.seed(91519) # important set seed. 

##Hiperparámetros ------------####

#En este caso los hiperparámetros que vamos a escoger son 
#   (i) mfinal: número de árboles 
#   (ii) maxdepth: profundidad de los árboles 
#   (iii) coeflearn: forma funcional de alpha (que es el peso de los clasificadores)
#                     probar las dos formas funcionales




adagrid<-  expand.grid(
                      mfinal = c( 50, 100),
                      maxdepth = c(4,6,8), 
                      coeflearn = c("Freund"))

adagrid_mini<-  expand.grid(
                      mfinal = c( 50),
                      maxdepth = c(4), 
                      coeflearn = c('Freund'))


##Entrenamiento del módelo ----------####
adaboost_tree <- train(Pobre ~ Dominio + Ncuartos_duermen + arriendo + linea_pobreza + t_bonificaciones_anuales +
                         t_microempresa + menor_15 + segur_subsidiado  + cabecera + prop_vivienda + mayor_60 + P_Ed_Superior
                       + Desempleados + credit_vivienda_mes + edad + t_cotiza_pension + p_ocupados
                       + p_gran_empresa + p_trabaja_solo + mujer + segur_social + Ncuartos,  #Poner variables con mayor capacidad explicativa 
                       data = train, 
                       method = "AdaBoost.M1",  # para implementar el algoritmo antes descrito
                       trControl = ctrl,
                       metric = "F1",
                       tuneGrid=adagrid
)

adaboost_tree




#==============================Playground=======================================





