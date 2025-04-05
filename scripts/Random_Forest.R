#Random Forest  ----------------------------------------------------------------

## Ángel y Juan Pablo

## Cargar datos ----------------------------------------------------------------

db <- readRDS('db_final.rds') %>% 
  as_tibble()

test_hogares <- read.csv("test_hogares.csv") %>% 
  as_tibble()

# Modelo -----------------------------------------------------------------------


head(db)

Pobre_num <- db$Pobre


#Crear base train

trainRF <- db %>% 
  filter(test == 0) %>% 
  select(-test)

#Crear base test

testRF <- db %>% 
  filter(test == 1) %>% 
  select(-test)

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(trainRF))
missing_tab<-data.frame(
  Miss_val=missing_values
)
print(missing_tab)



trainRF$Pobre <- as.factor(trainRF$Pobre)
trainRF$Depto <- as.factor(trainRF$Depto)
trainRF$cabecera <- as.factor(trainRF$cabecera)
trainRF$prop_vivienda <- as.factor(trainRF$prop_vivienda)
trainRF$Dominio <- as.factor(trainRF$Dominio)

#Definir control

fitControl <- trainControl(method = 'cv', number = 10)

#Ideas: Usar prop o conteo

#MODELO ------------------------------------------------------------------------

set.seed(1112) #Se fija semilla para reproducibilidad

rf<- ranger::ranger(
  Pobre ~ cabecera + Dominio + Ncuartos + Ncuartos_duermen + prop_vivienda + 
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
    educ_sup + tiempo_empresa, 
  data = trainRF,
  method = 'ranger',
  trainControl = fitControl,
  tuneGrid = expand.grid(
    num.trees = seq(300, 1000, by = 100),   # 300, 400, ..., 800
    mtry = 8:20,  
    splitrule = "gini", 
    min.node.size = c(1, 5, 10, 50, 100, 200, 300, 500, 1000)
  )
)
rf

# Calculamos las predicciones
Pobre_hat <-predict(rf, data = testRF, predict.all = TRUE)$predictions
pred.rf <- as.data.frame(Pobre_hat)

# Calcular las probabilidades de Default 
ntrees <- ncol( pred.rf ) 
phat.rf <- rowSums(pred.rf == 2) / ntrees

aucval_rf <- Metrics::auc(
  actual = Pobre_num[-inTrain],
  predicted = phat.rf
)
aucval_rf






