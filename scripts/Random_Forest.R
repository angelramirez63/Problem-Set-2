#Random Forest  ----------------------------------------------------------------

## Ángel y Juan Pablo

## Cargar datos ----------------------------------------------------------------

db <- readRDS('db_final.rds') %>% 
  as_tibble()

# Modelo -----------------------------------------------------------------------


head(db)


#Crear base train

trainRF <- db %>% 
  filter(test == 0) %>% 
  select(-id) %>% 
  select(-test)

#Crear base test

testRF <- db %>% 
  filter(test == 1) %>% 
  select(-id) %>% 
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

#Ideas: Usar prop o conteo

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
  num.trees= 500, ## Numero de bootstrap samples y arboles a estimar. Default 500  
  mtry= 8,   # N. var aleatoriamente seleccionadas en cada partición
  min.node.size  = 1, ## Numero minimo de observaciones en un nodo
  importance="impurity",
  splitrule = 'gini') 
rf

