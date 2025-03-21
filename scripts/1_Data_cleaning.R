
#============================= Problem Set 2 ===================================



#0. Preparación ambiente de trabajo --------------------------------------------

##Limpiar ambiente####

rm(list = ls())
cat("\014")

##Librerías####

if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, 
       dplyr, gridExtra, corrplot)


##Definir el directorio#### 
wd <- here()
setwd(wd)
rm(wd)

#1 Cargar datos ----------------------------------------------------------------
 
##Ejemplo Kaggle####
sample_sub <- read.csv("stores/sample_submission.csv") %>% 
  as_tibble()

##Testeo#### 
test_hogares <- read.csv("stores/test_hogares.csv") %>% 
  as_tibble()

test_personas <- read.csv("stores/test_personas.csv") %>% 
  as_tibble()

test_full <- left_join(test_personas, test_hogares, by = "id")

#Exportar base de testeo 
export(test_full, 'stores/test_full.rds')


##Entrenamiento####
train_hogares <- read.csv("stores/train_hogares.csv") %>% 
  as_tibble()

train_personas <- readRDS("stores/train_personas.rds") %>% 
  as_tibble() #Por el peso del archivo se convirtió a rds

train_full <- left_join(train_personas, train_hogares, by = "id")

#Exportar base de entrenamiento 
export(train_full, 'stores/train_full.rds')


#2 Exploración base de entrenamiento -------------------------------------------

rm(list = ls())
train_full <- readRDS("stores/train_full.rds")


##Exploración####

train_skim <- skim(train_full) #Exploración inicial de la estructura de la base 
train_skim
head(train_full) #Visualizar 5 primeras filas
table(train_full$Pobre) #Tabular variables de resultado 

##Resultados exploración####

#i)Número de observaciones: 543109 personas que pertenecen a un hogar 
#ii)Número de variables: 157 variables previó a la limpieza
#iii) Tipos de las variables: hay 3 de tipo str y 154 númericas previó a la limpieza
#     hay variables que son de tipo númericas que probablemente son categóricas 
#iv) la clase Pobre esta desbalanceada 



#3 Limpieza de datos -----------------------------------------------------------



##Parte 1 - Limpieza general####


###Variables con todas las observaciones###

full_at_100 <- train_skim %>% 
  filter(complete_rate == 1) # Hay 30 variables que están completas al 100%

###Eliminar variables sin mucha información####

#Remover variables que solo tienen missing values o no tienen variación 
train_clean <- train_full %>% 
               select_if(~ !all(is.na(.)) & length(unique(.))>1) # Ninguna variable cumple esta condición 

#Proporción de missings por variable 
missings <- train_skim %>% 
            select(skim_variable, n_missing, complete_rate) %>% 
            filter(complete_rate != 1)


#Visualización de la proporición de missings por variable 
#Alrededor de la 50% de la variables tienen un complete rate menos a 0,25
ggplot(missings, aes(x = reorder(skim_variable, +complete_rate) , y =  complete_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "Complete Rate", x = "Nombre variable", y = "Proporción de no nulos")+ 
  theme(axis.text = element_text(size = 5))  


#Remover variables que tiene el menos del 0.25 de complete rate o menos: 
#son 86 variables que cumplen esta condición 
missings_75 <- missings %>% 
               filter(complete_rate <= 0.25)

#Las variables que se van a remover son las que están en missings_75
train_clean <- train_clean %>% 
               select(-P6500, -P6510, -P6510s1, -P6510s2, -P6545, -P6545s1, 
                      -P6545s2, -P6580, -P6580s1, -P6580s2, -P6585s1, -P6585s1a1, 
                      -P6585s1a2, -P6585s2, -P6585s2a1, -P6585s2a2, -P6585s3, -P6585s3a1, 
                      -P6585s3a2, -P6585s4, -P6585s4a1, -P6585s4a2, -P6590, -P6590s1, -P6600, 
                      -P6600s1, -P6610, -P6610s1, -P6620, -P6620s1, -P6630s1, -P6630s1a1, -P6630s2, 
                      -P6630s2a1, -P6630s3, -P6630s3a1, -P6630s4, -P6630s4a1, -P6630s6, -P6630s6a1, 
                      -P6750, -P6760, -P550, -P7045, -P7050, -P7110, -P7120, -P7140s1, -P7140s2, -P7150, 
                      -P7160, -P7310, -P7350, -P7422, -P7422s1, -P7500s1, -P7500s2, -P7500s3, -P7510s1, 
                      -P7510s2, -P7510s3, -P7510s5, -P7510s6, -P7510s7, -Des, -Ie, -Imdi, -Cclasnr2, -Cclasnr3, 
                      -Cclasnr4, -Cclasnr5, -Cclasnr6, -Cclasnr7, -Cclasnr8, -Cclasnr11, -Impaes, -Isaes, -Iees, 
                      -Imdies, -Iof1es, -Iof2es, -Iof3hes, -Iof3ies, -Iof6es, -Ingtotes, -P5100) 




##Parte 2 - Limpieza granular####


###Imputar missings en las variables con más del 60% de observaciones####

#Visulizar cuales variables cumplen esta condición: 
train_clean_skim <- skim(train_clean) %>% filter(complete_rate != 1)

ggplot(train_clean_skim, aes(x = reorder(skim_variable, +complete_rate) , y =  complete_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "Complete Rate", x = "Nombre variable", y = "Proporción de no nulos")+ 
  theme(axis.text = element_text(size = 5))  


full_at_60 <- train_clean_skim %>% 
  filter(complete_rate >= 0.6) # Hay 24 variables que están completas al 75% o y 1 que tiene aproximadamente 63,5% 

#Nota: de las 71 variables de la base limpia hay 55 que tienen al menos el 63,5% de las observaciones completas de estas 30 están completas al 100%

        
#Visisualizar los valores faltantes de estás variables: 
#las variables que se van a conservar son las que están full_at_60

missings_full_at_60 <- train_clean %>% 
                        select(P6090, P6100, P6210, P6210s1, P6240, P7495, P7500s1a1, 
                               P7500s2a1, P7500s3a1, P7505, P7510s1a1, P7510s2a1, 
                               P7510s3a1, P7510s5a1, P7510s6a1, P7510s7a1, Pet, Iof1, 
                               Iof2, Iof3h, Iof3i, Iof6, Ingtotob, Ingtot, P5130, Pobre)


#De las 25 variables 22 tienes el mismo número de missings values que son 95450
#¿Habra personas que no respondieron nada en ninguna de las preguntas? -> ¿Habra filas que son solo missings?
skim(missings_full_at_60)




##Variables Categorícas a imputar: 

#P6090 - seguridad social:
table(train_clean$Pobre, train_clean$P6090) # Los Pobres reportan en su mayoría estar afiliados a seguridad social > Categoría más común 

#P6100 - regimen de seguridad social: 
table(train_clean$Pobre, train_clean$P6090) # Los Pobres reportan en su mayoría estar afiliados al regimen subsidiado -> Categoría más común 

#P6210 - máximo nivel educativo categoríco: 
table(train_clean$Pobre, train_clean$P6210) # Los Pobres tiene en su mayoría hasta educación media es decir de 6to a 9to -> Crear Categoría max9to e imputar con esa

#P6210s1 - máximo grado alcanzado: 
# Se puede usar para imputar la información de la variable P6210 -> Imputar con valor alguno de los extremos del intervalo alcanzado o pensar otra forma 

#P6240 - actividad que ocupo más tiempo: 
table(train_clean$Pobre, train_clean$P6240) # Los Pobres usaron la mayor parte de su tiempo trabajando, en oficios del hogar o estudiando -> se podría imputar usando la categoría más común condicional a la edad(P6040) y al género(P6020)
#Si es mujer y no reporta ingreso imputar como oficio del hogar(4), si sí lo reporta imputar como trabajando(1)  
#Si las personas tienen 20 años o menos es probable que estén estudiando de los 21 para arriba es más probable que las personas estén trabajando 

#P7495 - Recibio arriendo a pensiones: 
table(train_clean$Pobre, train_clean$P7495) # Los Pobres no reciben arriendo o pensiones -> imputar catergortía más común para todas las observaciones

#P7500s1a1 - Valor recibido por arriendo: 
table(train_clean$P7500s1a1, train_clean$Pobre) # Los Pobres en promedio no reciben arriendo -> imputar con cero si la persona es Pobre, pensar que hacer cuando no es pobre -> La mayoría de las personas no reciben un arriendo 

#P7500s2a1 - Valor recibido por pensión: 
table(train_clean$P7500s2a1, train_clean$Pobre) # Los Pobres en promedio no reciben pensión -> imputar con cero si la persona es Pobre,  pensar que hacer cuando no es pobre  -> La mayoría de las personas no reciben pensión 

#P7500s3a1 - Recibio pensión alimentaria por divorcio o separación: 
table(train_clean$P7500s3a1, train_clean$Pobre) #Los Pobres en promedio no recibne pensión alimentaria -> imputar con cero -> la mayoría de personas no reciben couta alimentaria 


#P7505 - Recibio dinero de alguin que no sea el gob: 
table(train_clean$P7505, train_clean$Pobre) #Imputar con la categoría más común 



#===============================================================================

#===============================================================================

# Eliminamos las variables para las cuales más del 60% de las observaciones son faltantes
missing_percent <- colMeans(is.na(db_limpia)) * 100
db_limpia <- db_limpia[, missing_percent <= 60]

#Eliminar variables de solo missings o que no tienen variación

db_limpia <- db_limpia %>% select_if(~ !all(is.na(.)) & length(unique(.))>1) %>%
  select(!directorio, !secuencia_p, !orden)

#Imputar variables categoricas con la categoría más común:

#Función para calcular la moda (el valor más frecuente)
calcular_moda <- function(x) {
  tabla <- table(x)
  moda <- names(tabla)[which.max(tabla)]
  return(moda)
}

db_limpia <- db_limpia %>%
  group_by(formal, estrato1) %>%
  mutate(
    p6100 = ifelse(is.na(p6100) | p6100 == 9, as.numeric(calcular_moda(p6100)), p6100),
    p6510 = ifelse(is.na(p6510) | p6510 == 9, as.numeric(calcular_moda(p6510)), p6510),
    p6545 = ifelse(is.na(p6545) | p6545 == 9, as.numeric(calcular_moda(p6545)), p6545),
    p6580 = ifelse(is.na(p6580) | p6580 == 9, as.numeric(calcular_moda(p6580)), p6580),
    p6585s1 = ifelse(is.na(p6585s1) | p6585s1 == 9, as.numeric(calcular_moda(p6585s1)), p6585s1),
    p6585s2 = ifelse(is.na(p6585s2) | p6585s2 == 9, as.numeric(calcular_moda(p6585s2)), p6585s2),
    p6585s3 = ifelse(is.na(p6585s3) | p6585s3 == 9, as.numeric(calcular_moda(p6585s3)), p6585s3),
    p6585s4 = ifelse(is.na(p6585s4) | p6585s4 == 9, as.numeric(calcular_moda(p6585s4)), p6585s4),
    p6590 = ifelse(is.na(p6590) | p6590 == 9, as.numeric(calcular_moda(p6590)), p6590),
    p6600 = ifelse(is.na(p6600) | p6600 == 9, as.numeric(calcular_moda(p6600)), p6600),
    p6610 = ifelse(is.na(p6610) | p6610 == 9, as.numeric(calcular_moda(p6610)), p6610),
    p6620 = ifelse(is.na(p6620) | p6620 == 9, as.numeric(calcular_moda(p6620)), p6620),
    p6090 = ifelse(is.na(p6090) | p6090 == 9, as.numeric(calcular_moda(p6090)), p6090),
    p6210 = ifelse(is.na(p6210) | p6210 == 9, as.numeric(calcular_moda(p6210)), p6210)
  ) %>%
  ungroup()

#Ahora realizamos lo mismo, pero con las variables categóricas faltantes
db_limpia <- db_limpia %>%
  group_by(formal, estrato1) %>%
  mutate(
    p6630s1 = ifelse(is.na(p6630s1), as.numeric(calcular_moda(p6630s1)), p6630s1),
    p6630s2 = ifelse(is.na(p6630s2), as.numeric(calcular_moda(p6630s2)), p6630s2),
    p6630s3 = ifelse(is.na(p6630s3), as.numeric(calcular_moda(p6630s3)), p6630s3),
    p6630s4 = ifelse(is.na(p6630s4), as.numeric(calcular_moda(p6630s4)), p6630s4),
    p6630s6 = ifelse(is.na(p6630s6), as.numeric(calcular_moda(p6630s6)), p6630s6),
    p6920 = ifelse(is.na(p6920), as.numeric(calcular_moda(p6920)), p6920),
    maxEducLevel = ifelse(is.na(maxEducLevel), as.numeric(calcular_moda(maxEducLevel)), maxEducLevel),
    regSalud = ifelse(is.na(regSalud), as.numeric(calcular_moda(regSalud)), regSalud),
    cotPension = ifelse(is.na(cotPension), as.numeric(calcular_moda(cotPension)), cotPension)
  ) %>%
  ungroup()

# Lista de variables a procesar. Se omitieron variables que estaban muy 
#concentrados en cero y hacian que el límite superior tuviera este mismo valor
vars <- c(
  "p6500", "p6585s2a1", "p6585s3a1", "p6590s1", "p6630s1a1", "p6630s2a1", 
  "p6630s3a1", "p6630s4a1", "p7070", "impa", "isa", "ie", "y_salary_m", "y_salary_m_hu",
  "y_primaServicios_m", "y_total_m", "y_total_m_ha", "y_ingLab_m", "y_ingLab_m_ha"
)

# Aplicar el proceso a cada variable en el loop
for (var in vars) {
  # Calcular el percentil 97.5% de la variable
  up <- quantile(db_limpia[[var]], 0.975, na.rm = TRUE)
  
  # Reemplazar valores mayores o iguales al percentil 97.5%
  db_limpia <- db_limpia %>%
    mutate(!!sym(var) := ifelse(test = (.data[[var]] >= up), 
                                yes = up, 
                                no = .data[[var]]))
}

#### Para no tener problemas con las variables de entrenamiento y testeo, eliminamos las observaciones cuyos valores agrupados son menores a 5
db_limpia <- db_limpia %>%
  group_by(oficio) %>%
  filter(n() >= 5) %>%
  ungroup()

db_limpia <- db_limpia %>%
  group_by(p6050) %>%
  filter(n() > 3) %>%
  ungroup()

db_limpia <- db_limpia %>% 
  filter(p6210s1<99)
#Borramos el las observaciones influyentes en la relación edad/salario

modelo0 <- lm(y_ingLab_m_ha ~ age + I(age^2), data = db_limpia)
db_limpia <- db_limpia %>% mutate(leverage = hatvalues(modelo0))
db_limpia <- db_limpia %>% 
  filter(leverage<0.005) %>%
  select(-leverage)

#Categorías variables 

db_limpia <- db_limpia %>%
  mutate(p7040 = ifelse(p7040 == 2, 0, 1))

#Convertir variables categoricas en factores

db_limpia <- db_limpia %>%
  mutate(across(c(college, cotPension, formal, informal, microEmpresa, regSalud,
                  sex, estrato1, maxEducLevel, oficio, p6050, p6090, p6100, p6210, 
                  p6240, p6545, p6580, p6585s1, p6585s2, p6585s3, p6585s4, 
                  p6590, p6600, p6610, p6620, p6630s1, p6630s2, p6630s3,p6630s4, 
                  p6630s6, p6920, p7040, p7505, regSalud, relab, sizeFirm, p7510s1,
                  p7510s2, p7510s3, p7510s5, p7510s6, p7510s7, p7495, p7090), as.factor))

#Renombramos variables para facilitar el manejo de datos
db_limpia <- db_limpia %>% rename(parentesco_jefe = p6050, segur_social = p6090, 
                                  regim_segur_social =p6100, nivel_educ = p6210, 
                                  grad_aprob = p6210s1, actividad_prin = p6240, 
                                  tiempo_trabaj = p6426, cotiza_pens = p6920, 
                                  ingreso_laboral = p6500, segundo_trabajo = p7040, 
                                  recibe_ing_hor_ext = p6510, ingreso_hor_ext = p6510s1)


#Utilizamos K-Nearest Neighbour para imputar los missings que quedan
db_limpia <- kNN(db_limpia)
"kNN se demora mucho, entonces vale la pena hacer otras aproximaciones a la imputación
de variables primero y luego llenar las que faltan usando kNN"
missing_percent2 <- colMeans(is.na(db_limpia)) * 100
db_limpia <- db_limpia %>% select(!ends_with("_imp"))

#Creamos la variable de resultado: el logarítmo natural del salario
db_limpia$ln_sal <- log(db_limpia$y_ingLab_m_ha) 

#Creamos la variable female
db_limpia <- db_limpia %>% 
  mutate(female = ifelse(sex == 0, yes = 1 , no = 0)) %>%
  select(-sex)

db_limpia$female <- as.factor(db_limpia$female)

#Guardamos los datos
export(db_limpia, 'stores/datos_modelos.rds')


