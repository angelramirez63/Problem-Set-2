
#============================= Problem Set 2 ===================================



#0. Preparación ambiente de trabajo --------------------------------------------

##Limpiar ambiente####

rm(list = ls())
cat("\014")

##Librerías####

if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, 
       dplyr, gridExtra)


##Crear el directorio#### 
wd <- here()
setwd(wd)
rm(wd)

#2.1. Cargar datos ------------------------------------------------------------
 
##Ejemplo Kaggle####
sample_sub <- read.csv("stores/sample_submission.csv") %>% 
  as_tibble()

##Testeo#### 
test_hogares <- read.csv("stores/test_hogares.csv") %>% 
  as_tibble()

test_personas <- read.csv("stores/test_personas.csv") %>% 
  as_tibble()


test_full <- left_join(test_personas, test_hogares, by = "id")

##Entrenamiento####
train_hogares <- read.csv("stores/train_hogares.csv") %>% 
  as_tibble()

train_personas <- readRDS("stores/train_personas.rds") %>% 
  as_tibble() #Por el peso del archivo se convirtió a rds

train_full <- left_join(train_personas, train_hogares, by = "id")


#2.2. Limpieza de datos ---------------------------------------------------------

rm(list = ls())
db <- readRDS("stores/datos_GEIH.rds") %>% 
  as_tibble()

#i) la condicción para conservar la observación es:  age == edad_personas , ocu === dummy_si_la_persona esta ocupada

db_limpia <- db %>% 
  filter(age >= 18 & ocu == 1) 

"El método que usamos da lo mismo que el que usaron en la complementaria:
db_limpia2 <- db %>% filter(totalHoursWorked>0)"

#Eliminamos las observaciones que tienen valores faltantes en neustra variable de resultado

db_nas <- db_limpia %>% filter(is.na(y_ingLab_m_ha))
db_limpia <- db_limpia %>% filter(!is.na(y_ingLab_m_ha))

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


