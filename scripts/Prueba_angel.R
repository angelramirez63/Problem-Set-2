#### Preparación
library(pacman)

p_load(tidyverse,
       here,
       skimr,
       VIM,
       glmnet,    # Modelos de regresión regularizados (EN, Lasso y Ridge).
       caret,
       MLmetrics, # Calcular metricas
       MLeval,    # Evaluar modelos de clasificación
       Metrics
)

rm(list = ls())

# Crear el directorio 
wd <- here()
setwd(wd)
rm(wd)

# Cargar los datos
sample_sub <- read.csv("sample_submission.csv") %>% 
  as_tibble()

test_hogares <- read.csv("test_hogares.csv") %>% 
  as_tibble()

train_hogares <- read.csv("train_hogares.csv") %>% 
  as_tibble()

test_personas <- read.csv("test_personas.csv") %>% 
  as_tibble()

train_personas <- readRDS("train_personas.rds") %>% 
  as_tibble() #Por el peso del archivo se convirtió a rds


#-----------------------------------------------------------------------------#

##------Limpieza train_hogares------##

train_hogares  %>% 
  select(id)  %>%
  head()

#Revisar calsificación DANE
table(train_hogares$Pobre)

train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand)

train_hogares<- train_hogares %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<Lp*Npersug,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand_2)

train_hogares <- train_hogares %>%
  select(-Pobre_hand, -Pobre_hand_2)

# Dejar las mismas variables que test
train_hogares <- train_hogares %>%
  select(-Ingtotug, -Ingtotugarr, 
         -Ingpcug, -Indigente, 
         -Npobres, -Nindigentes)

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(train_hogares))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab

train_hogares <- train_hogares %>% 
  select(-P5100, -P5140) # Eliminamos variables con más de 60% en missing values

# Observar los tipos de variable
tipo_variables <- data.frame(
  Variable = names(train_hogares),
  Tipo = sapply(train_hogares, class)
)
print(tipo_variables)

# Convertir variables categóricas en factores
train_hogares <- train_hogares %>%
  mutate(
    P5090 = as.factor(P5090),
    Clase = as.factor(Clase),
    Pobre = factor(Pobre, levels = c(1,0), labels = c("Pobre","Otros")),
    Depto = as.factor(Depto)
  )

# Gráfico para visualizar la distribución de Pobres
ggplot(train_hogares, aes(x = Pobre, fill = Pobre)) +
  geom_bar() + 
  theme_minimal() +
  scale_fill_manual(values = c("Pobre" = "orange", "Otros"= "blue")) +
  labs(x = "", y = "# de Personas")

# Renombrar variables
train_hogares <- train_hogares %>% 
  rename(cabecera = Clase,
         Ncuartos = P5000,
         Ncuartos_duermen = P5010,
         vivienda = P5090,
         Npersonas = Nper,
         Nper_unidad_gasto = Npersug,
         linea_indigencia = Li,
         linea_pobreza = Lp,
         factor_exp = Fex_c,
         factor_ex_dep = Fex_dpto
  )

#-----------------------------------------------------------------------------#

##------Limpieza test_hogares------##

test_hogares  %>% 
  select(id)  %>%
  head()

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(test_hogares))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab

test_hogares <- test_hogares %>% 
  select(-P5100, -P5140) # Eliminamos variables con más de 60% en missing values

# Convertir variables categóricas en factores
test_hogares <- test_hogares %>%
  mutate(
    P5090 = as.factor(P5090),
    Clase = as.factor(Clase),
    Depto = as.factor(Depto)
  )

# Renombrar variables
test_hogares <- test_hogares %>% 
  rename(cabecera = Clase,
         Ncuartos = P5000,
         Ncuartos_duermen = P5010,
         vivienda = P5090,
         Npersonas = Nper,
         Nper_unidad_gasto = Npersug,
         linea_indigencia = Li,
         linea_pobreza = Lp,
         factor_exp = Fex_c,
         factor_ex_dep = Fex_dpto
  )

#-----------------------------------------------------------------------------#

##------Limpieza train_personas------##

train_personas  %>% 
  select(id, Orden)  %>%
  head()

# Creamos la variable female
train_personas <- train_personas %>% 
  mutate(female = ifelse(P6020 == 2, yes = 1 , no = 0)) %>%
  select(-P6020)

# Dejar las mísmas variables que la base test personas
train_personas <- train_personas %>%
  select(-Estrato1, -P6500, -P6510s1, -P6510s2, -P6545s1, -P6545s2, -P6580s1, -P6580s2, 
         -P6585s1a1, -P6585s1a2, -P6585s2a1, -P6585s2a2, -P6585s3a1, -P6585s3a2, -P6585s4a1, -P6585s4a2, 
         -P6590s1, -P6600s1, -P6610s1, -P6620s1, -P6630s1a1, -P6630s2a1, -P6630s3a1, -P6630s4a1, 
         -P6630s6a1, -P6750, -P6760, -P550, -P7070, -P7140s1, -P7140s2, -P7422s1, -P7472s1, 
         -P7500s1, -P7500s1a1, -P7500s2a1, -P7500s3a1, -P7510s1a1, -P7510s2a1, -P7510s3a1, -P7510s5a1, 
         -P7510s6a1, -P7510s7a1, -Impa, -Isa, -Ie, -Imdi, -Iof1, -Iof2, -Iof3h, -Iof3i, 
         -Iof6, -Cclasnr2, -Cclasnr3, -Cclasnr4, -Cclasnr5, -Cclasnr6, -Cclasnr7, -Cclasnr8, 
         -Cclasnr11, -Impaes, -Isaes, -Iees, -Imdies, -Iof1es, -Iof2es, -Iof3hes, -Iof3ies, 
         -Iof6es, -Ingtotob, -Ingtotes, -Ingtot)

# Observar si los hogares son los mismos en las dos bases
train_personas_grouped <- train_personas %>%
  group_by(id) %>%
  summarise(n_personas = n())  # Cuenta el número de personas por id
all(sort(train_personas_grouped$id) == sort(train_hogares$id)) # Ver si contienen los mismos elementos, sin importar el orden

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(train_personas))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab

# Calcular el porcentaje de valores faltantes por variable
missing_percent <- colMeans(is.na(train_personas)) * 100  
variables_60_NA_train <- names(missing_percent[missing_percent > 60])
print(variables_60_NA_train)

# Eliminar variables con más del 60% de variables como missing values
train_personas <- train_personas[, missing_percent <= 60]

# Observar los tipos de variable
tipo_variables <- data.frame(
  Variable = names(train_personas),
  Tipo = sapply(train_personas, class)
)
print(tipo_variables)

# Renombrar variables
train_personas <- train_personas %>% 
  rename(cabecera = Clase,
         edad = P6040,
         parentesco_jefe = P6050,
         pension = P6090,
         regim_salud = P6100,
         max_educ = P6210,
         grado_aprob = P6210s1,
         actividad_ocupada = P6240,
         antiguedad_empresa = P6426,
         posicion_empresa = P6430,
         horas_trabajo = P6800,
         tamaño_empresa = P6870,
         cotiza_pension = P6920,
         otro_trabajo = P7040,
         horas_extra = P7090,
         arriendo_o_pension = P7495,
         ingreso_extra = P7505,
         ocupado = Oc,
         factor_exp = Fex_c,
         factor_ex_dep = Fex_dpto
  )


# Reconstruir variables
train_personas <- train_personas %>%
  mutate(arriendo_o_pension = ifelse(arriendo_o_pension == 2, 0, arriendo_o_pension),
         cotiza_pension = ifelse(cotiza_pension %in% c(1, 3), 1, 0), # 1 si cotiza o es pensionado
         regim_salud = ifelse(regim_salud == 3, 1, 0) # 1 si es subsidiado
         )

# Agrupar poe hogar
train_personas <- train_personas %>%
  group_by(id) %>%
  summarise(
    max_educ = sum(max_educ, na.rm = TRUE),
    grado_aprob = sum(grado_aprob, na.rm = TRUE),
    antiguedad_empresa = sum(antiguedad_empresa, na.rm = TRUE),
    horas_trabajo = sum(horas_trabajo, na.rm = TRUE),
    cotiza_pension = sum(cotiza_pension, na.rm = TRUE),
    Pet = sum(Pet, na.rm = TRUE),
    ocupado = sum(ocupado, na.rm = TRUE),
    female = sum(female, na.rm = TRUE),
    regim_salud = sum(regim_salud, na.rm = TRUE)
  )

#-----------------------------------------------------------------------------#

##------Limpieza test_personas------##

test_personas  %>% 
  select(id, Orden)  %>%
  head()

# Creamos la variable female
test_personas <- test_personas %>% 
  mutate(female = ifelse(P6020 == 2, yes = 1 , no = 0)) %>%
  select(-P6020)

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(test_personas))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab

# Calcular el porcentaje de valores faltantes por variable
missing_percent <- colMeans(is.na(test_personas)) * 100  
variables_60_NA_test <- names(missing_percent[missing_percent > 60])
print(variables_60_NA_test)

# Comparar si las variables con las mismas para train y test
if (identical(sort(variables_60_NA_train), sort(variables_60_NA_test))) {
  print("Las variables con más del 60% de NA son las mismas en ambas bases.")
} else {
  print("Las variables con más del 60% de NA son diferentes entre las bases.")
}

# Observar los tipos de variable
tipo_variables <- data.frame(
  Variable = names(train_hogares),
  Tipo = sapply(train_hogares, class)
)
print(tipo_variables)

# Eliminar variables con más del 60% de variables como missing values
test_personas <- test_personas[, missing_percent <= 60]

# Renombrar variables
test_personas <- test_personas %>% 
  rename(cabecera = Clase,
         edad = P6040,
         parentesco_jefe = P6050,
         pension = P6090,
         regim_salud = P6100,
         max_educ = P6210,
         grado_aprob = P6210s1,
         actividad_ocupada = P6240,
         antiguedad_empresa = P6426,
         posicion_empresa = P6430,
         horas_trabajo = P6800,
         tamaño_empresa = P6870,
         cotiza_pension = P6920,
         otro_trabajo = P7040,
         horas_extra = P7090,
         arriendo_o_pension = P7495,
         ingreso_extra = P7505,
         ocupado = Oc,
         factor_exp = Fex_c,
         factor_ex_dep = Fex_dpto
  )

# Reconstruir variables
test_personas <- test_personas %>%
  mutate(arriendo_o_pension = ifelse(arriendo_o_pension == 2, 0, arriendo_o_pension),
         cotiza_pension = ifelse(cotiza_pension %in% c(1, 3), 1, 0), # 1 si cotiza o es pensionado
         regim_salud = ifelse(regim_salud == 3, 1, 0) # 1 si es subsidiado
  )

test_personas <- test_personas %>%
  group_by(id) %>%
  summarise(
    max_educ = sum(max_educ, na.rm = TRUE),
    grado_aprob = sum(grado_aprob, na.rm = TRUE),
    antiguedad_empresa = sum(antiguedad_empresa, na.rm = TRUE),
    horas_trabajo = sum(horas_trabajo, na.rm = TRUE),
    cotiza_pension = sum(cotiza_pension, na.rm = TRUE),
    Pet = sum(Pet, na.rm = TRUE),
    ocupado = sum(ocupado, na.rm = TRUE),
    female = sum(female, na.rm = TRUE),
    regim_salud = sum(regim_salud, na.rm = TRUE)
  )

#-----------------------------------------------------------------------------#

##------Unión de train------##

# Unir dos bases de datos por la variable "id"
base_train <- left_join(train_hogares, train_personas, by = "id")

#-----------------------------------------------------------------------------#

##------Unión de test------##

# Unir dos bases de datos por la variable "id"
base_test <- left_join(test_hogares, test_personas, by = "id")
