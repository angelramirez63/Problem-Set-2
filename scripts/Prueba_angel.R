#### Preparación
library(pacman)

p_load(tidyverse, here, glmnet, caret, Mlmetrics, Metrics)

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
    Indigente = as.factor(Indigente),
    Depto = as.factor(Depto)
  )

# Gráfico para visualizar la distribución de Pobres
ggplot(train_hogares, aes(x = Pobre, fill = Pobre)) +
  geom_bar() + 
  theme_minimal() +
  scale_fill_manual(values = c("Pobre" = "orange", "Otros"= "blue")) +
  labs(x = "", y = "# de Personas")

##(Revisar calsificación DANE)

# Dejar las mismas variables que test
train_hogares <- train_hogares %>%
  select(-Ingtotug, -Ingtotugarr, 
         -Ingpcug, -Indigente, 
         -Npobres, -Nindigentes, -P5130, Fex_c)

# Renombrar variables
train_hogares <- train_hogares %>% 
  rename(cabecera = Clase,
         Ncuartos = P5000,
         Ncuartos_duermen = P5010,
         vivienda = P5090,
         Npersonas = Nper,
         Nper_unidad_gasto = Npersug,
         linea_indigencia = Li,
         linea_pobreza = Lp
  )

##------Limpieza test_hogares------##

# Eliminar variables irrelevates
test_hogares <- test_hogares %>%
  select(-P5130, -Fex_c)

# Renombrar variables
test_hogares <- test_hogares %>% 
  rename(cabecera = Clase,
         Ncuartos = P5000,
         Ncuartos_duermen = P5010,
         vivienda = P5090,
         amortizacion = P5100,
         arriendo = P5140,
         Npersonas = Nper,
         Nper_unidad_gasto = Npersug,
         linea_indigencia = Li,
         linea_pobreza = Lp
  )

##------Limpieza train_personas------##

train_personas  %>% 
  select(id, Orden)  %>%
  head()

# Creamos la variable female
train_personas <- train_personas %>% 
  mutate(female = ifelse(P6020 == 0, yes = 1 , no = 0)) %>%
  select(-P6020)

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(train_personas))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab

# Eliminar variables con más del 60% de variables como missing values
missing_percent <- colMeans(is.na(train_personas)) * 100
db <- train_personas[, missing_percent <= 60]

# Observar los tipos de variable
tipo_variables <- data.frame(
  Variable = names(train_hogares),
  Tipo = sapply(train_hogares, class)
)
print(tipo_variables)

##------Limpieza test_personas------##

test_personas  %>% 
  select(id, Orden)  %>%
  head()

# Creamos la variable female
test_personas <- test_personas %>% 
  mutate(female = ifelse(P6020 == 0, yes = 1 , no = 0)) %>%
  select(-P6020)

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(test_personas))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab

# Observar los tipos de variable
tipo_variables <- data.frame(
  Variable = names(train_hogares),
  Tipo = sapply(train_hogares, class)
)
print(tipo_variables)

# Eliminar variables con más del 60% de variables como missing values
missing_percent <- colMeans(is.na(test_personas)) * 100
df <- test_personas[, missing_percent <= 60]

