#### Preparación
library(pacman)
library(knitr)


p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, dplyr, stats)

p_load(tidyverse, stargazer, here, glmnet, caret, Mlmetrics, Metrics)

rm(list = ls())

# Crear el directorio 
wd <- here()

setwd("/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/stores")

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


# LIMPIEZA train_hogares -------------------------------------------------------

train_hogares  %>% 
  select(id)  %>%
  head()

# Observar la cantidad de missing values de cada variable
missing_values<-colSums(is.na(train_hogares))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab

# Identificar variables con más de 60% de missings:

threshold <- 0.6 *nrow(train_hogares)

missing_cols <- colSums(is.na(train_hogares)) > threshold

print(names(train_hogares)[missing_cols]) #Las variables con más de 60% de missings son P5100 y P5140

train_hogares <- train_hogares[, !missing_cols] # Eliminamos variables con más de 60% en missing values

colSums(is.na(train_hogares)) #Note que las variables con más de 60% de missings fueron removidas.



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
    Depto = as.factor(Depto),
    Dominio = as.factor(Dominio)
  )

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
         estimacion_arriendo = P5130
  )

#RESUMEN DE ESTADÍSICAS DESCRIPTIVAS DE VARIABLES CONTÍNUAS train_hogares ------

# Seleccionar variables numéricas
numeric_vars <- train_hogares %>% 
  select(Ncuartos, Ncuartos_duermen, estimacion_arriendo, Npersonas, Nper_unidad_gasto, 
         Ingtotug, Ingtotugarr, Ingpcug, linea_indigencia, linea_pobreza, 
         Npobres, Nindigentes, Fex_c, Fex_dpto)

# Generar tabla de estadística descriptiva con stargazer
numeric_vars <- as.data.frame(numeric_vars)

stargazer(numeric_vars, type = "text", 
          title = "Estadísticas Descriptivas de Variables Numéricas",
          digits = 2, summary.stat = c("mean", "sd", "min", "max", "median"))



#REVISAR VALORES ATIPICOS Y OTRAS IRREGULARIDADES



#RESUMEN DE ESTADÍSTICAS DESCRIPTIVAS DE VARIABLES CATEGÓRICAS train_hogares----

# Definir función para crear tablas de estadísticas descriptivas
categorical_summary <- function(data, variable, labels, var_name) {
  freq_table <- table(data[[variable]])
  prop_table <- prop.table(freq_table)
  
  summary_table <- data.frame(
    Categoría = labels,
    Cuenta = as.numeric(freq_table),
    Proporción = round(as.numeric(prop_table), 4),
    Variable = var_name
  )
  
  return(summary_table)
}

# Create summary tables for all categorical variables
summary_table_cabecera <- categorical_summary(train_hogares, "cabecera", 
                                              c("Cabecera", "Resto"), 
                                              "Cabecera")
summary_table_pobre <- categorical_summary(train_hogares, "Pobre", 
                                           c("Otros", "Pobre"), 
                                           "Clasificación del hogar según ingresos")
summary_table_vivienda <- categorical_summary(train_hogares, "vivienda", 
                                              c("Propia y pagada", "Propia, pagando", 
                                                "Arriendo / Subarriendo", "Usufructo", 
                                                "Posesión sin título", "Otra"), 
                                              "Tipo de vivienda del hogar")
summary_table_indigente <- categorical_summary(train_hogares, "Indigente", 
                                               c("No Indigente", "Indigente"), 
                                               "Son los individuos del hogar indigentes?")
summary_table_depto <- categorical_summary(
  train_hogares, "Depto",
  levels(train_hogares$Depto)  # Niveles del factor Depto
)

summary_table_dominio <- categorical_summary(
  train_hogares, "Dominio", 
  levels(train_hogares$Dominio)
)

# Combinar todas las tablas en una sola

summary_table_categorical_train_hogares <- bind_rows(
  summary_table_cabecera,
  summary_table_pobre,
  summary_table_vivienda,
  summary_table_indigente,
  summary_table_depto,
  summary_table_dominio
) %>%
  dplyr::select(Variable, Categoría, Cuenta, Proporción)  # Reorder columns

# Mostrar tabla de estadísticas descriptivas para v. categóricas
kable(summary_table_categorical_train_hogares, 
      caption = "Tabla de Estadísticas Descriptivas TRAIN HOGAR - Variables Categóricas")


# OTRAS OBSERVACIONES train_hogares --------------------------------------------

# Gráfico para visualizar la distribución de Pobres
ggplot(train_hogares, aes(x = Pobre, fill = Pobre)) +
  geom_bar() + 
  theme_minimal() +
  scale_fill_manual(values = c("Pobre" = "orange", "Otros"= "blue")) +
  labs(x = "", y = "# de Personas")

# Revisar calsificación DANE

table(train_hogares$Pobre)

train_hogares<- train_hogares %>% mutate(Pobre_hand=ifelse(Ingpcug<Lp,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand)

train_hogares<- train_hogares %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<Lp*Npersug,1,0))
table(train_hogares$Pobre,train_hogares$Pobre_hand_2)

#La clasificación del DANE es perfecta.



# LIMPIEZA train_personas -------------------------------------------------------

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
>>>>>>> Stashed changes

