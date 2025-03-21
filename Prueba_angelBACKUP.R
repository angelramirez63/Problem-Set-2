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

train_hogares_clean <- train_hogares[, !missing_cols] # Eliminamos variables con más de 60% en missing values

colSums(is.na(train_hogares_clean)) #Note que las variables con más de 60% de missings fueron removidas.


# Observar los tipos de variable
tipo_variables <- data.frame(
  Variable = names(train_hogares_clean),
  Tipo = sapply(train_hogares_clean, class)
)
print(tipo_variables)

# Convertir variables categóricas en factores
train_hogares_clean <- train_hogares_clean %>%
  mutate(
    P5090 = as.factor(P5090),
    Clase = as.factor(Clase),
    Pobre = factor(Pobre, levels = c(1,0), labels = c("Pobre","Otros")),
    Indigente = as.factor(Indigente),
    Depto = as.factor(Depto),
    Dominio = as.factor(Dominio)
  )

# Renombrar variables  
train_hogares_clean <- train_hogares_clean %>% 
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
numeric_vars <- train_hogares_clean %>% 
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
summary_table_cabecera <- categorical_summary(train_hogares_clean, "cabecera", 
                                              c("Cabecera", "Resto"), 
                                              "Cabecera")
summary_table_pobre <- categorical_summary(train_hogares_clean, "Pobre", 
                                           c("Pobre", "Otros"), 
                                           "Clasificación del hogar según ingresos")
summary_table_vivienda <- categorical_summary(train_hogares_clean, "vivienda", 
                                              c("Propia y pagada", "Propia, pagando", 
                                                "Arriendo / Subarriendo", "Usufructo", 
                                                "Posesión sin título", "Otra"), 
                                              "Tipo de vivienda del hogar")
summary_table_indigente <- categorical_summary(train_hogares_clean, "Indigente", 
                                               c("No Indigente", "Indigente"), 
                                               "Son los individuos del hogar indigentes?")
summary_table_depto <- categorical_summary(
  train_hogares_clean, "Depto",
  levels(train_hogares_clean$Depto), "Departamento de residencia del hogar"  # Niveles del factor Depto
)

summary_table_dominio <- categorical_summary(
  train_hogares_clean, "Dominio", 
  levels(train_hogares_clean$Dominio), "Dominio de residencia del hogar"
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
ggplot(train_hogares_clean, aes(x = Pobre, fill = Pobre)) +
  geom_bar() + 
  theme_minimal() +
  scale_fill_manual(values = c("Pobre" = "orange", "Otros"= "blue")) +
  labs(x = "", y = "# de Personas")

# Revisar calsificación DANE

table(train_hogares_clean$Pobre)

train_hogares_clean<- train_hogares_clean %>% mutate(Pobre_hand=ifelse(Ingpcug<linea_pobreza,1,0))
table(train_hogares_clean$Pobre,train_hogares_clean$Pobre_hand)

train_hogares_clean<- train_hogares_clean %>% mutate(Pobre_hand_2=ifelse(Ingtotugarr<linea_pobreza*Nper_unidad_gasto,1,0))
table(train_hogares_clean$Pobre,train_hogares_clean$Pobre_hand_2)

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

# Identificar variables con más de 60% de missings:

threshold <- 0.6 *nrow(train_personas)

missing_cols <- colSums(is.na(train_personas)) > threshold

# Obtener nombres de variables con más del 60% de missings
vars_with_missing <- names(train_personas)[missing_cols]

# Mostrar el número de variables con más del 60% de missings (88 variables)
cat("Las variables con más de 60% de missings son:", length(vars_with_missing), "\n")
print(vars_with_missing)

# Crear un dataframe para visualización
missing_data <- data.frame(
  variable = names(train_personas),
  missing_rate = colSums(is.na(train_personas)) / nrow(train_personas)
)

# Filtrar solo las variables con más del 60% de missings
missing_data_filtered <- missing_data %>% filter(missing_rate > 0.6)

# Visualización de variables con más del 60% de missings
ggplot(missing_data_filtered, aes(x = reorder(variable, missing_rate), y = missing_rate)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "Variables con más del 60% de Datos Faltantes", 
       x = "Nombre de Variable", y = "Proporción de Missings") +
  theme(axis.text = element_text(size = 5))

# Eliminar las variables con más del 60% de valores faltantes
train_personas_clean <- train_personas[, !(colSums(is.na(train_personas)) / nrow(train_personas) > 0.6)]

# Verificar el número de variables eliminadas
cat("Número de variables eliminadas:", ncol(train_personas) - ncol(train_personas_clean), "\n")

# Mostrar las dimensiones del nuevo dataset
cat("Dimensiones del dataset limpio:", dim(train_personas_clean), "\n")

# Mostrar variables missing del dataset limpio

missing_values<-colSums(is.na(train_personas_clean))
missing_tab<-data.frame(
  Miss_val=missing_values
)
missing_tab

#Parece que hay variables con el mismo número de missings. Vamos a confirmar:

table(missing_tab$Miss_val)

#Hay un montón de variables con el mismo número de missings. Vamos a ver cuáles no:

missing_tab <- data.frame(
  Variable = names(missing_values),
  Miss_val = missing_values
)

var_diff_missings <- missing_tab$Variable[missing_tab$Miss_val != min(missing_tab$Miss_val)]
print(var_diff_missings)

#Unimos las dos bases de datos habiendo removido los missing values

train_full <- left_join(train_personas_clean, train_hogares_clean, by = "id")


##Variables Categorícas a imputar: 

#P6090 - seguridad social:
table(train_clean$Pobre, train_full$P6090) # Los Pobres reportan en su mayoría estar afiliados a seguridad social > Categoría más común 

#P6100 - regimen de seguridad social: 
table(train_clean$Pobre, train_full$P6090) # Los Pobres reportan en su mayoría estar afiliados al regimen subsidiado -> Categoría más común 

#P6210 - máximo nivel educativo categoríco: 
table(train_clean$Pobre, train_full$P6210) # Los Pobres tiene en su mayoría hasta educación media es decir de 6to a 9to -> Crear Categoría max9to e imputar con esa

#P6210s1 - máximo grado alcanzado: 
# Se puede usar para imputar la información de la variable P6210 -> Imputar con valor alguno de los extremos del intervalo alcanzado o pensar otra forma 

#Oficio - No están claras las respuestas bajo la ficha técnica del DANE.

#P6240 - actividad que ocupo más tiempo: 
table(train_clean$Pobre, train_full$P6240) # Los Pobres usaron la mayor parte de su tiempo trabajando, en oficios del hogar o estudiando -> se podría imputar usando la categoría más común condicional a la edad(P6040) y al género(P6020)
#Si es mujer y no reporta ingreso imputar como oficio del hogar(4), si sí lo reporta imputar como trabajando(1)  
#Si las personas tienen 20 años o menos es probable que estén estudiando de los 21 para arriba es más probable que las personas estén trabajando 

#P7495 - Recibio arriendo a pensiones: 
table(train_clean$Pobre, train_full$P7495) # Los Pobres no reciben arriendo o pensiones -> imputar catergortía más común para todas las observaciones

#P7500s1a1 - Valor recibido por arriendo: 
table(train_clean$P7500s1a1, train_full$Pobre) # Los Pobres en promedio no reciben arriendo -> imputar con cero si la persona es Pobre, pensar que hacer cuando no es pobre -> La mayoría de las personas no reciben un arriendo 

#P7500s2a1 - Valor recibido por pensión: 
table(train_clean$P7500s2a1, train_full$Pobre) # Los Pobres en promedio no reciben pensión -> imputar con cero si la persona es Pobre,  pensar que hacer cuando no es pobre  -> La mayoría de las personas no reciben pensión 

#P7500s3a1 - Recibio pensión alimentaria por divorcio o separación: 
table(train_clean$P7500s3a1, train_full$Pobre) #Los Pobres en promedio no recibne pensión alimentaria -> imputar con cero -> la mayoría de personas no reciben couta alimentaria 



##Variables Contínuas a imputar: 

#P6426 - ¿Cuanto tiempo lleva trabajando en esta empresa, negocio, industria, oficina, firma o finca de manera continua?

train_full %>%
  group_by(Pobre) %>%
  summarise(across(P6426, list(mean = mean, median = median, min = min, max = max, sd = sd), na.rm = TRUE))



#P7505 - Recibio dinero de alguin que no sea el gob: 
table(train_clean$P7505, train_clean$Pobre) #Imputar con la categoría más común 




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

