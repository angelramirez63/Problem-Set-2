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

# Observar los tipos de variable
tipo_variables <- data.frame(
  Variable = names(train_hogares),
  Tipo = sapply(train_hogares, class)
)
print(tipo_variables)

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
table(train_full$Pobre, train_full$P6090) # Los Pobres reportan en su mayoría estar afiliados a seguridad social > Categoría más común 

#P6100 - regimen de seguridad social: 
table(train_full$Pobre, train_full$P6090) # Los Pobres reportan en su mayoría estar afiliados al regimen subsidiado -> Categoría más común 

#P6210 - máximo nivel educativo categoríco: 
table(train_full$Pobre, train_full$P6210) # Los Pobres tiene en su mayoría hasta educación media es decir de 6to a 9to -> Crear Categoría max9to e imputar con esa

#P6210s1 - máximo grado alcanzado: 
# Se puede usar para imputar la información de la variable P6210 -> Imputar con valor alguno de los extremos del intervalo alcanzado o pensar otra forma 

#Oficio - No están claras las respuestas bajo la ficha técnica del DANE.

#P6240 - actividad que ocupo más tiempo: 
table(train_full$Pobre, train_full$P6240) # Los Pobres usaron la mayor parte de su tiempo trabajando, en oficios del hogar o estudiando -> se podría imputar usando la categoría más común condicional a la edad(P6040) y al género(P6020)
#Si es mujer y no reporta ingreso imputar como oficio del hogar(4), si sí lo reporta imputar como trabajando(1)  
#Si las personas tienen 20 años o menos es probable que estén estudiando de los 21 para arriba es más probable que las personas estén trabajando 

#P6430 - Tipo de trabajador (cuenta propia, obrero o empleado de empresa particular, del gobierno, etc.): 

table(train_full$Pobre, train_full$P6430) #Notar que los pobres son predominantemente trabajadores por cuenta propia (4). Podría imputarse utilizando este valor.

#P6870 - Tamaño de la firma en donde trabaja - Puede ser redundante para determinar si la persona trabaja por cuenta propia o en una empresa grande.

#P6920 - Cotiza o no en fondo de pensiones?

table(train_full$Pobre, train_full$P6920) #Notar que hay una gran predominancia en pobres que no cotizan (2) vs. pobres que sí cotizan (1). Hay muy pocos pobres ya pensionados. Imputar con clase predominante.

#P7040, P7070, P7090, Variables que tienen que ver con un oficio secundario.

#Por parsimonia, de pronto estas variables son redundantes.

#P7495 - Recibio arriendo a pensiones: 
table(train_full$Pobre, train_full$P7495) # Los Pobres no reciben arriendo o pensiones -> imputar catergortía más común para todas las observaciones

#P7500s1a1 - Valor recibido por arriendo: 
table(train_full$P7500s1a1, train_full$Pobre) # Los Pobres en promedio no reciben arriendo -> imputar con cero si la persona es Pobre, pensar que hacer cuando no es pobre -> La mayoría de las personas no reciben un arriendo 

#P7500s2a1 - Valor recibido por pensión: 
table(train_full$P7500s2a1, train_full$Pobre) # Los Pobres en promedio no reciben pensión -> imputar con cero si la persona es Pobre,  pensar que hacer cuando no es pobre  -> La mayoría de las personas no reciben pensión 

#P7500s3a1 - Recibio pensión alimentaria por divorcio o separación: 
table(train_full$P7500s3a1, train_full$Pobre) #Los Pobres en promedio no recibne pensión alimentaria -> imputar con cero -> la mayoría de personas no reciben couta alimentaria 

#P7505 - Recibio dinero de alguin que no sea el gob: 
table(train_clean$P7505, train_clean$Pobre) #Imputar con la categoría más común 

#Por parsimonia, de pronto el resto de estas variables no son tan relevantes dentro del análisis.

#Pet - Población en edad de trabajo (1) si sí, (0) si no

table(train_full$Pobre, train_full$Pet) #OJO: Acá todos están en edad de trabajar. Según el DANE, la Pet: En colombia corresponde a la población de 12 o más años en la parte urbana y de 10 y más en la parte rural.

#Entonces podemos imputar los datos haciendo la tarea de revisar si se cumplen estas condiciones para los individuos en la parte de missings:

train_full$Pet[is.na(train_full$Pet)] <- ifelse(
  (train_full$cabecera == 1 & train_full$P6040 >= 12) |
    (train_full$cabecera == 2 & train_full$P6040 >= 10),
  1, 0
) #Este código se asegura que hicimos el procedimiento correctamente.


#Oc - Ocupado sí == 1. Fíjese que el montón de missing values es porque no hay una variable propia de desocupados. En ese caso, tendríamos que asumir que todos los valores faltantes corresponden a gente desocupada. Es un gran supuesto.

train_full$Ocupado <- ifelse(is.na(train_full$Oc), 0, train_full$Oc)

table(train_full$Ocupado)
table(train_full$Pobre, train_full$Ocupado, useNA = "always")

#Imputación de variables categóricas:







##Variables Contínuas a imputar: 

#P6426 - ¿Cuanto tiempo lleva trabajando en esta empresa, negocio, industria, oficina, firma o finca de manera continua?

train_full %>%
  group_by(Pobre) %>%
  summarise(across(P6426, list(mean = mean, median = median, min = min, max = max, sd = sd), na.rm = TRUE))

# Filtrar los datos sin valores NA en P6426
df <- train_full %>% filter(!is.na(P6426), !is.na(Pobre))

# Calcular media y mediana por grupo de Pobre
stats <- df %>%
  group_by(Pobre) %>%
  summarise(Media = mean(P6426, na.rm = TRUE), 
            Mediana = median(P6426, na.rm = TRUE))

# Crear el histograma con líneas de media y mediana
ggplot(df, aes(x = P6426, fill = as.factor(Pobre))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ Pobre, scales = "free") +  # Divide la figura en dos gráficos
  geom_vline(data = stats, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = stats, aes(xintercept = Mediana, color = "Mediana"), linetype = "solid", size = 1) +
  scale_fill_manual(values = c("blue", "red")) + 
  scale_color_manual(values = c("Media" = "black", "Mediana" = "darkgreen")) +
  labs(title = "Histograma de P6426 por Pobreza", 
       x = "P6426", 
       y = "Frecuencia",
       fill = "Pobre",
       color = "Líneas") +
  theme_minimal()

#Note que ambos grupos se parecen pero hay asimetrías a la derecha. Por ende,se sugiere imputar con la mediana correspondiente de ambos grupos.

# Calcular la mediana de P6426 para cada grupo de Pobre (0 y 1)
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_P6426 = median(P6426, na.rm = TRUE), .groups = "drop")

# Imputar los valores faltantes con la mediana correspondiente según Pobre
train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    P6426 = ifelse(is.na(P6426), Mediana_P6426, P6426)
  ) %>%
  select(-Mediana_P6426)  # Eliminar la columna auxiliar

# Verificar si aún hay valores missing en P6426
sum(is.na(train_full$P6426))

#P6800 - ¿Cuántas horas a la semana trabaja normalmente.... en ese trabajo ? 

train_full %>%
  group_by(Pobre) %>%
  summarise(across(P6800, list(mean = mean, median = median, min = min, max = max, sd = sd), na.rm = TRUE))

# Filtrar los datos sin valores NA en P6800
df <- train_full %>% filter(!is.na(P6800), !is.na(Pobre))

# Calcular media y mediana por grupo de Pobre
stats <- df %>%
  group_by(Pobre) %>%
  summarise(Media = mean(P6800, na.rm = TRUE), 
            Mediana = median(P6800, na.rm = TRUE))

# Crear el histograma con líneas de media y mediana
ggplot(df, aes(x = P6800, fill = as.factor(Pobre))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ Pobre, scales = "free") +  # Divide la figura en dos gráficos
  geom_vline(data = stats, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = stats, aes(xintercept = Mediana, color = "Mediana"), linetype = "solid", size = 1) +
  scale_fill_manual(values = c("blue", "red")) + 
  scale_color_manual(values = c("Media" = "black", "Mediana" = "darkgreen")) +
  labs(title = "Histograma de P6800 por Pobreza", 
       x = "P6800", 
       y = "Frecuencia",
       fill = "Pobre",
       color = "Líneas") +
  theme_minimal()

#Note que ambos grupos se parecen y sus respuestas se distribuyen normal con media centrada en 50. Podría imputarse con la media de ambos grupos.

# Calcular la mediana de P6426 para cada grupo de Pobre (0 y 1)
medias <- train_full %>%
  group_by(Pobre) %>%
  summarise(Media_P6800 = mean(P6800, na.rm = TRUE), .groups = "drop")

# Imputar los valores faltantes con la media correspondiente según Pobre
train_full <- train_full %>%
  left_join(medias, by = "Pobre") %>%
  mutate(
    P6800 = ifelse(is.na(P6800), Media_P6800, P6800)
  ) %>%
  select(-Media_P6800)  # Eliminar la columna auxiliar

# Verificar si aún hay valores missing en P6800
sum(is.na(train_full$P6800))

#Impa - Ingreso monetario de primera actividad antes de imputación

train_full %>%
  group_by(Pobre) %>%
  summarise(across(Impa, list(mean = mean, median = median, min = min, max = max, sd = sd), na.rm = TRUE))

# Filtrar los datos sin valores NA en P6800
df <- train_full %>% filter(!is.na(Impa), !is.na(Pobre))

# Calcular media y mediana por grupo de Pobre
stats <- df %>%
  group_by(Pobre) %>%
  summarise(Media = mean(Impa, na.rm = TRUE), 
            Mediana = median(Impa, na.rm = TRUE))

# Crear el histograma con líneas de media y mediana
ggplot(df, aes(x = Impa, fill = as.factor(Pobre))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ Pobre, scales = "free") +  # Divide la figura en dos gráficos
  geom_vline(data = stats, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = stats, aes(xintercept = Mediana, color = "Mediana"), linetype = "solid", size = 1) +
  scale_fill_manual(values = c("blue", "red")) + 
  scale_color_manual(values = c("Media" = "black", "Mediana" = "darkgreen")) +
  labs(title = "Histograma de Impa por Pobreza", 
       x = "Impa", 
       y = "Frecuencia",
       fill = "Pobre",
       color = "Líneas") +
  theme_minimal()

#Los grupos esta vez no se parecen tanto y hay asimetría a la derecha, entonces imputamos usando la mediana de cada grupo.

# Calcular la mediana de P6426 para cada grupo de Pobre (0 y 1)
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_Impa = median(Impa, na.rm = TRUE), .groups = "drop")

# Imputar los valores faltantes con la mediana correspondiente según Pobre
train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    Impa = ifelse(is.na(Impa), Mediana_Impa, Impa)
  ) %>%
  select(-Mediana_Impa)  # Eliminar la columna auxiliar

# Verificar si aún hay valores missing en Impa
sum(is.na(train_full$Impa))

#Isa - Ingreso monetario de la segunda actividad

train_full %>%
  group_by(Pobre) %>%
  summarise(across(Isa, list(mean = mean, median = median, min = min, max = max, sd = sd), na.rm = TRUE))

# Filtrar los datos sin valores NA en P6800
df <- train_full %>% filter(!is.na(Isa), !is.na(Pobre))

# Calcular media y mediana por grupo de Pobre
stats <- df %>%
  group_by(Pobre) %>%
  summarise(Media = mean(Isa, na.rm = TRUE), 
            Mediana = median(Isa, na.rm = TRUE))

# Crear el histograma con líneas de media y mediana
ggplot(df, aes(x = Isa, fill = as.factor(Pobre))) +
  geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
  facet_wrap(~ Pobre, scales = "free") +  # Divide la figura en dos gráficos
  geom_vline(data = stats, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
  geom_vline(data = stats, aes(xintercept = Mediana, color = "Mediana"), linetype = "solid", size = 1) +
  scale_fill_manual(values = c("blue", "red")) + 
  scale_color_manual(values = c("Media" = "black", "Mediana" = "darkgreen")) +
  labs(title = "Histograma de Impa por Pobreza", 
       x = "Impa", 
       y = "Frecuencia",
       fill = "Pobre",
       color = "Líneas") +
  theme_minimal()

#Los grupos esta vez no se parecen tanto y hay asimetría a la derecha, entonces imputamos usando la mediana de cada grupo.

# Calcular la mediana de P6426 para cada grupo de Pobre (0 y 1)
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_Isa = median(Isa, na.rm = TRUE), .groups = "drop")

# Imputar los valores faltantes con la mediana correspondiente según Pobre
train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    Isa = ifelse(is.na(Isa), Mediana_Isa, Isa)
  ) %>%
  select(-Mediana_Isa)  # Eliminar la columna auxiliar

# Verificar si aún hay valores missing en Isa
sum(is.na(train_full$Isa))

#Se hace lo mismo para todas las variables faltantes:

# Lista de variables a graficar
vars_to_plot <- c("Iof1", "Iof2", "Iof3h", "Iof3i", "Iof6", "Ingtotob", "Ingtot")

# Función para crear histogramas con líneas de media y mediana
plot_histogram <- function(df, var) {
  df_filtered <- df %>% filter(!is.na(.data[[var]]))  # Filtrar valores NA
  stats <- df_filtered %>%
    group_by(Pobre) %>%
    summarise(Media = mean(.data[[var]], na.rm = TRUE),
              Mediana = median(.data[[var]], na.rm = TRUE), .groups = "drop")
  
  ggplot(df_filtered, aes(x = .data[[var]], fill = as.factor(Pobre))) +
    geom_histogram(bins = 30, alpha = 0.6, position = "identity") +
    facet_wrap(~ Pobre, scales = "free") +
    geom_vline(data = stats, aes(xintercept = Media, color = "Media"), linetype = "dashed", size = 1) +
    geom_vline(data = stats, aes(xintercept = Mediana, color = "Mediana"), linetype = "solid", size = 1) +
    scale_fill_manual(values = c("blue", "red")) +
    scale_color_manual(values = c("Media" = "black", "Mediana" = "darkgreen")) +
    labs(title = paste("Histograma de", var, "por Pobreza"),
         x = var,
         y = "Frecuencia",
         fill = "Pobre",
         color = "Líneas") +
    theme_minimal()
}

# Crear y mostrar todos los histogramas
histograms <- map(vars_to_plot, ~ plot_histogram(train_full, .x))
histograms

#Todos los histogramas muestran asimetrías a la derecha, entonces se impuatarán los missings utilizando la mediana.

# Imputación de Iof1
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_Iof1 = median(Iof1, na.rm = TRUE), .groups = "drop")

train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    Iof1 = ifelse(is.na(Iof1), Mediana_Iof1, Iof1)
  ) %>%
  select(-Mediana_Iof1)

# Imputación de Iof2
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_Iof2 = median(Iof2, na.rm = TRUE), .groups = "drop")

train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    Iof2 = ifelse(is.na(Iof2), Mediana_Iof2, Iof2)
  ) %>%
  select(-Mediana_Iof2)

# Imputación de Iof3h
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_Iof3h = median(Iof3h, na.rm = TRUE), .groups = "drop")

train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    Iof3h = ifelse(is.na(Iof3h), Mediana_Iof3h, Iof3h)
  ) %>%
  select(-Mediana_Iof3h)

# Imputación de Iof3i
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_Iof3i = median(Iof3i, na.rm = TRUE), .groups = "drop")

train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    Iof3i = ifelse(is.na(Iof3i), Mediana_Iof3i, Iof3i)
  ) %>%
  select(-Mediana_Iof3i)

# Imputación de Iof6
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_Iof6 = median(Iof6, na.rm = TRUE), .groups = "drop")

train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    Iof6 = ifelse(is.na(Iof6), Mediana_Iof6, Iof6)
  ) %>%
  select(-Mediana_Iof6)

# Imputación de Ingtotob
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_Ingtotob = median(Ingtotob, na.rm = TRUE), .groups = "drop")

train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    Ingtotob = ifelse(is.na(Ingtotob), Mediana_Ingtotob, Ingtotob)
  ) %>%
  select(-Mediana_Ingtotob)

# Imputación de Ingtot
medianas <- train_full %>%
  group_by(Pobre) %>%
  summarise(Mediana_Ingtot = median(Ingtot, na.rm = TRUE), .groups = "drop")

train_full <- train_full %>%
  left_join(medianas, by = "Pobre") %>%
  mutate(
    Ingtot = ifelse(is.na(Ingtot), Mediana_Ingtot, Ingtot)
  ) %>%
  select(-Mediana_Ingtot)

# Verificar si aún hay valores missing en las variables imputadas
colSums(is.na(train_full[c("Iof1", "Iof2", "Iof3h", "Iof3i", "Iof6", "Ingtotob", "Ingtot")]))



























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

