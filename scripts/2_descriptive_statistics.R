# Estadísticas descriptivas ----------------------------------------------------
rm(list = ls())

## 0. Preparación --------------------------------------------------------------

### Librerías 
if(!require(pacman)) install.packages("pacman") ; require(pacman)

library(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, 
       dplyr, boot, kableExtra, knitr)


### Crear el directorio 
wd <- here()
setwd(wd)
rm(wd)

## Cargar datos
db <- readRDS("stores/datos_modelos.rds") %>% 
  as_tibble()

## Estadísticas descriptivas ---------------------------------------------------
#Visualización: de las distribuciones de las variables de ingreso 
db <- db %>% mutate(y_ingLab_m_k =y_ingLab_m/1000)
variables <- c("y_ingLab_m_k", "y_ingLab_m_ha", "ln_sal") 
labels <- c("Salario mensual (miles)", "Salario por hora", "Logarítmo natural del salario")

# Iteramos con índice para asociar cada variable con su label correspondiente
for (i in seq_along(variables)) {
  variable <- variables[i]
  label <- labels[i]
  
  plot <- ggplot(db, aes_string(variable)) +
    geom_histogram(color = "#000000", fill = "cornflowerblue") +
    geom_vline(xintercept = median(db[[variable]], na.rm = TRUE), linetype = "dashed", color = "red") +
    geom_vline(xintercept = mean(db[[variable]], na.rm = TRUE), linetype = "dashed", color = "blue") +  
    ggtitle(label) +
    xlab(label) + 
    ylab("Conteo") +
    theme_classic() +
    theme(plot.title = element_text(size = 16, hjust = 0.5, face = "bold"))
  
  print(plot)
  ggsave(filename = paste0("views/", variable, ".png"), width = 7, height = 6, dpi = 300)
  
}

#Tabla de estadísticas descriptivas

interest_vars <- db %>% select(y_ingLab_m, y_ingLab_m_ha, ln_sal, 
                               age, female, microEmpresa, formal, 
                               totalHoursWorked, hoursWorkUsual,
                               grad_aprob, college, segundo_trabajo) %>%
  as.data.frame() %>%
  mutate(across(c(college, microEmpresa, formal, female, segundo_trabajo), 
                ~ as.integer(levels(.))[.]))

stargazer(interest_vars, summary = TRUE, type = "latex", 
          title = "Estadísticas descriptivas",
          out = "Views/desc_est.txt", digits = 2)

# Estadísticas descriptivas, variables categóricas: 

# Lista de variables categóricas
categoricas <- c("maxEducLevel", "cotPension", "regSalud", "oficio", 
                 "parentesco_jefe", "segur_social", "sizeFirm", 
                 "relab")

# Calcular frecuencias y porcentajes
tabla_categoricas <- db %>%
  select(all_of(categoricas)) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "Variable", values_to = "Categoria") %>%
  group_by(Variable, Categoria) %>%
  summarise(Frecuencia = n(), .groups = "drop") %>%
  mutate(Porcentaje = round((Frecuencia / nrow(db)) * 100, 2))

#Categoría más frecuente
tabla_categoricas_top <- tabla_categoricas %>%
  group_by(Variable) %>%
  slice_max(order_by = Frecuencia, n = 1) %>%
  ungroup()

