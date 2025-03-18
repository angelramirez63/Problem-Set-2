#### Preparación
library(pacman)

p_load(tidyverse, glmnet, caret, Mlmetrics, Metrics)

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


