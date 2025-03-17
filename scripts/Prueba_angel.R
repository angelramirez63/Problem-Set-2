#### Preparación
library(pacman)

p_load(tidyverse, rvest, rebus, htmltools, rio, skimr,
       visdat, margins, stargazer, here, VIM, caret, dplyr, stats)

rm(list = ls())

# Crear el directorio 
wd <- here()
setwd()
rm(wd)

# Cargar los datos
ss <- read.csv("sample_submission.csv") %>% 
  as_tibble()

tsh <- read.csv("test_hogares.csv") %>% 
  as_tibble()

trh <- read.csv("train_hogares.csv") %>% 
  as_tibble()

tsp <- read.csv("test_personas.csv") %>% 
  as_tibble()

trp <- readRDS("train_personas.rds") %>% 
  as_tibble() #Por el peso del archivo se convirtió a rds


