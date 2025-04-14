#============== Estadística descriptivas para pobre y no pobre ================


#Alistar ambiente de trabajo ---------------------------------------------------

#Limpiar ambiente
#rm(list = ls())
cat("\014")

#Cargar paquetes
p_load(tidyverse,
       here,
       skimr,
       VIM,
       rio,
       xtable, 
       knitr, 
       stargazer)

#Cargary alistar datos ---------------------------------------------------------
db <- readRDS('stores/db_final.rds') %>% 
  as_tibble()


#Variables para hacer las estadísticas descriptivas 
db_clean <- db %>% 
        select(Ncuartos, arriendo,
                Npersonas, 
                t_horas_trabajadas, mujer, segur_subsidiado,
                menor_15, edad, P_Ed_Basica_primaria, 
                P_Ed_Basica_secundaria, P_Ed_Media, P_Ed_Superior, 
                t_tiempo_empresa, Ocupados, Inactivos, 
                p_cotiza_pension, segur_social, Pobre)


#Base de datos pobres 
db_pobre <- db_clean %>%  
                  filter(Pobre == 1) %>% 
                  select(-Pobre) %>% 
                  as.data.frame()


#Base de datos no pobres
db_no_pobre <- db_clean %>%  
                  filter(Pobre == 0) %>% 
                  select(-Pobre) %>% 
                  as.data.frame()

#Tablas estadísticas descriptivas-----------------------------------------------


#Estadísticas descriptivas Pobres 
stargazer(db_pobre, type = "latex", 
          title = "Estadísticas Descriptivas Pobres",
          digits = 3, summary.stat = c("mean", "sd", "min", "max", "median"))
          #out = "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/views/tablavarsCONTINUAS.tex")

#Estadísticas descriptivas No pobres
stargazer(db_no_pobre, type = "latex", 
          title = "Estadísticas Descriptivas No Pobres",
          digits = 3, summary.stat = c("mean", "sd", "min", "max", "median"))
#out = "/Users/juanpablogrimaldos/Documents/Documentos - MacBook Pro de Juan/GitHub/Problem-Set-2/views/tablavarsCONTINUAS.tex")




















