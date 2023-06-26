# Thesis Code: Dejo and Campos -------------------------------------------------
# Libraries and functions ------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stargazer)
library(copula)
library(ggplot2)
library(texreg)
# setwd("C:/Users/gj.camposm/OneDrive - Universidad del Pac?fico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo")
setwd("C:/Users/gj.camposm/OneDrive - Universidad del Pacífico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo")
# setwd("C:/Users/geral/OneDrive - Universidad del Pacífico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo")
source("Results.R")

# Copulas ----------------------------------------------------------------------
list_copulas_1 <- copulas_test_function(df_1, 100, 1000, 100)
list_copulas_2 <- copulas_test_function(df_2, 100, 1000, 100)

list_copulas_1[[1]] %>%
  mutate(diff = KZ - QSa) %>%
  pull(diff) %>% hist()

stargazer(list_copulas_1[[1]],type = "html",out="simulations result.html",summary = T)

# Sensitivity ------------------------------------------------------------------
list_sim <- seq(1, 100, 1)
df_sensitivity_final_1 <- sensitivity_function(df_1, list_sim)
df_sensitivity_final_2 <- sensitivity_function(df_2, list_sim)
write.csv(df_sensitivity_final_1, 
          file = "C:/Users/gj.camposm/OneDrive - Universidad del Pacífico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo/df1 sensitivity.csv", 
          row.names=FALSE)
write.csv(df_sensitivity_final_2, 
          file = "C:/Users/gj.camposm/OneDrive - Universidad del Pacífico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo/df2 sensitivity.csv", 
          row.names=FALSE)
