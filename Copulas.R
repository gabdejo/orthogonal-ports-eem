# Thesis Code: Dejo and Campos -------------------------------------------------
# Libraries and functions ------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stargazer)
library(copula)
library(ggplot2)
library(texreg)
# setwd("C:/Users/gj.camposm/OneDrive - Universidad del Pac?fico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo")
setwd('C:/Users/CRISTINA/OneDrive - Universidad del Pacífico/Universidad/Tesis IE2/Codigo Gerald')
source("Resultados.R")

# Copulas ----------------------------------------------------------------------
list_copulas_gabo <- copulas_test(df_gabo, 100, 1000, 100)
list_copulas_kenneth <- copulas_test(df_kenneth, 100, 1000, 100)

(list_copulas_gabo[[1]] %>%
  mutate(diff = KZ - QS) %>%
  pull(diff) %>% #hist()
  wilcox.test())$p.value

(list_copulas_gabo[[1]] %>%
  mutate(diff = KZ - QSa) %>%
  pull(diff) %>% #hist()
  wilcox.test())$p.value

list_copulas_kenneth[[4]]

stargazer(list_copulas_gabo[[1]],type = "html",out="simulation_gabo.html",summary = T)

# Sensitivity ------------------------------------------------------------------
list_sim <- c(seq(0.1,0.9,0.1),seq(1, 10, 1))
df_sensitivity_final_gabo <- sensitivity(df_gabo, list_sim)
df_sensitivity_final_kenneth <- sensitivity(df_kenneth, list_sim)
write.csv(df_sensitivity_final_gabo, 
          file = "df sensitivity gabo.csv", 
          row.names=FALSE)
write.csv(df_sensitivity_final_kenneth, 
          file = "df sensitivity kenneth.csv", 
          row.names=FALSE)
