# Thesis Code: Dejo and Campos -------------------------------------------------
# Libraries and functions ------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stargazer)
# setwd("C:/Users/gj.camposm/OneDrive - Universidad del Pac?fico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo")
setwd('C:/Users/CRISTINA/OneDrive - Universidad del Pacífico/Universidad/Tesis IE2/Codigo Gerald')
source("Funciones.R")

# Implementable portfolios -----------------------------------------------------
## Gabo data -------------------------------------------------------------------
df_gabo <- read.csv("factor portfolios excess mensual.csv")
df_gabo_returns <- df_gabo[,2:ncol(df_gabo)]
results_implementable_gabo <- performance_table(df_gabo_returns, 'implementable', 100, gamma = 1)

## Kenneth data -------------------------------------------------------------------
df_kenneth <- read.csv("kenneth em factor excess mensual.csv")
df_kenneth_returns <- df_kenneth[,2:ncol(df_kenneth)]
results_implementable_Kenneth <- performance_table(df_kenneth_returns, 'implementable', 100,  gamma = 1)
## present results ---------------------------------------------------------------

results_implementable_gabo
results_implementable_Kenneth
stargazer(results_implementable_gabo,type = "html",out="implementable gabo.html",summary = F)
stargazer(results_implementable_Kenneth,type = "html",out="implementable kenneth.html",summary = F)

# Theoretical Portfolios -------------------------------------------------------
## Gabo data -------------------------------------------------------------------
results_theoretical_gabo <- performance_table(df_gabo_returns, 'theoretical', 100,  gamma = 1)

## Kenneth data ----------------------------------------------------------------
results_theoretical_Kenneth <- performance_table(df_kenneth_returns, 'theoretical', 100,  gamma = 1)

## joint results ---------------------------------------------------------------
results_theoretical_gabo
results_theoretical_Kenneth
stargazer(results_theoretical_gabo,type = "html",out="theoretical gabo.html",summary = F)
stargazer(results_theoretical_Kenneth,type = "html",out="theoretical kenneth.html",summary = F)

## Compare ---------------------------------------------------------------------

stargazer(results_implementable_gabo,type = "html",summary = F)
stargazer(results_implementable_Kenneth,type = "html",summary = F)
stargazer(results_theoretical_gabo,type = "text",summary = F)
stargazer(results_theoretical_Kenneth,type = "text",summary = F)

cat("Gabo: Estimation risk for KZ", results_theoretical_gabo[1,"KZ"] - results_implementable_gabo[1,"KZ"])
cat("Gabo: Estimation risk for Q", results_theoretical_gabo[1,"Q"] - results_implementable_gabo[1,"QS"])

cat("Kenneth: Estimation risk for KZ", results_theoretical_Kenneth[1,"KZ"] - results_implementable_Kenneth[1,"KZ"])
cat("Kenneth: Estimation risk for Q", results_theoretical_Kenneth[1,"Q"] - results_implementable_Kenneth[1,"QS"]) 