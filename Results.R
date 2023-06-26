# Thesis Code: Dejo and Campos -------------------------------------------------
# Libraries and functions ------------------------------------------------------
rm(list = ls())
library(tidyverse)
library(stargazer)
# setwd("C:/Users/gj.camposm/OneDrive - Universidad del Pac?fico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo")
setwd("C:/Users/gj.camposm/OneDrive - Universidad del Pacífico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo")
# setwd("C:/Users/geral/OneDrive - Universidad del Pacífico/UP/Ciclo 11/1. IE 2/Trabajo Investigacion Economica/Codigo")
source("Functions.R")

# Implementable portfolios -----------------------------------------------------
## Gabo data -------------------------------------------------------------------
df_1 <- read.csv("factor portfolios excess mensual.csv")
df_1_returns <- df_1[,2:ncol(df_1)]
results_implementable_1 <- performance_table_function(df_1_returns, 'implementable', 100, gamma = 1)

## Kenneth data -------------------------------------------------------------------
df_2 <- read.csv("kenneth em factor excess mensual.csv")
df_2_returns <- df_2[,2:ncol(df_2)]
results_implementable_2 <- performance_table_function(df_2_returns, 'implementable', 100,  gamma = 1)
## present results ---------------------------------------------------------------

results_implementable_1
results_implementable_2
stargazer(results_implementable_1,type = "html",out="implementable df1.html",summary = F)
stargazer(results_implementable_2,type = "html",out="implementable df2.html",summary = F)

# Theoretical Portfolios -------------------------------------------------------
## Gabo data -------------------------------------------------------------------
results_theoretical_1 <- performance_table_function(df_1_returns, 'theoretical', 100,  gamma = 1)

## Kenneth data ----------------------------------------------------------------
results_theoretical_2 <- performance_table_function(df_2_returns, 'theoretical', 100,  gamma = 1)

## joint results ---------------------------------------------------------------
results_theoretical_1
results_theoretical_2
stargazer(results_theoretical_1,type = "html",out="theoretical df1.html",summary = F)
stargazer(results_theoretical_2,type = "html",out="theoretical df2.html",summary = F)

## Compare ---------------------------------------------------------------------

stargazer(results_implementable_1,type = "html",summary = F)
stargazer(results_implementable_2,type = "html",summary = F)
stargazer(results_theoretical_1,type = "text",summary = F)
stargazer(results_theoretical_2,type = "text",summary = F)

cat("df1: Estimation risk for KZ", results_theoretical_1[1,"KZ"] - results_implementable_1[1,"KZ"])
cat("df1: Estimation risk for Q", results_theoretical_1[1,"Q"] - results_implementable_1[1,"QS"])

cat("df2: Estimation risk for KZ", results_theoretical_2[1,"KZ"] - results_implementable_2[1,"KZ"])
cat("df2: Estimation risk for Q", results_theoretical_2[1,"Q"] - results_implementable_2[1,"QS"]) 