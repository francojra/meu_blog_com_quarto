
# Análise Modelo Linear Generalizado - GLM -------------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(visdat)
library(DHARMa)
library(MuMIn)
library(piecewiseSEM)
library(performance)
library(ggExtra)

# Carregar dados ---------------------------------------------------------------------------------------------------------------------------

var_floresta <- readxl::read_xlsx("dados_amb.xlsx")
solo <- readxl::read_xlsx("dados_solo.xlsx")

# Ver estrutura dos dados ------------------------------------------------------------------------------------------------------------------

View(var_floresta)
str(var_floresta)

View(var_solo)
str(var_solo)

# Manipulação das tabelas ------------------------------------------------------------------------------------------------------------------

var_amb <- var_amb %>% 
  mutate(distancia = case_when(
    parcelas %in% c(1, 2, 11, 12, 21, 22, 31, 32,
                    41, 42, 51, 52) ~ 100,
    parcelas %in% c(3, 4, 13, 14, 23, 24, 33, 34, 
                    43, 44, 53, 54) ~ 200,
    parcelas %in% c(5, 6, 15, 16, 25, 26, 35, 36,
                    45, 46, 55, 56) ~ 300,
    parcelas %in% c(7, 8, 17, 18, 27, 28, 37, 38,
                    47, 48, 57, 58) ~ 400,
    parcelas %in% c(9, 10, 19, 20, 29, 30, 39, 40,
                    49, 50, 59, 60) ~ 500
  ))

view(var_amb)

var_amb <- var_amb %>%
  select(altura_m, distancia) %>%
  view()

var_amb$altura_m <- as.numeric(var_amb$altura_m)
var_amb$distancia <- as.numeric(var_amb$distancia)

# Dados faltantes --------------------------------------------------------------------------------------------------------------------------

## Visualizar dados faltantes

vis_miss(var_amb, cluster = TRUE) 

## Removendo dados faltantes

var_amb_semNA <- remove_missing(var_amb, 
             vars = c("altura_m")) 

## Visualizar

vis_miss(var_amb_semNA)
view(var_amb_semNA)