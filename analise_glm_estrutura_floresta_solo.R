
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

View(solo)
str(solo)

# Manipulação das tabelas ------------------------------------------------------------------------------------------------------------------

var_floresta <- var_floresta %>% 
  select(cap_cm, dap_cm, altura_m, diametro_copa_m)

view(var_floresta)

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