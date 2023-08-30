
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
  select(cap_cm, dap_cm, altura_m, diametro_copa_m, parcelas)

view(var_floresta)

# Tratar dados faltantes --------------------------------------------------------------------------------------------------------------------------

## Visualizar dados faltantes

vis_miss(var_floresta, cluster = TRUE) 

## Removendo dados faltantes

var_floresta <- remove_missing(var_floresta) 

## Visualizar

vis_miss(var_floresta)
view(var_floresta)

# Manipulação das tabelas ------------------------------------------------------------------------------------------------------------------

## Calcular médias por parcelas

var_floresta <- var_floresta %>%
  drop_na() %>%
  group_by(parcelas) %>%
  summarise(cap_cm = mean(cap_cm),
            dap_cm = mean(dap_cm),
            altura_m = mean(altura_m),
            diametro_copa_m = mean(diametro_copa_m))

view(var_floresta)

## Unir tabelas de dados de solo com dados de floresta

View(var_floresta)
View(solo)

floresta_solo <- left_join(var_floresta, solo, by = 'parcelas') 
view(floresta_solo)

# Verificar se existe relação linear -------------------------------------------------------------------------------------------------------

ggplot(floresta_solo, aes(cap_cm, ph)) +
    geom_point(cex = 4,alpha = 0.7) +
    geom_smooth(method = "lm", formula = y~x) +
    labs(x = "Distância do rio (m)", 
         y = "Circunferência a altura do peito (cm)") +
    theme_bw()

ggplot(floresta_solo, aes(cap_cm, areia)) +
    geom_point(cex = 4,alpha = 0.7) +
    geom_smooth(method = "lm", formula = y~x) +
    labs(x = "Distância do rio (m)", 
         y = "Circunferência a altura do peito (cm)") +
    theme_bw()

ggplot(floresta_solo, aes(cap_cm, silte)) +
    geom_point(cex = 4,alpha = 0.7) +
    geom_smooth(method = "lm", formula = y~x) +
    labs(x = "Distância do rio (m)", 
         y = "Circunferência a altura do peito (cm)") +
    theme_bw()

ggplot(floresta_solo, aes(cap_cm, argila)) +
    geom_point(cex = 4,alpha = 0.7) +
    geom_smooth(method = "lm", formula = y~x) +
    labs(x = "Distância do rio (m)", 
         y = "Circunferência a altura do peito (cm)") +
    theme_bw()

ggplot(floresta_solo, aes(cap_cm, soma_bases)) +
    geom_point(cex = 4,alpha = 0.7) +
    geom_smooth(method = "lm", formula = y~x) +
    labs(x = "Distância do rio (m)", 
         y = "Circunferência a altura do peito (cm)") +
    theme_bw()

ggplot(floresta_solo, aes(cap_cm, N)) +
    geom_point(cex = 4,alpha = 0.7) +
    geom_smooth(method = "lm", formula = y~x) +
    labs(x = "Distância do rio (m)", 
         y = "Circunferência a altura do peito (cm)") +
    theme_bw()
