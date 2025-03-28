
# Consultoria Lucas Zanzini 02/2025 --------------------------------------------------------------------------------------------------------

# Cálculos da estatística descritiva dos dados ---------------------------------------------------------------------------------------------

# Carregar pacotes -------------------------------------------------------------------------------------------------------------------------

library(tidyverse)
library(gt)

# Cálculos de média, mediana e desvio-padrão da estrutura da vegetação por módulo (diâmetro do caule, altura e fitossanidade)  -------------

### Diâmetro do caule e altura

var_floresta <- readxl::read_xlsx("dados.xlsx")
view(var_floresta)

## DAP

var_flor_descritiva_dap <- var_floresta |>
  select(modulo, parcelas, dap_cm) |>
  drop_na() |>
  group_by(modulo) |>
  summarise(med_dap = mean(dap_cm),
            median_dap = median(dap_cm),
            n_dap = n(),
            dp_dap = sd(dap_cm),
            se_dap = dp_dap/sqrt(n_dap))

view(var_flor_descritiva_dap)

gt(var_flor_descritiva_dap) %>%
  fmt_number(columns = 2:6,
             decimals = 2,
             dec_mark = ",",
             sep_mark = ".",
              drop_trailing_zeros = T) %>%
    cols_label(modulo = "Módulos",
             med_dap = "Médias",
             median_dap = "Medianas",
             n_dap = "Amostras",
             dp_dap = "Desvio padrão",
             se_dap = "Erro padrão") 

## Altura

var_flor_descritiva_alt <- var_floresta |>
  select(modulo, parcelas, altura_m) |>
  drop_na() |>
  group_by(modulo) |>
  summarise(med_alt = mean(altura_m),
            median_alt = median(altura_m),
            n_alt = n(),
            dp_alt = sd(altura_m),
            se_alt = dp_alt/sqrt(n_alt))

view(var_flor_descritiva_alt)

gt(var_flor_descritiva_alt) %>%
  fmt_number(columns = 2:6,
             decimals = 2,
             dec_mark = ",",
             sep_mark = ".",
              drop_trailing_zeros = T) %>%
    cols_label(modulo = "Módulos",
             med_alt = "Médias",
             median_alt = "Medianas",
             n_alt = "Amostras",
             dp_alt = "Desvio padrão",
             se_alt = "Erro padrão") 

plot_desc_dap <- ggplot(var_flor_descritiva, 
                          aes(x = modulo, y = med_dap)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_dap + se_dap,
                     ymin = med_dap - se_dap),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Diâmetro a altura do peito (cm)", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_dap

plot_desc_alt <- ggplot(var_flor_descritiva, 
                          aes(x = modulo, y = med_alt)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_alt + se_alt,
                     ymin = med_alt - se_alt),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Altura (m)", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_alt

# Estrutura da vegetação por módulo - Área basal ---------------------------------------------

var_floresta1 <- readxl::read_xlsx("dados_arboreas_parcelas.xlsx")
view(var_floresta1)
glimpse(var_floresta1)

var_floresta1$parcelas <- as.factor(var_floresta1$parcelas)
var_floresta1$modulos <- as.factor(var_floresta1$modulos)
var_floresta1$area_basal_m2 <- as.double(var_floresta1$area_basal_m2)

var_flor_descritiva_ab <- var_floresta1 |>
  select(modulos, parcelas, area_basal_m2) |>
  group_by(modulos) |>
  summarise(med_area = mean(area_basal_m2),
            median_area = median(area_basal_m2),
            n_area = n(),
            dp_area = sd(area_basal_m2),
            se_area = dp_area/sqrt(n_area))

view(var_flor_descritiva_ab)

gt(var_flor_descritiva_ab) %>%
  fmt_number(columns = 2:6,
             decimals = 2,
             dec_mark = ",",
             sep_mark = ".",
              drop_trailing_zeros = T) %>%
    cols_label(modulos = "Módulos",
             med_area = "Médias",
             median_area = "Medianas",
             n_area = "Amostras",
             dp_area = "Desvio padrão",
             se_area = "Erro padrão") 

plot_desc_ab <- ggplot(var_flor_descritiva1, 
                          aes(x = modulos, y = med_area)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_area + se_area,
                     ymin = med_area - se_area),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Área basal (m²)", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_ab

### Fitossanidade

fito <- readxl::read_xlsx("dados.xlsx")

fito <- fito %>%
  dplyr::select(parcelas, modulo, fitossanidade) 

view(fito)

## Calcular número de plantas saudáveis

fito1 <- fito %>%
  filter(fitossanidade %in% c("bom", "otimo")) %>%
  drop_na()

view(fito1)
glimpse(fito1)

fito1 <- fito1 |> 
  mutate(fitossanidade = as.factor(fitossanidade),
         modulo = as.factor(modulo))
glimpse(fito1)

## Calcular número de plantas saudáveis

fito1.1 <- fito1 %>%
  dplyr::select(-fitossanidade) %>%
  group_by_all() %>%
  summarise(n_fito = n()) 

view(fito1.1)

fito2 <- fito1.1 |>
  dplyr::group_by(modulo) |>
  dplyr::summarise(media_fito = mean(n_fito),
            mediana_fito = median(n_fito),
            n_fito = n(),
            desvio_fito = sd(n_fito),
            se_fito = desvio_fito/sqrt(n_fito))

view(fito2)

gt(fito2) %>%
  fmt_number(columns = 2:6,
             decimals = 2,
             dec_mark = ",",
             sep_mark = ".",
              drop_trailing_zeros = T) %>%
    cols_label(modulo = "Módulos",
             media_fito = "Média",
             mediana_fito = "Mediana",
             n_fito = "Amostras",
             desvio_fito = "Desvio padrão",
             se_fito = "Erro padrão")

## Para calcular os desvios padrão por grupo

sd_m1 <- sd(fito1.1$n_fito[fito1.1$modulo == unique(fito1.1$modulo)[1]], 
   na.rm = TRUE)
sd_m2 <- sd(fito1.1$n_fito[fito1.1$modulo == unique(fito1.1$modulo)[2]], 
   na.rm = TRUE)
sd_m3 <- sd(fito1.1$n_fito[fito1.1$modulo == unique(fito1.1$modulo)[3]], 
   na.rm = TRUE)
sd_m4 <- sd(fito1.1$n_fito[fito1.1$modulo == unique(fito1.1$modulo)[4]], 
   na.rm = TRUE)
sd_m5 <- sd(fito1.1$n_fito[fito1.1$modulo == unique(fito1.1$modulo)[5]], 
   na.rm = TRUE)
sd_m6 <- sd(fito1.1$n_fito[fito1.1$modulo == unique(fito1.1$modulo)[6]], 
   na.rm = TRUE)

## Cálculos erros padrão

se_m1 <- sd_m1/sqrt(10)
se_m2 <- sd_m2/sqrt(10)
se_m3 <- sd_m3/sqrt(10)
se_m4 <- sd_m4/sqrt(10)
se_m5 <- sd_m5/sqrt(10)
se_m6 <- sd_m6/sqrt(10)

# Cálculos de média, mediana e desvio-padrão das variáveis ambientais por módulo -----------------------------------------------------------

var_amb <- readxl::read_xlsx("dados_solo_ambiente.xlsx")
view(var_amb)
glimpse(var_amb)

var_amb1 <- var_amb |>
  drop_na() |>
  mutate(modulos = as.factor(modulos)) |>
  group_by(modulos) |>
  summarise(med_ph = mean(ph), med_areia = mean(areia), 
            med_silte = mean(silte), med_arg = mean(argila), 
            med_som = mean(soma_bases), med_n = mean(N),
            med_p = mean(P), med_k = mean(K), 
            med_ca = mean(Ca), med_mg = mean(Mg),
            median(ph), median(areia), median(silte),
            median(argila), median(soma_bases), median(N),
            median(P), median(K), median(Ca), median(Mg),
            sd_ph = sd(ph), sd_areia = sd(areia), 
            sd_silte = sd(silte), sd_arg = sd(argila), 
            sd_som = sd(soma_bases), sd_n = sd(N), 
            sd_p = sd(P), sd_k = sd(K), sd_ca = sd(Ca),
            sd_mg = sd(Mg),n = n(),
           se_ph = sd_ph/sqrt(n), se_areia = sd_areia/sqrt(n),
            se_silte = sd_silte/sqrt(n), se_arg = sd_arg/sqrt(n), 
            se_som = sd_som/sqrt(n), se_n = sd_n/sqrt(n), 
            se_p = sd_p/sqrt(n), se_k = sd_k/sqrt(n), 
           se_ca = sd_ca/sqrt(n), se_mg = sd_mg/sqrt(n))

view(var_amb1)

## pH

plot_desc_ph <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_ph)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_ph + se_ph,
                     ymin = med_ph - se_ph),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "pH", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_ph

## Areia

plot_desc_areia <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_areia)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_areia + se_areia,
                     ymin = med_areia - se_areia),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Areia", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_areia

## Silte

plot_desc_silte <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_silte)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_silte + se_silte,
                     ymin = med_silte - se_silte),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Silte", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_silte

## Argila

plot_desc_arg <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_arg)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_arg + se_arg,
                     ymin = med_arg - se_arg),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Argila", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_arg

## Soma das bases

plot_desc_som <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_som)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_som + se_som,
                     ymin = med_som - se_som),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Soma das bases", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_som

## Nitrogênio

plot_desc_n <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_n)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_n + se_n,
                     ymin = med_n - se_n),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Nitrogênio", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_n

## Fósforo

plot_desc_p <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_p)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_p + se_p,
                     ymin = med_p - se_p),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Fósforo", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_p

## Potássio

plot_desc_k <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_k)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_k + se_k,
                     ymin = med_k - se_k),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Potássio", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_k

## Cálcio 

plot_desc_ca <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_ca)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_ca + se_ca,
                     ymin = med_ca - se_ca),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Cálcio", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_ca

## Magnésio

plot_desc_mg <- ggplot(var_amb1, 
                          aes(x = modulos, y = med_mg)) +
    geom_col(color = "white",
             fill = "forestgreen") +
   geom_errorbar(aes(ymax = med_mg + se_mg,
                     ymin = med_mg - se_mg),
                   size = 0.8, width = 0.2) +
  # scale_x_discrete(breaks = c(M1, M2, M3, M4, M5, M6),
  #                  labels = c("M1", "M2", "M3", 
  #                             "M4", "M5", "M6")) +
    labs(y = "Magnésio", 
         x = "Módulos") +
    theme_bw() +
    theme(text = element_text(size = 15,color = "black"),
        axis.text = element_text(color = "black", size = 13),
        legend.position = "none")

plot_desc_mg

# Tabelas com números de espécies, indivíduos e famílias por módulos -----------------------------------------------------------------------

## Número de famílias

var_familia <- readxl::read_xlsx("dados.xlsx")
view(var_familia)
glimpse(var_familia)

var_familia$modulo <- as.factor(var_familia$modulo)
var_familia$familia <- as.factor(var_familia$familia)

var_familia <- var_familia |>
  select(modulo, parcelas, familia)

## Número de famílias por módulos

var_familia1 <- var_familia |>
  group_by(modulo) |>
  summarise(n_familias = n_distinct(familia)) |>
  arrange(n_familias)

view(var_familia1)

## Número de indivíduos por família

ind_por_familia <- var_familia %>%
  count(familia, name = "n_individuos") %>%
  arrange(n_individuos)

view(ind_por_familia)

## Número de espécies

dados <- readxl::read_xlsx("tab_pres_aus_sp.xlsx")
view(dados)

dados_sp <- dados %>%
  pivot_longer(cols = contains((" ")),
               names_to = "especie",       
               values_to = "abundancia")

view(dados_sp)

## Gerar tabela de riqueza

library(vegan) # Uso da função specnumber

Riqueza <- specnumber(dados)
Riqueza
view(Riqueza)

riqueza <- as.data.frame(Riqueza)
view(riqueza)

riqueza <- riqueza %>%
  mutate(parcela = 1:60) %>%
  rename(parcelas = parcela)
view(riqueza)

# riqueza <- riqueza %>%
#   filter(parcelas != 59) %>% # Parcela sem dados
#   view()

## Gerar tabela de abundância

abundancia <- dados_sp %>%
  group_by(parcelas) %>%
  summarise(abundancia = sum(abundancia)) 
view(abundancia)

### Juntar tabelas 

view(riqueza)
view(abundancia)

abund_sp <- left_join(abundancia, riqueza, by = 'parcelas') 
view(abund_sp)

### Adicionar coluande modulos

abund_sp <- abund_sp %>%
  mutate(modulo = 1:60) |>
  mutate(modulos = case_when(
      modulo <= 10 ~ "M1",
      modulo <= 20 ~ "M2",
      modulo <= 30 ~ "M3",
      modulo <=40 ~ "M4",
      modulo <= 50 ~ "M5",
      modulo <= 60 ~ "M6"))

abund_sp <- abund_sp %>%
  select(-modulo)

view(abund_sp)
glimpse(abund_sp)

## Definir estrutura dos dados 

abund_sp$parcelas <- as.factor(abund_sp$parcelas)
abund_sp$Riqueza <- as.integer(abund_sp$Riqueza)
abund_sp$abundancia <- as.numeric(abund_sp$abundancia)
abund_sp$modulos <- as.factor(abund_sp$modulos)

tab_abu <- abund_sp |>
  group_by(modulos) |>
  summarise(med_abu = mean(abundancia),
            median_abu = median(abundancia),
            n_abu = n(),
            dp_abu = sd(abundancia),
            se_abu = dp_abu/sqrt(n_abu))

view(tab_abu)

gt(tab_abu) %>%
  fmt_number(columns = 2:6,
             decimals = 2,
             dec_mark = ",",
             sep_mark = ".",
              drop_trailing_zeros = T) %>%
    cols_label(modulos = "Módulos",
             med_abu = "Médias",
             median_abu = "Medianas",
             n_abu = "Amostras",
             dp_abu = "Desvio padrão",
             se_abu = "Erro padrão")

tab_riq <- abund_sp |>
  group_by(modulos) |>
  summarise(med_riq = mean(Riqueza),
            median_riq = median(Riqueza),
            n_riq = n(),
            dp_riq = sd(Riqueza),
            se_riq = dp_riq/sqrt(n_riq))

view(tab_riq)

gt(tab_riq) %>%
  fmt_number(columns = 2:6,
             decimals = 2,
             dec_mark = ",",
             sep_mark = ".",
              drop_trailing_zeros = T) %>%
    cols_label(modulos = "Módulos",
             med_riq = "Médias",
             median_riq = "Medianas",
             n_riq = "Amostras",
             dp_riq = "Desvio padrão",
             se_riq = "Erro padrão")

