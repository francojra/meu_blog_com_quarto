library(lavaan)

# 1. Carregar dados
data("HolzingerSwineford1939")
dados <- HolzingerSwineford1939

View(dados)

# 2. Definir modelo (CFA)
modelo <- '
  # Fatores latentes
  espacial =~ x1 + x2 + x3
  verbal =~ x4 + x5 + x6
  velocidade =~ x7 + x8 + x9
'

# 3. Ajustar modelo
ajuste <- cfa(modelo, data = dados)

# 4. Ver resultados
summary(ajuste, fit.measures = TRUE, standardized = TRUE)

# 5. Plotar (requer semPlot)
library(semPlot)

semPaths(
  ajuste,
  whatLabels = "std",      # Mostra coeficientes padronizados
  edge.label.cex = 2.3,    # Ajusta tamanho dos rótulos
  layout = "tree2",       # Layout circular (alternativa)
  style = "lisrel",           # Estilo alternativo "ram"
  color = list(
    lat = "cyan3",  # Cor das latentes
    man = "cyan" # Cor das observadas
  ),
  mar = c(4, 4, 4, 4),      # Margens do gráfico
  
    # Cores personalizadas:
  edge.color = "black",   # Cor das linhas/setas
  edge.label.color = "red",  # Cor dos rótulos das setas
  edge.width = 3.5,    # Espessura das linhas e setas
  
    # Estilo dos nós:
  label.cex = 1.3,           # Tamanho dos rótulos dentro dos nós
  label.color = "black",     # Cor dos rótulos internos
  label.scale = FALSE,       # Evita redimensionamento automático
  
    sizeMan = 8,              # Tamanho dos retângulos (variáveis observadas)
  sizeLat = 9,              # Tamanho dos círculos (variáveis latentes)
  
  nodeLabels = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9",
                 "Espacial", "Verbal", "Velocidade")
)
