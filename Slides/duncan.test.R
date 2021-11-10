# ANOVA e Teste de Comparações de Médias - Experimento em DIC
############################################################################

# Carrega os pacotes necessários
library(dplyr)
library(agricolae)
library(AgroR)

# Acessa o conjunto de dados
(data <- labestData::BanzattoQd3.7.1)

# Realizar a análise de variância
res.aov <- aov(alt ~ comp, data = data)

# Resumo da análise de variância
summary(res.aov)

#------------------------------------------
# Usando o pacote agricolae

# Teste de Duncan
out.duncan <- duncan.test(res.aov, "comp", group=TRUE,
                          alpha=0.05, console=TRUE)

out.duncan$duncan

# Gráfico Duncan
plot(out.duncan, main = "Composição de substratos")

#------------------------------------------
# Usando o pacote AgroR
library(AgroR)
with(data, DIC(comp, alt, ylab = "Weight loss (%)",
               mcomp = "duncan", geom="point"))

# Tarefa: (Este script está incompleto. Portanto, adicione códigos para fazer as tarefas abaixo)
# 1 - Faça uma análise exploratória dos dados.
# 2 - Procure: Como analisar as pressuposições da ANOVA?
# 3 - Faça o Teste de Tukey usando os pacotes
