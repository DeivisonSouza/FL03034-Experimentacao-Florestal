library(dplyr)
library(agricolae)

(data <- labestData::BanzattoQd3.7.1)

# Realizar a análise de variância
res.aov <- aov(alt ~ comp, data = data)

# Resumo da análise de variância
summary(res.aov)

# Teste de Duncan
out.duncan <- duncan.test(res.aov,"comp", group=TRUE,
                          alpha=0.05, console=TRUE)

# Gráfico Duncan
plot(out.duncan, variation="IQR")

# Gráfico Tukey
plot(out.tukey, main = "Competição de Progênies")

?duncan.test
?plot.agricolae
