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

out.duncan$duncan

# Gráfico Duncan
plot(out.duncan, main = "Composição de substratos")

?duncan.test
?plot.agricolae
