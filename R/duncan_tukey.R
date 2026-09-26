############################################################################
# ANOVA e Teste de Comparações de Médias - Experimento em DIC
############################################################################

# Passo1: Instala pacotes...
install.packages("agricolae")
install.packages("labestData")
install.packages("pastecs")

# Passo 2: Carrega os pacotes...
library(agricolae)   # ??????
library(labestData)  # bases de dados
library(pastecs)     # estatistica descritiva
library(dplyr)

# Passo 3: Carrega o conjunto de dados
data <- labestData::BarbinEx18
data1 <- labestData::BanzattoQd7.3.1
data3 <- labestData::BarbinEx8

data <- labestData::BarbinPg177

write.csv(data, file = "Teste/data.csv")

write.csv(data1, file = "Teste/data1.csv")



x <- 1:10
y <- 11:20

plot(x, y)




write.csv(data1, file = "Teste/data1.csv")

library(readr)
data <- read.csv("Teste/data1.csv")


# Passo 4: Analise Exploratória dos Dados
# Estatistica descritiva quantitativa

stat.desc(data$alt)

data %>%
  group_by(espec) %>%
  summarise(
    count = n(),
    mean = mean(alt, na.rm = TRUE),
    sd = sd(alt, na.rm = TRUE),
    max = max(alt, na.rm = TRUE),
    min = min(alt, na.rm = TRUE)
  )

data1 %>%
  group_by(dt) %>%
  summarise(
    count = n(),
    mean = mean(alt, na.rm = TRUE),
    sd = sd(alt, na.rm = TRUE),
    max = max(alt, na.rm = TRUE),
    min = min(alt, na.rm = TRUE)
  )

# Visualização...
# BoxPlot
library(ggplot2)

ggplot(data = data3, aes(x = fumig, y = alt, fill = fumig)) +
  geom_boxplot() +
  geom_jitter() +
  stat_summary(fun=mean, geom="point",
               shape=2, size=2, color="blue",
               fill="red")


data3 %>%
  group_by(fumig) %>%
  summarise(media = mean(alt)) %>%
  ggplot() +
  geom_bar(aes(x = fumig, y = media, fill = fumig),
           stat = "identity") +
  geom_label(aes(x = fumig, y = media,
                 label = round(media,2)),
             size = 3) +
  labs(y = "Altura das plântulas (cm)",
       x = "Fumigante",
       title = "Médias de alturas por fumig")





ggplot(data=data3, aes(x = fumig, y = alt,
                      fill = fumig)) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Boxplot básico") +
  stat_summary(fun=mean, geom="point",
               shape=20, size=4, color="red", fill="red")

ggplot(data=data1, aes(x = as.factor(dt), y = alt,
                      fill = as.factor(dt))) +
  geom_boxplot() +
  geom_jitter() +
  ggtitle("Boxplot básico") +
  stat_summary(fun=mean, geom="point",
               shape=20, size=4, color="red", fill="red")

# Grafico barra

# Passo 5: Analise de Variancia (ANOVA)
# Qual delineamento - DIC...
# DBC...

# DIC
res <- aov(alt ~ espec, data = data)
summary(res)

# DBC
res1 <- aov(alt ~ dt + bloco , data = data1)
summary(res1)




# Interpretar o teste F? P-valor < 0,05: Rejeita H0.


# Passo 6: Testes de comparações de médias
# Teste Tukey ou Teste de Duncan...

# agricolae
# AgroR
# ExpDest.pt
# Outros...

# Teste de Duncan

out.duncan <- duncan.test(res, "espec", group = T,
            alpha = 0.05, console=T)

# Teste de Tukey

out.tukey <-HSD.test(res, "espec", group = T,
            alpha = 0.05, console=T)


# Gráfico Duncan
plot(out.duncan, main = "Teste de Duncan")
plot(out.tukey)

# Conclui????????

