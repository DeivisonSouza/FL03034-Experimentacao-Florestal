library(dplyr)
library(agricolae)

data <- readr::read_csv("Slides/data/DIC-Pimentel-Gomes-2002.csv")

data_pivot <- data %>%
  tidyr::pivot_longer(
    cols = starts_with("P"),
    names_to = "Progenie",
    values_to = "Valor",
    names_transform = list(Progenie = as.factor),
    values_drop_na = TRUE
  ) %>%
  rename(Repeticao = `Rep./Prog.`) %>%
  arrange(Progenie, Repeticao)

# Realizar a análise de variância
res.aov <- aov(Valor ~ Progenie, data = data_pivot)

# Resumo da análise de variância
summary(res.aov)

# Teste de Duncan
out.duncan <- duncan.test(res.aov,"Progenie",main="",
                          alpha=0.05, console=TRUE)

# Teste Tukey
out.tukey <- HSD.test(res.aov, "Progenie", group=TRUE,
                      console=TRUE, main="")

# Gráfico Duncan
plot(out.duncan, variation="IQR")

# Gráfico Tukey
plot(out.tukey)
