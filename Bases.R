#################### Seminário Final ####################
# Nome: Deivison Venicio Souza 😀                    ####
# E-mail= deivisonvs@ufpa.br                         ####
# Instituição: Universidade Federal do Pará - UFPA   ####
#########################################################

# 1: Instalar os pacotes ---------------------------------

install.packages('agricolae')
install.packages('ExpDes.pt') # https://www.quantargo.com/help/r/latest/packages/ExpDes.pt/ex5.html/dic
install.packages('AgroR')
install.packages("http://leg.ufpr.br/~walmes/pacotes/labestData_0.1.1.462.zip",
                 repos = NULL) # labestData Para Windows.
install.packages("http://leg.ufpr.br/~walmes/pacotes/labestData_0.1-1.462.tar.gz",
                 repos = NULL) # labestData Para Linux.

# 2: Carregar os pacotes ---------------------------------

library(labestData) # Dados para Aprendizado de Estatística
library(agricolae)
library(ExpDes.pt)
library(AgroR)
library(tidyverse)

# 3: Sobre o pacote labestData ---------------------------------

str(PimentelEg5.2)            # Mostra a estrutura do dataset.
help(PimentelEg5.2)           # Mostra as informações de ajuda do dataset.
labestDataView()              # Abre aplicação shiny para navegar pelos dados.
browseVignettes("labestData") # Abre a lista de vinhetas no navegador.
help(obras)                   # Documentação das obras contidas no pacote.

# 4: Alguns conjuntos de dados ---------------------------------
data <- read_csv(file = "Slides/data/DIC-Ficticio.csv")
dataX <- read_csv(file = "Slides/data/DIC_Ficticio2.csv")
data1 <- labestData::BanzattoQd7.3.1 # Grupo 1 (DBC)
data2 <- labestData::BarbinEx13      # Grupo 2 (DBC)
data3 <- labestData::BarbinEx18      # Grupo 3
data4 <- labestData::BarbinEx8       # Grupo 4
data5 <- labestData::BarbinPg177     # Grupo 5 (DBC)
data6 <- labestData::StorckTb2       # Grupo 6
data7 <- labestData::FariaQd11.9     # Grupo 7

# 5: Ajuste dos modelos ----------------------------------------

m0 <- lm(dc ~ Substrato, data = data)
mX <- lm(CPA ~ Sub, data = dataX)
m1 <- lm(alt ~ dt + bloco, data = data1)
m2 <- lm(diam ~ euca + bloc, data = data2)
m3 <- lm(alt ~ espec, data = data3)
m4 <- lm(alt ~ fumig, data = data4)
m5 <- lm(alt ~ prog + bloc, data = data5)
m6 <- lm(peso ~ trat, data = data6)
m7 <- lm(quali ~ recip, data = data7)

# 6: Quadro de ANOVA ----------------------------------------
anova(m0)
anova(mX)
anova(m1)
anova(m2)
anova(m3)
anova(m4)
anova(m5)
anova(m6)
anova(m7)

# Pressuposições da ANOVA
par(mfrow = c(2, 2))
plot(mX); layout(1)

#------------------------------------------
# Usando o pacote agricolae
#------------------------------------------
# Teste de Tukey
out.tukey <- LSD.test(m0, "Substrato", group=TRUE,
                          alpha=0.05, console=TRUE)
bar.group(out.tukey$group)
bar.err(out.tukey$means)
plot(out.tukey, variation="range",las=1)

# Teste de Duncan
out.duncan <- duncan.test(m0, "Substrato", group=TRUE,
                          alpha=0.05, console=TRUE)
bar.group(out.duncan$group)
bar.err(out.duncan$means)
plot(out.duncan,variation="range",las=1,
     main = "Composição de substratos")

#------------------------------------------
# Usando o Pacote AgroR
#------------------------------------------
library(AgroR)
attach(dataX)
DIC(Substrato, h, mcomp = "tukey")
DIC(Substrato, dc, mcomp = "tukey")
DIC(Sub, CR, mcomp = "tukey")
DIC(Sub, CPA, mcomp = "tukey")

DBC(dt, bloco, alt, mcomp = "duncan")      # Grupo 3
DBC(euca, bloc, diam)                      # Grupo 6
DIC(espec, alt)                            # Grupo 4
DIC(fumig, alt)                            # Grupo 2
DBC(prog, bloc, alt)                       # Grupo 7
DIC(trat, peso)                            # Grupo 5
DIC(recip, quali)                          # Grupo 1

#------------------------------------------
# Usando o Pacote ExpDes.pt
#------------------------------------------
help(ExpDes.pt)
dic(
  trat = Sub,
  resp = cr,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05
)

dic(
  trat = Substrato,
  resp = dc,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05
)

