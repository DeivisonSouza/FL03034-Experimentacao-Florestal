# Seminário Final

# Carrega os pacotes necessários
library(agricolae)
library(labestData)
library(ExpDes.pt)
library(AgroR)
library(dplyr)

# Bases do pacote labestData
# labestData::labestDataView()
# labestData::BarbinPg114 (hipsometros)

data <- labestData::BanzattoQd7.3.1 # Grupo 3 (DBC)
data <- labestData::BarbinEx13      # Grupo 6 (DBC)
data <- labestData::BarbinEx18      # Grupo 4
data <- labestData::BarbinEx8       # Grupo 2
data <- labestData::BarbinPg177     # Grupo 7 (DBC)
data <- labestData::StorckTb2       # Grupo 5
data <- labestData::FariaQd11.9     # Grupo 1

?max


#labestData::DemetrioEx1.4.1.4

# Realizar a análise de variância (R-base)

res.aov <- aov(alt ~ dt + bloco, data = data)     # Grupo 3
res.aov <- aov(diam ~ euca + bloc, data = data)   # Grupo 6
res.aov <- aov(alt ~ espec, data = data)          # Grupo 4
res.aov <- aov(alt ~ fumig, data = data)          # Grupo 2
res.aov <- aov(alt ~ prog + bloc, data = data)    # Grupo 7
res.aov <- aov(peso ~ trat, data = data)          # Grupo 5
res.aov <- aov(quali ~ recip, data = data)        # Grupo 1

# Resumo da análise de variância
summary(res.aov)

#------------------------------------------
# Usando o pacote agricolae
#------------------------------------------
# Teste de Duncan
out.duncan <- duncan.test(res.aov, "recip", group=TRUE,
                          alpha=0.05, console=TRUE)

# Gráfico Duncan
plot(out.duncan, main = "Composição de substratos")

# Teste de Tukey
out.tukey <- HSD.test(res.aov, "comp", group=TRUE,
                      alpha=0.05, console=TRUE)

# Gráfico Tukey
plot(out.tukey, main = "Composição de substratos")

#-----------------------
# Pacote AgroR
library(AgroR)
attach(data)
DBC(dt, bloco, alt, mcomp = "duncan")      # Grupo 3
DBC(euca, bloc, diam)                      # Grupo 6
DIC(espec, alt)                            # Grupo 4
DIC(fumig, alt)                            # Grupo 2
DBC(prog, bloc, alt)                       # Grupo 7
DIC(trat, peso)                            # Grupo 5
DIC(recip, quali)                          # Grupo 1
