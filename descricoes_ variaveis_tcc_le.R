getwd()
setwd("/Users/Mariana/Desktop/GHC_Bases_de_dados/TCC Leticia/")

dados_Leticia <- read.csv(file = "BD_LETICIA09052019.csv", header = TRUE, sep = ";")
View(dados_Leticia)
attach(dados_Leticia) #database can be accessed by simply giving their names (cuidado quando tiver +de1 database)
library(fields) #stats
library (descr)
names(dados_Leticia)

#Discriminacao das variaveis
str_dados_Leticia <- str(dados_Leticia) #tipo de todas variaveis da tabela
class(dados_Leticia$paSist) #tipo de variavel

# FREQUENCIAS 
library (descr)
freq(qsofa)
freq(qsofa_atual)
freq(morte1e2)
freq(ghc_sepse1e2)
freq(qsofa >=1)
freq(dados_Leticia$ghc_sepse1e2 == 1, dados_Leticia$morte1e2 == 1)

# Criacao de data.frames separados para sepse e nao sepse
sepse_1 <- subset(dados_Leticia, ghc_sepse1e2 == 1)
sepse_2 <- subset(dados_Leticia, ghc_sepse1e2 == 2)
write.csv2(x=sepse_2 , file = "sepse_2.csv", row.names = FALSE)


###### DESCRICAO SINAIS VITAIS
### paSist
summary(dados_Leticia$paSist)
stats (dados_Leticia$paSist)

#Verificando normalidade (paSist)
#A hipotese nula do teste de Shapiro-Wilk eh que a populacao possui distribuicao normal. Portanto, 
#um valor de p < 0.05 os dados n?o possuem distribuicao normal.
shapiro.test(dados_Leticia$paSist) 
options(scipen=999) # remover nota??o cientifica
hist(dados_Leticia$paSist) #histograma nao eh bom para avaliar variaveis discretas
# Coeficiente de assimetria
library(moments)
skewness(dados_Leticia$paSist) 
# paSist = Anormal e 0.89 (assim?trico mod) -> testes n?o param?tricos
# qq norm=> gráfica
qqnorm(paSist)
qqline(paSist)

stats(sepse_1$paSist)
stats(sepse_2$paSist)

#### Teste Wilcoxon (Mann-Whitney)
wilcox.test(paSist~ghc_sepse1e2)

### paDist
stats(sepse_1$paDiast)
paDiast2_sem0 <- subset(sepse_2, paDiast != 0) #dois pacientes = 0
stats(paDiast2_sem0$paDiast) 

shapiro.test(paDiast)
wilcox.test(paDiast~ghc_sepse1e2)

### fc
stats(dados_Leticia$fc)
stats(sepse_1$fc)
fc2_sem0 <- subset(sepse_2, fc != 0) #5 pacientes = 0
stats(fc2_sem0$fc)
shapiro.test(fc2_sem0)

wilcox.test(fc~ghc_sepse1e2)

### fr
stats(dados_Leticia$fr)
stats(sepse_1$fr)
stats(sepse_2$fr)

shapiro.test(fr)
wilcox.test(fr~ghc_sepse1e2)


### satO2
satO2_1_sem0 <- subset(sepse_1, satO2 != 0) #3 pacientes = 0
stats(satO2_1_sem0$satO2)
satO2_2_sem0 <- subset(sepse_2, satO2 != 0) #9 pacientes = 0
stats(satO2_2_sem0$satO2)

shapiro.test(dados_Leticia$satO2)

wilcox.test(satO2~ghc_sepse1e2)

### tax
tax1_sem0 <- subset(sepse_1, tax != 0) # 16 pacientes = 0
stats(tax1_sem0$tax)
tax2_sem0 <- subset(sepse_2, tax != 0) # 113 pacientes = 0
stats(tax2_sem0$tax)

shapiro.test(tax)
wilcox.test(tax~ghc_sepse1e2)


### hgt
hgt1_sem0 <- subset(sepse_1, hgt != 0)  # 7 pac = 0
stats(hgt1_sem0$hgt)
hgt2_sem0 <- subset(sepse_2, hgt != 0) #40 pac = 0
stats(hgt2_sem0$hgt)

shapiro.test(hgt)
wilcox.test(hgt~ghc_sepse1e2)

### glasgow
stats(sepse_1$glasgow)
stats(sepse_2$glasgow)

shapiro.test(glasgow)
wilcox.test(glasgow~ghc_sepse1e2)


###### RISCO RELATIVO

# RR qSOFA >=2 E SEPSE (ghc_sepse1e2)
RR_qsofa2_sepse <- table(qsofa_atual >=2, ghc_sepse1e2) #Criar tabela 2x2
RR_qsofa2_sepse
a<- 19 
b<- 94 
c<- 33 
d<- 174
RR_exp <- a/(a+b)
RR_exp
RR_naoexp <- c/(c+d)
RR_naoexp
RR <- RR_exp / RR_naoexp
RR

library(epiR)
# RR, odds ratio
epi.2by2(RR_qsofa2_sepse, method = "cohort.count", conf.level = 0.95)

# RR qSOFA >=1 E SEPSE
RR_qsofa1_sepse <- table(qsofa_atual >=1, ghc_sepse1e2)
RR_qsofa1_sepse
a <- 45
b <- 199
c <- 7
d <- 69
a/(a+b)
c/(c+d)
RR_qsofa1_sepseM <- 0.1844262/0.09210526
RR_qsofa1_sepseM

epi.2by2(RR_qsofa1_sepse, method = "cohort.count", conf.level = 0.95)

# RR qSOFA >=2 e MORTE (morte1e2)
RR_qsofa2_morte <- table(qsofa_atual >=2, morte1e2)
RR_qsofa2_morte
a <- 53
b <- 60
c <- 41
d <- 166
RR_exp <- a/(a+b)
RR_exp
RR_naoexp <- c/(c+d)
RR_naoexp
RR <- RR_exp / RR_naoexp
RR


epi.2by2(RR_qsofa2_morte, method = "cohort.count", conf.level = 0.95)

# RR qSOFA >=1 e MORTE (morte1e2)
RR_qsofa1_morte <- table(qsofa_atual >=1, morte1e2)
RR_qsofa1_morte
a <- 83
b <- 161
c <- 11
d <- 65
RR_exp <- a/(a+b)
RR_exp
RR_naoexp <- c/(c+d)
RR_naoexp
RR <- RR_exp / RR_naoexp
RR


epi.2by2(RR_qsofa1_morte, conf.level = 0.95)

###### SENSIBILIDADE, ESPEC, VPP, VPN ######
sevpp <- function(a, b, c, d) {
  sensib <- a/(a+c)
  espec <- d/(b+d)
  vpp <- a/(a+b)
  vpn <- d/(c+d)
  lista <- list("Sensib" = sensib, "Espec" = espec, "VPP" = vpp, "VPN" = vpn)
  return (lista)
}

### sepse 1 e 2
tab_freq_1 <- table(qsofa_atual >=1, ghc_sepse1e2)
tab_freq_1
a <- 45
b <- 199
c <- 7
d <- 69
qsofa1_E_sepse <- sevpp(45, 199, 7,69)
qsofa1_E_sepse

tab_freq_2 <- table(qsofa >=2, ghc_sepse1e2)
tab_freq_2
a <- 12
b <- 74
c <- 40
d <- 194
qsofa2_E_sepse <- sevpp(12,74,40,194)
qsofa2_E_sepse

tab_freq_3 <- table(qsofa == 0, ghc_sepse1e2)
tab_freq_3
a <- 10
b <- 88
c <- 42
d <- 180
qsofa2_E_sepse <- sevpp(10,88,42,180)
qsofa2_E_sepse


### Morte 1(óbito) e 2(alta)
tab_freq_morte1 <- table(qsofa_atual >=1 ,morte1e2)
tab_freq_morte1 
a <- 83
b <- 161
c <- 11
d <- 65
qsofa1_E_morte <- sevpp(83,161,11,65)
qsofa1_E_morte

tab_freq_morte2 <- table(qsofa_atual >=2 ,morte1e2)
tab_freq_morte2 
a <- 53
b <- 60
c <- 41
d <- 166
qsofa2_E_morte <- sevpp(53,60,41,166) 
qsofa2_E_morte

tab_freq_3 <- table(qsofa == 0, morte1e2)
tab_freq_3
a <- 16
b <- 82
c <- 78
d <- 144
qsofa2_E_sepse <- sevpp(16,82,78,144)
qsofa2_E_sepse

### paSist
tab_freq_paSist <- table (qsofa>=1, paSist <= 100) 
tab_freq_paSist
a <- 97
b <- 125
c <- 11
d <- 87
qsofa1_E_paSist1_100 <- sevpp(97,125,11,87)
qsofa1_E_paSist1_100


tab_freq_paSist2 <- table (qsofa>=2, paSist <= 100) 
tab_freq_paSist2
a <- 57
b <- 29
c <- 51
d <- 183
qsofa1_E_paSist1_100 <- sevpp(57,29,51,183)
qsofa1_E_paSist1_100

### freq Respiratória
tab_freq_fr1 <- table(dados_Leticia$qsofa >= 1, dados_Leticia$fr >= 22)
tab_freq_fr1
a <- 145
b <- 77
c <- 13
d <- 85
qsofa1_E_fr <- sevpp(145,77,13,85)
qsofa1_E_fr 

tab_freq_fr2 <- table(dados_Leticia$qsofa >= 2, dados_Leticia$fr >= 22)
tab_freq_fr2
a <- 76
b <- 10
c <- 82
d <- 152
qsofa1_E_fr <- sevpp(76,10,82,152)
qsofa1_E_fr 

### Glasgow <15
tab_freq_glasgow1 <- table(dados_Leticia$qsofa >=1, dados_Leticia$glasgow <15)
tab_freq_glasgow1
a <- 121
b <- 101
c <- 0
d <- 98
qsofa_E_glasgow1 <- sevpp(121,101,0,98)
qsofa_E_glasgow1


tab_freq_glasgow2 <- table(dados_Leticia$qsofa >=2, dados_Leticia$glasgow <15)
tab_freq_glasgow2
a <- 67
b <- 19
c <- 54
d <- 180
qsofa_E_glasgow2 <- sevpp(67, 19, 54, 180)
qsofa_E_glasgow2





### Quantidade de internações (ghc_qtd_internacao)
qtd_intern_x_sepse <- table(ghc_qtd_internacao, ghc_sepse1e2)
qtd_intern_x_sepse #dividir por grupos

### sexo e idade
freq(sexo)
sexo_sepse <- table(sexo, ghc_sepse1e2)
sexo_sepse

freq(idadeAnos)
idade_sepse <- table(idadeAnos, ghc_sepse1e2)
idade_sepse

### queixa e afeccao presumida ; consciencia? (falta dados); gravidade
queixa_sepse <- table(queixa, ghc_sepse1e2)
queixa_sepse
class(queixa_sepse)
write.csv2(x = queixa_sepse, file = "queixa_sepse2.csv", row.names = T)

afeccoes <- data.frame (afeccaoPresumida, afeccaoComprovada, ghc_sepse1e2)
write.csv2(x = afeccoes, file = "Afecções.csv", row.names = T)

get <- function(as.table(c(a,b,c,d)))
