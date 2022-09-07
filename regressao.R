rm(list=ls()) #limpa a memoria do R


#Instalando e importando bibliotecas

if (! require (pacman)) install.packages("pacman")
library(pacman)
pacman:: p_load(GGally, lmtest, robustX, sandwich)


# Descrevendo os dados
summary(dados)

#Gráfico de calor da correlação 
ggcorr(dados, label=T, label_round = 3)


#------------------------------------------------

# Verificando e corrigindo outliers

par(mfrow = c(1,3))
boxplot(dados$liq_cor, outline = T)
boxplot(dados$roa, outline = T)
boxplot(dados$marg_liq, outline = T)

# Corrigindo outliers
install.packages('robustX')
library(robustX)

# Filtra os dados das colunas especificadas para retornar false para dados considerados outliers
outliers = robustX::mvBACON(dados[c("roa", "marg_liq")], alpha = 0.95, init.sel = "Mahalanobis")

# Adiciona uma coluna chamada outliers com true ( não outlier) e false ( outlier)
dados_out = within(dados, {outliers = outliers$subset})

# Identificando quantidade de observações 
table(dados_out$outliers)

# Excluindo outliers
dados_out = subset(dados_out, outliers == TRUE)
dados_out$outliers = NULL
remove(outliers)

# Comparando dados
par(mfrow = c(2,2))
boxplot(dados$roa, outline = T)
boxplot(dados$marg_liq, outline = T)
boxplot(dados_out$roa)
boxplot(dados_out$marg_liq, outline = T)


#------------------------------------------------

# Gerando histograma de cada variavel
par(mfrow = c(1,3))
hist(dados_out$roa)
hist(dados_out$marg_liq)
hist(dados_out$liq_cor)

shapiro.test(dados_out$roa)
shapiro.test(dados_out$marg_liq)
shapiro.test(dados_out$liq_cor)

#-------------------------------------------------

# Criando modelo
modelo = lm(roa ~ marg_liq + liq_cor, data = dados_out)
summary(modelo)

# Testando pressupostos
par(mfrow = c(2,2))
plot(modelo)

# Teste de normalidade de Shapiro
shapiro.test(modelo$residuals)

# Testando outliers 
summary(rstandard(modelo))
par(mfrow = c(1,1))
boxplot(modelo$residuals, outline = T)

# Teste de multicolinearidade 
vif(modelo)

#Teste de homocedasticidade
bptest(modelo)

#Criando modelo com controle para heterocedasticidade
library(sandwich)

dados_het = coeftest(modelo, vcov. = vcovHC, type = "HC1")
dados_het
