#Regressão Linear Multipla

rm(list=ls()) #limpa a memoria do R


#Instalando e importando bibliotecas

if (! require (pacman)) install.packages("pacman")
library(pacman)
pacman:: p_load(GGally, lmtest, plotly)


#Plotando um gráfico de dispersão usando ggplot
ggplot(data = dados, aes(y= lucro_por_acao, x=roa)) + geom_point()

#Adicionando uma reta de regressao com o argumento geom_smooth
gg  = ggplot(data = dados, aes(y= lucro_por_acao, x=roa)) + geom_point()
gg + geom_smooth(method = 'lm')

# Plota um gráfico de dispersão'dinamico'
ggplotly(gg + geom_smooth(method = 'lm')) 


#Correlação entre variáveis
cor(dados$lucro_por_acao, dados$ebitda)
cor(dados$lucro_por_acao, dados$div_tot_liq)
cor(dados$lucro_por_acao, dados$liq_geral)
cor(dados$lucro_por_acao, dados$marg_liq)
cor(dados$lucro_por_acao, dados$roa)
cor(dados)


#Gráfico de calor da correlação 
ggcorr(dados, label=T, label_round = 3)


# Plotando gráfico 3D entre lucro, roa e margem
plot_ly(dados, z= ~lucro_por_acao,
        x = ~roa,
        y = ~marg_liq) %>% add_markers()


# Gerando histograma de cada variavel
par(mfrow = c(2,3))
hist(dados$lucro_por_acao)
hist(dados$ebitda)
hist(dados$div_tot_liq)
hist(dados$liq_geral)
hist(dados$marg_liq)
hist(dados$roa)



# Ajustando o modelo de regressão linear multipla
modelo = lm(lucro_por_acao ~ ebitda + div_tot_liq +liq_geral + marg_liq + roa, data = dados)
summary(modelo)



# Criando um novo modelo
modelo2 = lm(lucro_por_acao ~ marg_liq + roa, data = dados)
summary(modelo2)


# Testando pressupostos
par(mfrow = c(2,2))
plot(modelo2)


# Teste de normalidade de Shapiro
shapiro.test(modelo2$residuals)

# Teste de outliers
summary(rstandard(modelo2))


#Teste de homocedasticidade
bptest(modelo2)
