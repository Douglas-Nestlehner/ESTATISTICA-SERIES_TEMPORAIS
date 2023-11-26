# Analsie - Preco do Ouro
####


df = read.csv2("", sep = ",")

EUA = df$United.States.USD.
JPN = df$Japan.JPY.
EU = df$Europe.EUR.
CHINA = df$China.CNY.
CHINA


x = ts(EUA, start = c(1979,1), frequency =  12)
y = ts(JPN, start = c(1979,1), frequency =  12)

ts.plot(x)
ts.plot(y)

#### Analise descritiva 


plot(decompose(x, type = "additive"))
plot(decompose(x, type = c("multiplicative")))


#################################################3


library(readxl)
library("seasonal")
library(forecast)
library(aTSA)
library(TSA)
library(randtests)
ouro <- read_excel("C:/Users/User/Downloads/ouro.xlsx")
View(ouro)

## salvar os dados como dataframe
dados_ouro = as.data.frame(ouro)

## criacao da serie
serie = ts(dados_ouro$`United States(USD)`, start = c(1979,1),
           end = c(2021,7), frequency = 12)


summary(serie)
var(serie)


## Histograma dos valores
hist(serie, xlab = "Valores do preço do ouro ",
     ylab = "Frequência", col = "pink",
     main ="Histograma do preço do ouro nos EUA")
abline(v = c(mean(serie), median(serie)),
       col = c("green", "red"),
       lwd = c(2,2,4),
       lty=c(1,2,3));

# acrescentando quadro com legenda
legend(x="topright", #posicao da legenda
       c("Media","Mediana"), #nomes da legenda
       col=c("green","red"), #cores
       lty=c(1,2,3),
       lwd=c(2,2,4))

## plot da serie temporal
ts.plot(serie, xlab = "tempo",
        ylab ="Valores Série Temporal 744860",
        main = "Série Temporal 744860", 
        col = "black")


## decomposicao da serie
plot(decompose(serie))

## aACF para desenvolvimento do modelo
par(mfrow=c(1,2))
acf(serie, main='ACF da série ')
pacf(serie, main='PACF da série ')

## testes de tendencia e homocedasticidade
cox.stuart.test(serie) 

adf.test(serie)



par(mfrow=c(1,1))

## analise da variabilidade da serie
med.var<-function(x,k)
{N<-length(x)
x.m<-rep(0,(N-k))
x.r<-rep(0,(N-k))
for (i in 1:(N-k)) x.m[i]<-mean(x[i:(i+k)])
for (i in 1:(N-k)) x.r[i]<-max(x[i:(i+k)])-min(x[i:(i+k)])
plot(x.m,x.r,xlab="medias",ylab="amplitude")
aa1<-lm(x.r~x.m)
abline(aa1$coef[1],aa1$coef[2],col=2)
}

# media das vaiancias
med.var(serie, 12)

# grafico da diferenca
plot.ts(diff(serie),
        main = " Série temporal diferenciada",
        col='darkblue')

## ACF das diferenciacoes
par(mfrow=c(1,2))
acf(diff(serie), main='ACF da Série diferenciada')
pacf(diff(serie), main='PACF da Série diferenciada')


## medias moveis
par(mfrow=c(1,1))
med.moveis<-function(x,k)
{x<-as.vector(x)
N<-length(x)
xstar<-rep(0,N)
for (i in 1:k) xstar[i]<-NA
for (i in (1+k):(N-k)) xstar[i]<-mean(x[(i-k):(i+k)],na.rm=TRUE)
for (i in (N-k+1):N) xstar[i]<-NA
ts.plot(x)
lines(xstar, col=2)
xstar}

med.moveis(serie, 3)

med.moveis(serie, 5)


## ajuste do modelo arima 
modelo_autoarima = auto.arima(serie)

modelo_autoarima

tsdiag(modelo_autoarima)

res <- residuals(modelo_autoarima)

plot(res, main="Residuos do Metodo Auto Arima",
     ylab="", xlab="Periodo")
grid()
abline(h=0)

Acf(res, main="ACF dos residuos")

hist(res, main="Histograma dos residuos")

summary(res)


## deteccao de pontos influentes
detectIO(modelo_autoarima)


## modelo para previsao
modelo_final = arima(serie, order = c(2,1,2), seasonal= c(2,0,0))

## predicao
p = predict(modelo3, n.ahead = 48)

## grafico da predicao
plot.ts(serie, xlim = c(1979,2025), 
        ylim = c(1800,22000), xlab="Tempo",
        ylab="Preço do ouro nos EUA", 
        main="Previsão do preço do ouro nos EUA")
lines(p$pred,col="red")
abline(v = c(2022),
       col = c("Blue"),
       lwd = c(2,2,4),
       lty=c(1,2,3))


#### Impacto do 11 de Setembro no preço do ouros nos EUA ####

library(CausalImpact)
# inicio da serie: janeiro de 1979
## data da intervenção: 11 de setembro de 2001

time.points = seq.Date(as.Date("1979-01-31"),by=30,length.out=511)


data_intervention = zoo(serie,time.points)
serie
data_intervention
pre.period = as.Date(c("1979-01-31","2001-09-11"))
post.period = as.Date(c("2001-09-12","2020-12-21"))

effect = CausalImpact(data_intervention,pre.period,post.period)

plot(effect)