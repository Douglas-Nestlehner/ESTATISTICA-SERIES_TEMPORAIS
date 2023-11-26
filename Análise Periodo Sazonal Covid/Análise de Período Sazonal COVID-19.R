library(tidyverse)
library(readxl)
library(xts)
library(gridExtra)

set.seed(4)
setwd("")

df = read_excel("dados_estado_SP_2020.xlsx")
head(df)
str(df)

# transformar para o tipo "Time-Series
ts = xts(df$obitosNovos, order.by=as.Date(df$data))
plot.ts(ts)


# estabilizar a variancia da série
ts_vlog = log(ts) # -inf
ts_v = ts^{1/2} # ok, fazer ts^{1/4 ou mais} estabiliza mais a variancia  
ts_v3 = ts^{1/3}
ts.plot(ts_v, ylab = "")

?plot()
# verificar tendencia
ts_v = ts_v
TT = length(ts_v)
tempo = seq(1,TT)


m1 = lsfit(tempo,ts_v)
ls.print(m1)


ts.plot(ts_v)
abline(coef=m1$coef,col=2)


ts.plot(m1$residuals) # s?rie sem tendencia quadratica 
ts2 = m1$residuals

par(mfrow=c(2,2), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
ts.plot(ts,ylab= "serie")
ts.plot(ts_v1, ylab= "serie quadrática")
ts.plot(ts_v3, ylab= "serie cúbica")
ts.plot(ts_vlog, ylab= "serie log")

#cox:
#cs.test(ts)

# Teste de fisher
#Sazonalidade

#Fun??o para ajustar um polinomio a s?rie
polin<-function(x,p){
  x<-as.vector(x)
  N<-length(x)
  XX<-matrix(0,N,p)
  for (i in 1:N) {
    for (j in 1:p) XX[i,j]<-i**j}
  aa<-lm(x~XX)
  ts.plot(x)
  lines(aa$fitted,col=2)
  aa$res}

tsF = diff(ts_v)
x = polin(tsF,3)

#Teste de Fisher para verificar Sazonalidade

n<-length(x)
I=abs(fft(x)/sqrt(n))^2 
P=I*4/n 
ts.plot(P) 
freq=(0:(n/2))/n 
plot(freq,P[1:(n/2+1)], type="o", xlab="Frequencia",ylab="Periodograma Escalado")
Periodograma<-P[1:(n/2+1)]
g<-max(Periodograma)/sum(Periodograma)
g
alfa<-n*(1-g)^(n-1)  
alfa #muito pequeno, logo rejeitamos a H_0 e conclu?mos que a s?rie possui sazonalidade.
e1<-which.max(Periodograma) # qual o ponto maximo (do grafico)
1/freq[e1] # frequencia dada