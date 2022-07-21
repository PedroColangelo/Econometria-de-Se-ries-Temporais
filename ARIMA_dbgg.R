#Importar bibliotecas necessárias 

library(readr)
library(stringr)
library(urca)
library(forecast)
library(rbcb)

#### Coletando dados das séries do Banco Central

dbgg <- ts(get_series(13762, start_date = '2007-01-01', end_date = '2016-04-01')$`13762`,
           start = c(2007, 01), freq = 12)

### Realizar uma análise visual dos dados obtidos

autoplot(dbgg)

### À primeira vista, as suspeitas de que se trata de uma série temporal
### não-estacionária se fortalecem. Para verificar, vamos utilizar o 
### teste de Dickey-Fuller Aumentado para tal. 

### Determinação do lag correto para o teste ADF através do teste de
### autocorrelação serial de Ljung-Box

adf.t <- ur.df(dbgg, type = 'trend', lags = 1)
Box.test(adf.t@res, lag = 1, type = 'Ljung-Box', fitdf = 0)

### Como não há autocorrelação em primeira diferença, podemos conduzir o
### o teste ADF com a primeira defasagem

summary(adf.t)

### Ao analisarmos a estatística tau3, não podemos rejeitar a hipótese
### nula de que a série é não estacionária. Em seguida, prosseguimos logo
### para a estatística phi3, que é responsável por testar se a série
### não possui tendência dado que a série é não estacionária. Como o test-
### statistic dela não consegue ser superior nem ao valor crítico para 
### uma significância em 10%, não conseguimos rejeitar a hipótese nula
### de que os coeficientes "Beta" e "Pi" são 0.

### Devemos prosseguir para um teste ADF apenas com drift.

adf.t <- ur.df(dbgg, type = 'drift', lags = 1)
summary(adf.t)

### Novamente, as duas estatísticas de interesse (tau2 e phi1) não se
### mostram significativas nem quando consideramos uma significância de
### 10%. No caso, tau2 continua testando a hipótese nula de que a série
### é não estacionária e phi1 testa a hipótese nula da série não possuir
### drift dado que ela é não estacionária. Vamos para o terceiro e último
### teste ADF, o teste ADF sem drift e sem trend.

adf.t <- ur.df(dbgg, type = 'none', lags = 1)
summary(adf.t)

### Após este último teste, finalizamos a análise de estacionariedade
### (ou de forma equivalente, de ausência de estacionariedade) via teste
### de Dickey-Fuller Aumentado. Desta vez, há apenas uma estatística, tau1,
### e ela testa a hipótese nula de que se trata de uma série temporal
### não estacionária. Como falhamos em rejeitar esta hipótese nula, 
### confirmamos que se trata de uma série não estacionária, sem drift
### e sem tendência.

delta_dbgg <- diff(dbgg)

autoplot(delta_dbgg)

adf2.t <- ur.df(delta_dbgg, type = 'trend', lags = 1)
Box.test(adf2.t@res, lag = 1, type = 'Ljung-Box', fitdf = 0)

summary(adf2.t)

### Temos uma série estacionária. Iremos utilizar a função auto.arima()
### com d = 1 para determinar a ordem dos componentes autorregresivo 
### e de média móvel da nossa série temporal

arima_dbgg <- auto.arima(dbgg, d = 1, trace = TRUE)

summary(arima_dbgg)

### Vamos usar a função checkresiduals() para verificar se a hipótese de norma-
### lidade de resíduos se mantém, assim como verificar se ainda persis-
### te alguma forma de autocorrelação em nossa série de tempo através
### da função de autocorrelação e função densidade probabilidade dos
### resíduos. Caso nossos resíduos possuam uma distribuição que foge 
### muito da distribuição normal usual/ainda exista persistência de auto-
### correlação residual segundo o ACF, estes são indícios de que 
### nossa estimação do modelo necessita de revisões.

checkresiduals(arima_dbgg)

### Os resíduos do nosso modelo ARIMA mantêm as propriedades desejadas
### por nós. Vamos prever os 12 meses seguintes, assim como
### comparar o modelo fitted aos dados reais que possuímos do período:

forecast_dbgg <- forecast(arima_dbgg, h = 12)

autoplot(forecast_dbgg, ylab = '')

autoplot(fitted(arima_dbgg)) +
  autolayer(dbgg, series = 'dbgg real')