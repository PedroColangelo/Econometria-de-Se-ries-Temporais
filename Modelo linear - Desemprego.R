library(sidrar)
library(readr)
library(caret)
library(forecast)
library(xtable)
library(tidyverse)
library(scales)
library(gganimate)
library(ggplot2)
library(zoo)
library(lmtest)

#Importar os dados necessários para a estimação via regressão linear

xreg <- read_csv2('xreg.csv')

xreg <- ts(xreg[,-1], start = c(2012, 3), frequency = 12)

#Importar dados da PNAD

table = get_sidra(api='/t/6318/n1/all/v/1641/p/all/c629/all')

#Obter a População Economicamente Ativa e a População Desocupada. Vamos utilizar as duas para obtermos a taxa de desemprego.

pea <- table$Valor[table$`Condição em relação à força de trabalho e condição de ocupação (Código)`==32386]

desocupada <- table$Valor[table$`Condição em relação à força de trabalho e condição de ocupação (Código)`==32446]

desemprego <- ts(desocupada/pea * 100, start = c(2012, 3), 
                 frequency = 12)

#Inserir os dados ao dataframe original

data <- ts.intersect(desemprego, xreg)

#Renomear as colunas originárias do dataframe original

colnames(data) <- c("desemprego", "icd", "iaemp", "iie",
                    "google", "ibc", "selic")

#Verificar a existência de missing values em todo o dataframe

apply(is.na(data), 2, which)

#Há um missing value na coluna do índice IBC-Br

#Completar o missing value achado através de uma previsão via auto.arima()

ibc.forecast <- forecast::forecast(auto.arima(data[,6],
                                              max.p = 4,
                                              max.q = 4,
                                              seasonal = F),
                                   h = 1,
                                   level = 40)$upper

data[93, 6] <- ibc.forecast

tail(data)

#Plot da série de tempo de desemprego

autoplot(desemprego)

#Realizar a previsão dentro da amostra

intrain <- createDataPartition(data[,1], p = 0.7, list = FALSE)

#Achei preferível manter o mesmo seed para se tornar mais facilmente replicável 

set.seed(2017)

#Realizar a separação propriamente dita em amostra de treino e amostra de teste

train <- as.data.frame(data[intrain,])

test <- as.data.frame(data[-intrain,])

#Preparar o modelo para ser empregado sobre os dados de treino

lm = lm(desemprego ~ ., train)

summary(lm)

#Avaliar se há algum grau de
#heteroscedasticidade ou de autocorrelação
#entre os resíduos

plot(lm$residuals)

#Avaliar se há algum grau de
#heteroscedasticidade ou de autocorrelação
#entre os resíduos

##Avaliar normalidade

shapiro.test(lm$residuals)

##Avaliar heteroscedasticidade

bptest(lm)

##Avaliar autocorrelação 

Box.test(lm$residuals)

dwtest(lm)

#De fato, há confirmações de que os resíduos
#não obedecem os critérios de homoscedasticidade
#e ausência de autocorrelação. Sugere-se a me-
#lhoria do modelo ao incorporar elementos de
#modelos da família VAR e análise de cointegra-
#ção entre cada uma das séries de tempo das
#variáveis

#Realizar a previsão dentro da amostra

lm_forecast <- forecast::forecast(lm, 
                                  newdata = test[,-1],
                                  level = 95)

#Avaliar a performance da previsão contrastando os resultados com os originais

acc1 <- accuracy(lm_forecast$mean, test[,1])

print(xtable(acc1), comment = FALSE)

#Plotagem da série de desemprego real contra a série prevista pelo modelo

desemprego_prev <- data[,1]

desemprego_prev[-intrain] <- lm_forecast$mean

desemprego_prev_up <- data[,1]

desemprego_prev_up[-intrain] <- lm_forecast$upper

desemprego_prev_low <- data[,1]

desemprego_prev_low[-intrain] <- lm_forecast$lower

desemprego_data <- data.frame(unlist(data[,1]),
                              unlist(desemprego_prev),
                              unlist(desemprego_prev_up),
                              unlist(desemprego_prev_low)) 

colnames(desemprego_data) <- c('desemprego', 
                               'desemprego_prev',
                               'desemprego_prev_up',
                               'desemprego_prev_low')

dates <- as.yearmon(time(data))

desemprego_data['Dates'] <- dates

p <- ggplot(data = desemprego_data, aes(x = Dates,
                                        y = desemprego)) +
  geom_line(aes(color = 'Desemprego Efetivo'),
            size = 1) +
  geom_line(aes(y = desemprego_prev,
                color = 'Desemprego Previsto'),
            size = 1) +
  geom_ribbon(aes(x = Dates, ymax = desemprego_prev_up,
                  ymin = desemprego_prev_low), 
              fill = 'pink',
              alpha = .5) +
  theme_classic() +
  labs(title = 'Taxa de desemprego efetiva contra taxa de desemprego prevista pelo modelo',
       subtitle = 'Intervalo de confiança de 95% para pontos de teste',
       y = 'Desemprego (%)',
       x =  'Tempo') +
  theme(axis.title = element_text(face = 'bold')) +
  theme(plot.title = element_text(face = 'bold')) +
  scale_color_manual(values = c('royalblue1',
                                            'indianred1')) +
                                              theme(legend.title = element_blank()) +
  theme(legend.text = element_text(face = 'bold'))

#Animação do gráfico

p <- p + transition_reveal(as.Date(desemprego_data$Dates))

p
