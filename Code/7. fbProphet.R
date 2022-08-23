################################################### FB PROPHET FORECAST ###################################################

## installazione pacchetti e caricamento librerie

#install.packages("forecast")
#install.packages("lubridate")
#install.packages("tseries")
#install.packages("zoo")
#install.packages('prophet')
#install.packages('MLmetrics')
#install.packages('gridExtra')
library("forecast")
library("lubridate")
library("tseries")
library('ggplot2')
library("zoo")
library("readr")
library('prophet')
library('MLmetrics')
library('gridExtra')


## impostazione work directory
setwd('F:/Data Science LAB/dataset ristoranti')

## Caricamento e visualizzazione Datebase Ristoranti prima della pandemia
Ristoranti <- read.csv('dataset finali usati per il progetto/df_finale_NO_COVID.csv', sep = ',', header = TRUE)
View(Ristoranti)


## Conversione da formato character a formato date per la colonna row.ID
Ristoranti$row.ID <- ymd(Ristoranti$row.ID)
wday(Ristoranti$row.ID)
leap_year(2017:2020)

class(Ristoranti$row.ID)

## Riassunto indici statistici
summary(Ristoranti)


################################### fb Prophet Ristorante 1 ###################################

## Creazione e visualizzazione dataframe
# sono necessarie due sole colonne: ds(date) e y(valori)
ds <- Ristoranti$row.ID
y <- Ristoranti$Vendite.R1
df1 <- data.frame(ds, y)
View(df1)

## Visualizzazione qplot delle vendite
qplot(ds, y, data = df1)

## Rimozione outliers
outliers1 <- (df1$y < 2000
             | df1$y > 14000)
df1$y[outliers1] <- NA

## Fitting del modello
# Aggiungo la collezione built-in per il paese Italia
model1 <- prophet(holidays = )
model1 <- add_country_holidays(model1, country_name = 'IT')
model1 <- fit.prophet(model1, df1)

## Previsione
future1 <- make_future_dataframe(model1, periods = 90, freq = 'D') # prende in input il modello e un numero di periodi da prevedere
tail(future1) # il risultato è un dataframe con una singola colonna (ds) che rappresenta le date passate e quelle future

forecast1 <- predict(model1, future1) # funzione generale predict per prevedere i valori delle vendite future nel periodo interessato
tail(forecast1[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # visulizzo le date, i valori previsti, lower e upper bound

## Plot forecast
plot(model1, forecast1) + add_changepoints_to_plot(model1)
prophet_plot_components(model1, forecast1)

dyplot.prophet(model1, forecast1)

## Creazione di un dataframe con le sole colonne interessate del dataframe forecast (per grafici)
forecastR1 <- data.frame(forecast1$ds, forecast1$yhat, forecast1$yhat_lower, forecast1$yhat_upper)
colnames(forecastR1) <- c('ds', 'Forecast', 'Forecast_Lower_bound', 'Forecast_Upper_bound')

forecastR1$ds <- ymd(forecastR1$ds)

forecastR1 <- forecastR1[826:916, ] # prendo solo le righe della previsione (da 2020/01/04 a 2020/04/03)

forecast1$ds <- ymd(forecast1$ds)

## Plot serie temporale Vendite insieme alla previsione con fb Prophet
plot_forecastR1 <- ggplot() +
  geom_line(data = forecast1, mapping = aes(x = ds, y = trend), color='red', alpha=1, size=0.6)+
  geom_line(data = df1, mapping = aes(x = ds, y = y, color = 'Sales',), alpha=.7)+
  geom_line(data = forecastR1, mapping = aes(x = ds, y = Forecast, color = 'fbProphet Forecast',), alpha=.7)+
  labs(x='', y='Vendite', title = 'PREVISIONE VENDITE R1')+
  scale_color_manual(values = c('Sales'='blue', 'fbProphet Forecast'='orange'))+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))

plot_forecastR1

plot_forecastR1+
  scale_x_date(limits = as.Date(c('2019-11-01', '2020-04-03')))

## Plot serie temporale Vendite insieme ad upper e lower bound della previsione
plot_forecastboundsR1 <- ggplot() +
  geom_line(data = df1, mapping = aes(x = ds, y = y, color = 'Sales',), alpha=.7, )+
  geom_line(data = forecastR1, mapping = aes(x = ds, y = Forecast_Lower_bound, color = 'fbProphet Lower bound',), alpha=.7, )+
  geom_line(data = forecastR1, mapping = aes(x = ds, y = Forecast_Upper_bound, color = 'fbProphet Upper bound',), alpha=.7, )+
  labs(x='', y='Vendite')+
  scale_color_manual(values = c('Sales'='blue', 'fbProphet Lower bound'='red', 'fbProphet Upper bound'='green'))+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))

plot_forecastboundsR1

plot_forecastboundsR1+
  scale_x_date(limits = as.Date(c('2019-11-01', '2020-04-03')))

grid.arrange(plot_forecastR1, plot_forecastboundsR1, nrow = 2)

## Valutazione modello
df1train <- df1[1:651, ]
df1test <- df1[652:826, ]

# fitting modello su train set
mod_evaluation_1 <- prophet(df1train)
forecast_evaluation_1 <- make_future_dataframe(mod_evaluation_1, periods = 175, freq = 'D')
forecast_evaluation_1 <- predict(mod_evaluation_1, forecast_evaluation_1)

forecast_evaluation_1 <- forecast_evaluation_1[652:826, ]

# confronto tra previsti e valori reali (test set)
accuracy(forecast_evaluation_1$yhat, df1test$y)


################################### fb Prophet Ristorante 2 ###################################

## Creazione e visualizzazione dataframe
# sono necessarie due sole colonne: ds(date) e y(valori)
ds <- Ristoranti$row.ID
y <- Ristoranti$Vendite.R2
df2 <- data.frame(ds, y)
View(df2)

## Visualizzazione qplot delle vendite
qplot(ds, y, data = df2)

## Rimozione outliers
outliers2 <- (df2$y < 5000
             | df2$y > 16000)
df2$y[outliers2] <- NA

## Fitting del modello
# Aggiungo la collezione built-in per il paese Italia
model2 <- prophet(holidays = ) # 'mcmc.samples' per ottenere incertezza sulla stagionalità, è necessario eseguire un campionamento bayesiano completo
model2 <- add_country_holidays(model2, country_name = 'IT')
model2 <- fit.prophet(model2, df2)

## Previsione
future2 <- make_future_dataframe(model2, periods = 90, freq = 'D') # prende in input il modello e un numero di periodi da prevedere
tail(future2) # il risultato è un dataframe con una singola colonna (ds) che rappresenta le date passate e quelle future

forecast2 <- predict(model2, future2) # funzione generale predict per prevedere i valori delle vendite future nel periodo interessato
tail(forecast2[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # visulizzo le date, i valori previsti, lower e upper bound

## Plot forecast
plot(model2, forecast2) + add_changepoints_to_plot(model2)
prophet_plot_components(model2, forecast2)

dyplot.prophet(model2, forecast2)

## Creazione di un dataframe con le solo colonne interessate del dataframe forecast (per grafici)
forecastR2 <- data.frame(forecast2$ds, forecast2$yhat, forecast2$yhat_lower, forecast2$yhat_upper)
colnames(forecastR2) <- c('ds', 'Forecast', 'Forecast_Lower_bound', 'Forecast_Upper_bound')

forecastR2$ds <- ymd(forecastR2$ds)

forecastR2 <- forecastR2[826:916, ] # prendo solo le righe della previsione (da 2020/01/04 a 2020/04/03)

forecast2$ds <- ymd(forecast2$ds)

## Plot serie temporale Vendite insieme alla previsione con fb Prophet
plot_forecastR2 <- ggplot() +
  geom_line(data = forecast2, mapping = aes(x = ds, y = trend), color='red', alpha=1, size=0.6)+
  geom_line(data = df2, mapping = aes(x = ds, y = y, color = 'Sales',), alpha=.7)+
  geom_line(data = forecastR2, mapping = aes(x = ds, y = Forecast, color = 'fbProphet Forecast',), alpha=.7)+
  labs(x='', y='Vendite', title = 'PREVISIONE VENDITE R2')+
  scale_color_manual(values = c('Sales'='blue', 'fbProphet Forecast'='orange'))+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))

plot_forecastR2

plot_forecastR2+
  scale_x_date(limits = as.Date(c('2019-11-01', '2020-04-03')))

## Plot serie temporale Vendite insieme ad upper e lower bound della previsione
plot_forecastboundsR2 <- ggplot() +
  geom_line(data = df2, mapping = aes(x = ds, y = y, color = 'Sales',), alpha=.7)+
  geom_line(data = forecastR2, mapping = aes(x = ds, y = Forecast_Lower_bound, color = 'fbProphet Lower bound',), alpha=.7)+
  geom_line(data = forecastR2, mapping = aes(x = ds, y = Forecast_Upper_bound, color = 'fbProphet Upper bound',), alpha=.7)+
  labs(x='', y='Vendite')+
  scale_color_manual(values = c('Sales'='blue', 'fbProphet Lower bound'='red', 'fbProphet Upper bound'='green'))+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))

plot_forecastboundsR2

plot_forecastboundsR2+
  scale_x_date(limits = as.Date(c('2019-11-01', '2020-04-03')))

grid.arrange(plot_forecastR2, plot_forecastboundsR2, nrow = 2)

## Valutazione modello 
df2train <- df2[1:651, ]
df2test <- df2[652:826, ]

# fitting modello su train set
mod_evaluation_2 <- prophet(df2train)
forecast_evaluation_2 <- make_future_dataframe(mod_evaluation_2, periods = 175, freq = 'D')
forecast_evaluation_2 <- predict(mod_evaluation_2, forecast_evaluation_2)

forecast_evaluation_2 <- forecast_evaluation_2[652:826, ]

# confronto tra previsti e valori reali (test set)
accuracy(forecast_evaluation_2$yhat, df2test$y)


################################### fb Prophet Ristorante 6 ###################################

## Creazione e visualizzazione dataframe
# sono necessarie due sole colonne: ds(date) e y(valori)
ds <- Ristoranti$row.ID
y <- Ristoranti$Vendite.R6
df6 <- data.frame(ds, y)
View(df6)

## Visualizzazione qplot delle vendite
qplot(ds, y, data = df6)

## Rimozione outliers
outliers6 <- (df6$y < 2500)
df6$y[outliers6] <- NA

## Fitting del modello
# Aggiungo la collezione built-in per il paese Italia
model6 <- prophet(holidays = ) # 'mcmc.samples' per ottenere incertezza sulla stagionalità, è necessario eseguire un campionamento bayesiano completo
model6 <- add_country_holidays(model6, country_name = 'IT')
model6 <- fit.prophet(model6, df6)

## Previsione
future6 <- make_future_dataframe(model6, periods = 90, freq = 'D') # prende in input il modello e un numero di periodi da prevedere
tail(future6) # il risultato è un dataframe con una singola colonna (ds) che rappresenta le date passate e quelle future

forecast6 <- predict(model6, future6) # funzione generale predict per prevedere i valori delle vendite future nel periodo interessato
tail(forecast6[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')]) # visulizzo le date, i valori previsti, lower e upper bound

## Plot forecast
plot(model6, forecast6) + add_changepoints_to_plot(model6)
prophet_plot_components(model6, forecast6)

dyplot.prophet(model6, forecast6)

## Creazione di un dataframe con le solo colonne interessate del dataframe forecast (per grafici)
forecastR6 <- data.frame(forecast6$ds, forecast6$yhat, forecast6$yhat_lower, forecast6$yhat_upper)
colnames(forecastR6) <- c('ds', 'Forecast', 'Forecast_Lower_bound', 'Forecast_Upper_bound')

forecastR6$ds <- ymd(forecastR6$ds)

forecastR6 <- forecastR6[826:916, ] # prendo solo le righe della previsione (da 2020/01/04 a 2020/04/03)

forecast6$ds <- ymd(forecast6$ds)

## Plot serie temporale Vendite insieme alla previsione con fb Prophet
plot_forecastR6 <- ggplot() +
  geom_line(data = forecast6, mapping = aes(x = ds, y = trend), color='red', alpha=1, size=0.6)+
  geom_line(data = df6, mapping = aes(x = ds, y = y, color = 'Sales',), alpha=.7)+
  geom_line(data = forecastR6, mapping = aes(x = ds, y = Forecast, color = 'fbProphet Forecast',), alpha=.7)+
  labs(x='', y='Vendite', title = 'PREVISIONE VENDITE R6')+
  scale_color_manual(values = c('Sales'='blue', 'fbProphet Forecast'='orange'))+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))

plot_forecastR6

plot_forecastR6+
  scale_x_date(limits = as.Date(c('2019-11-01', '2020-04-03')))

## Plot serie temporale Vendite insieme ad upper e lower bound della previsione
plot_forecastboundsR6 <- ggplot() +
  geom_line(data = df6, mapping = aes(x = ds, y = y, color = 'Sales',), alpha=.7)+
  geom_line(data = forecastR6, mapping = aes(x = ds, y = Forecast_Lower_bound, color = 'fbProphet Lower bound',), alpha=.7)+
  geom_line(data = forecastR6, mapping = aes(x = ds, y = Forecast_Upper_bound, color = 'fbProphet Upper bound',), alpha=.7)+
  labs(x='', y='Vendite')+
  scale_color_manual(values = c('Sales'='blue', 'fbProphet Lower bound'='red', 'fbProphet Upper bound'='green'))+
  theme(legend.position = 'bottom', plot.title = element_text(hjust = 0.5))

plot_forecastboundsR6

plot_forecastboundsR6+
  scale_x_date(limits = as.Date(c('2019-11-01', '2020-04-03')))

grid.arrange(plot_forecastR6, plot_forecastboundsR6, nrow = 2)

## Valutazione modello 
df6train <- df6[1:651, ]
df6test <- df6[652:826, ]

# fitting modello su train set
mod_evaluation_6 <- prophet(df6train)
forecast_evaluation_6 <- make_future_dataframe(mod_evaluation_6, periods = 175, freq = 'D')
forecast_evaluation_6 <- predict(mod_evaluation_6, forecast_evaluation_6)

forecast_evaluation_6 <- forecast_evaluation_6[652:826, ]

# confronto tra previsti e valori reali (test set)
accuracy(forecast_evaluation_6$yhat, df6test$y)

