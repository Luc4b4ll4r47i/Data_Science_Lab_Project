library("forecast")
library("lubridate")
library("tseries")
library("zoo")
library("ggplot2")
library("tsoutliers")

library(readr)
SI_COVID <- read_csv("C:/Users/danie/OneDrive/Desktop/DSLAB/Dati/3. dataset finali usati per il progetto_dal clustering/df_finale_SI_COVID.csv")


#serie storica originale R1
my_ts_R1_si_covid <- ts(SI_COVID$`Vendite R1`,start=1,frequency=7)
plot(my_ts_R1_si_covid)


#serie storica originale R2
my_ts_R2_si_covid <- ts(SI_COVID$`Vendite R2`,start=1,frequency=7)
plot(my_ts_R2_si_covid)


#serie storica originale R6
my_ts_R6_si_covid <- ts(SI_COVID$`Vendite R6`,start=1,frequency=7)
plot(my_ts_R6_si_covid)

