#Import librerie e lettura dati
library("forecast")
library("lubridate")
library("tseries")
library("zoo")
library("ggplot2")
library("tsoutliers")

library(readr)
NO_COVID <- read_csv("C:/Users/danie/OneDrive/Desktop/DSLAB/Dati/3. dataset finali usati per il progetto_dal clustering/df_finale_NO_COVID.csv")

#Explorative analysis per R1
##Costruzione serie storica e decompose



my_ts_R1 <- ts(NO_COVID$`Vendite R1`,start=1,frequency=7)
plot(my_ts_R1)


##Visualizzazione decomposizione serie storica

plot(decompose(my_ts_R1,type="additive"))



## Verifica stazionarietà per R1

library("urca")
adf.test(my_ts_R1)

#Explorative analysis per R2
##Costruzione serie storica e decompose

my_ts_R2 <- ts(NO_COVID$`Vendite R2`,start=1,frequency=7)
plot(my_ts_R2)

##Visualizzazione decomposizione serie storica

plot(decompose(my_ts_R2,type="additive"))

## Verifica stazionarietà per R2

library("urca")
adf.test(my_ts_R2)


#Explorative analysis per R6
##Costruzione serie storica e decompose

my_ts_R6 <- ts(NO_COVID$`Vendite R6`,start=1,frequency=7)
plot(my_ts_R6)

##Visualizzazione decomposizione serie storica
plot(decompose(my_ts_R6,type="additive"))


## Verifica stazionarietà per R6
library("urca")
adf.test(my_ts_R6)


