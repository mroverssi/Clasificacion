setwd("C:/Users/juanp/OneDrive/MIAD/03 Modelos analisis estadistico/Datos Marketing Bancario")

library(datasets)
library(ggplot2)
library(ggpubr)
library(klaR)
library(pROC)
library(MVN)
library(caret)
library(faraway)
library(readxl)
library(GGally)
library(tidyverse)


#Cargar datos de entrenamiento
datos <- read_excel("Train bank.xlsx")
datos$Subscription <- as.factor(datos$Subscription)
colnames(datos)

#Cargar datos de prueba
datos_test <- read_excel("Test bank.xlsx")


## ------------------------------------------------------------------------------------------------------------

# Gráficos para análisis

#datos_si <- subset(datos, datos$Subscription == 1)
#datos_no <- subset(datos, datos$Subscription == 0)

#my_colors <- c("red","blue")
#pairs(datos[c(2,7,13,15,16)])
#pairs(datos_si[c(2,7,13,15,16)], col = "blue")
#pairs(datos_no[c(2,7,13,15,16)], col = "red")


ggplot(datos,aes(x=Age, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Job, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Marital.Status, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Education, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Credit, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Housing.Loan, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Personal.Loan, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Contact, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Last.Contact.Day, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Last.Contact.Month, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Last.Contact.Duration, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Campaign, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Poutcome, fill = Subscription))+geom_bar()
ggplot(datos,aes(x=Subscription, fill = Subscription))+geom_bar()


ggplot(datos,aes(x= Subscription,  y= Balance..euros., color=Subscription)) + geom_boxplot()
ggplot(datos,aes(x= Subscription,  y= Pdays, color=Subscription)) + geom_boxplot()
ggplot(datos,aes(x= Subscription,  y= Previous, color=Subscription)) + geom_boxplot()



# Revisando la edad
ggplot(datos,aes(x=Job, y=Age, color=Subscription)) + geom_boxplot()
ggplot(datos,aes(x=Marital.Status, y=Age, color=Subscription)) + geom_boxplot()
ggplot(datos,aes(x=Education, y=Age, color=Subscription)) + geom_boxplot()
ggplot(datos,aes(x=Credit, y=Age, color=Subscription)) + geom_boxplot()
ggplot(datos,aes(x=Housing.Loan, y=Age, color=Subscription)) + geom_boxplot()
ggplot(datos,aes(x=Personal.Loan, y=Age, color=Subscription)) + geom_boxplot()
ggplot(datos,aes(x=Contact, y=Age, color=Subscription)) + geom_boxplot()
ggplot(datos,aes(x=Poutcome, y=Age, color=Subscription)) + geom_boxplot()


# Revisando el saldo medio anual
ggplot(datos, aes(x=Job, y=Balance..euros., color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Marital.Status, y=Balance..euros., color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Education, y=Balance..euros., color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Credit, y=Balance..euros., color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Housing.Loan, y=Balance..euros., color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Personal.Loan, y=Balance..euros., color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Contact, y=Balance..euros., color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Poutcome, y=Balance..euros., color=Subscription)) + geom_boxplot()

# Revisando la duración del último contacto
ggplot(datos, aes(x=Job, y=Last.Contact.Duration, color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Marital.Status, y=Last.Contact.Duration, color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Education, y=Last.Contact.Duration, color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Credit, y=Last.Contact.Duration, color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Housing.Loan, y=Last.Contact.Duration, color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Personal.Loan, y=Last.Contact.Duration, color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Contact, y=Last.Contact.Duration, color=Subscription)) + geom_boxplot()
ggplot(datos, aes(x=Poutcome, y=Last.Contact.Duration, color=Subscription)) + geom_boxplot()


# Revisando datos de contacto
ggpairs(datos, columns = 11:13, aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(14,15,16), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,12,17), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,3,4,5,10), aes(color = Subscription, alpha = 0.5))

# Revisando última duración de contacto
ggpairs(datos, columns = c(13,3), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,4), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,5), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,6), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,8), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,9), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,10), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,12), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,15), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,16), aes(color = Subscription, alpha = 0.5))
ggpairs(datos, columns = c(13,17), aes(color = Subscription, alpha = 0.5))


## --------------------------------------------------------------------------------------------
# Regresión logística / todas las variables

datos_mlog = datos[c(2:18)]
modelo_logistico <- glm(Subscription ~ ., data = datos_mlog, family = "binomial")
summary(modelo_logistico)

roc_modelo <- roc(datos_mlog$Subscription, modelo_logistico$fitted.values,auc=T)
roc_modelo
plot.roc(roc_modelo,print.auc=T,xlab="1-ESpecificidad",ylab="Sensibilidad")


## --------------------------------------------------------------------------------------------
# Regresión logística / variables más significativas del modelo con todas las variables 

datos_mlog = datos[c(3,4,5,8,9,10,12,13,14,17,18)]
modelo_logistico <- glm(Subscription ~ ., data = datos_mlog, family = "binomial")
summary(modelo_logistico)

roc_modelo <- roc(datos_mlog$Subscription, modelo_logistico$fitted.values,auc=T)
roc_modelo
plot.roc(roc_modelo,print.auc=T,xlab="1-ESpecificidad",ylab="Sensibilidad")

## --------------------------------------------------------------------------------------------
# Regresión logística / variables identificadas en análisis gráfico inicial 

datos_mlog = datos[c(3,4,5,6,8,9,10,12,13,15,16,17,18)]
modelo_logistico <- glm(Subscription ~ ., data = datos_mlog, family = "binomial")
summary(modelo_logistico)

roc_modelo <- roc(datos_mlog$Subscription, modelo_logistico$fitted.values,auc=T)
roc_modelo
plot.roc(roc_modelo,print.auc=T,xlab="1-ESpecificidad",ylab="Sensibilidad")

## --------------------------------------------------------------------------------------------
# Regresión logística / variables en común de los dos anteriores

datos_mlog = datos[c(3,4,5,8,9,10,12,13,17,18)]
modelo_logistico <- glm(Subscription ~ ., data = datos_mlog, family = "binomial")
summary(modelo_logistico)

roc_modelo <- roc(datos_mlog$Subscription, modelo_logistico$fitted.values,auc=T)
roc_modelo
plot.roc(roc_modelo,print.auc=T,xlab="1-ESpecificidad",ylab="Sensibilidad")

## --------------------------------------------------------------------------------------------
# Regresión logística / variables básicas para llegar a AUC 0,9

#datos_mlog = datos[c(13,17,18)]
#datos_mlog = datos[c(13,12,18)]
#datos_mlog = datos[c(13,17,12,18)]
#datos_mlog = datos[c(10,12,13,17,18)]
#datos_mlog = datos[c(3,10,12,13,17,18)]
datos_mlog = datos[c(3,8,10,12,13,17,18)]
modelo_logistico <- glm(Subscription ~ ., data = datos_mlog, family = "binomial")
summary(modelo_logistico)

roc_modelo <- roc(datos_mlog$Subscription, modelo_logistico$fitted.values,auc=T)
roc_modelo
plot.roc(roc_modelo,print.auc=T,xlab="1-ESpecificidad",ylab="Sensibilidad")

## --------------------------------------------------------------------------------------------
# Regresión logística cargada en Kaggle en nov-29

datos_mlog = datos[c(3,8,10,12,13,17,18)]
modelo_logistico <- glm(Subscription ~ ., data = datos_mlog, family = "binomial")
summary(modelo_logistico)

roc_modelo <- roc(datos_mlog$Subscription, modelo_logistico$fitted.values,auc=T)
roc_modelo
plot.roc(roc_modelo,print.auc=T,xlab="1-ESpecificidad",ylab="Sensibilidad")

# Predicción de Subscription

glm(formula = Subscription ~ Job + Housing.Loan + Contact + Last.Contact.Month +
      Last.Contact.Duration + Poutcome, data = datos, family = "binomial")

glm.probs = predict.glm(modelo_logistico, type = "response", newdata = datos_test)
glm.pred = rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
glm.pred[glm.probs <= 0.5] = 0 # Esta línea se podría retirar


## --------------------------------------------------------------------------------------------
# Exportar archivo csv

exportar_kaggle <- tibble(Id=datos_test$..1,Predicted=glm.pred)
write.csv(exportar_kaggle, "prediccion_suscripcion.csv", row.names = FALSE)
