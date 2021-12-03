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
library(MASS)
library(caret)


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
# Análisis lineal discriminante / todas las variables

# Datos entrenamiento
datos_lda= datos[c(2:18)]

# Modelo
modelo_lda <- lda(Subscription~., data = datos_lda)
modelo_lda

# Predicciones
predicciones <- predict(object = modelo_lda, newdata = datos_lda)

# Matriz de confusión
xtab <- table(predicciones$class, datos_lda$Subscription)
caret::confusionMatrix(xtab, positive = "1")

# Curva ROC
roc_lda <- roc(datos_lda$Subscription , predicciones$posterior[,2])
plot(roc_lda,print.auc=T,xlab="1-ESpecificidad",ylab="Sensibilidad")

## --------------------------------------------------------------------------------------------

# Análisis lineal discriminante / variables modelo regresión logística Kaggle
# cargado en Kaggle 01-dic


# Datos entrenamiento
datos_lda= datos[c(3,8,10,12,13,17,18)]

# Modelo
modelo_lda <- lda(Subscription~., data = datos_lda)
modelo_lda

# Predicciones
predicciones <- predict(object = modelo_lda, newdata = datos_lda)

# Matriz de confusión
xtab <- table(predicciones$class, datos_lda$Subscription)
caret::confusionMatrix(xtab, positive = "1")

# Curva ROC
roc_lda <- roc(datos_lda$Subscription , predicciones$posterior[,2])
plot(roc_lda,print.auc=T,xlab="1-ESpecificidad",ylab="Sensibilidad")

# Predicciones para Kaggle
predicciones_kaggle <- predict(object = modelo_lda, newdata = datos_test)


## --------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------

# Análisis lineal discriminante / variables modelo regresión logística Kaggle
# agrupando en Poutcome en datos entrenamiento
# Cargado en Kaggle dic-02

unique(datos$Poutcome) # "unknown" "failure" "other"   "success"
datos_ajuste_1 <- mutate(datos, Poutcome = ifelse(Poutcome == "success","success","other"))
unique(datos_ajuste_1$Poutcome)
colnames(datos_ajuste_1)

# Datos entrenamiento
datos_lda = datos_ajuste_1[c(3,8,10,12,13,17,18)]

# Modelo
modelo_lda <- lda(Subscription~., data = datos_lda)
modelo_lda

# Predicciones
predicciones <- predict(object = modelo_lda, newdata = datos_lda)

# Matriz de confusión
xtab <- table(predicciones$class, datos_lda$Subscription)
caret::confusionMatrix(xtab, positive = "1")

# Curva ROC
roc_lda <- roc(datos_lda$Subscription , predicciones$posterior[,2])
plot(roc_lda,print.auc=T,xlab="1-ESpecificidad",ylab="Sensibilidad")

# agrupando en Poutcome en datos test

unique(datos_test$Poutcome) # "unknown" "failure" "other"   "success"
datos_ajuste_test <- mutate(datos_test, Poutcome = ifelse(Poutcome == "success","success","other"))
unique(datos_ajuste_test$Poutcome)
colnames(datos_ajuste_test)


# Predicciones para Kaggle
predicciones_kaggle <- predict(object = modelo_lda, newdata = datos_ajuste_test)


## --------------------------------------------------------------------------------------------
## --------------------------------------------------------------------------------------------
# Exportar archivo csv

exportar_kaggle <- tibble(Id=datos_test$..1,Predicted=predicciones_kaggle$posterior[,2])
write.csv(exportar_kaggle, "prediccion_lda_suscripcion.csv", row.names = FALSE)


###
#Para agrupar categorías en una misma columna con 1 o 0
#library (fastDummies)
#data <- dummy_cols(data, select_columns = "nombre")
#dataf <- dummy_cols(data, select_columns = c('Job', 'Marital.Status','Education'
#                                             ,'Credit','Housing.Loan','Personal.Loan',
#                                             'Contact','Last.Contact.Month','Last.Contact.Month',
#                                             'Poutcome'),remove_selected_columns = TRUE)
#data$diciembre <- ifelse(data$mes = "diciembre", 1, 0)
###