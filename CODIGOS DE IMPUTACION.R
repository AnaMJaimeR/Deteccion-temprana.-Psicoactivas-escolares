###########################################
###########################################
#CODIGOS DE IMPUTACION DE DATOS (ETAPA V).#
###########################################
###########################################

#Como primer paso, establezco el directorio en el que deseo trabajar para no tener 
#dificultades mas adelante al cargar la base de datos o guardar mis 
#objetos o archivos. Para ello utilizo la funcion "setwd" y entre parentesis y comillas
#especifico el directorio donde me deseo ubicar.
setwd("E:/BIG DATA/Talleres/PROYECTO FINAL/CÓDIGOS APÉNDICES")

#Asi mismo, para comenzar con un environment limpio utilizo la funcion: 
rm(list=ls())

#Se llama el environment que contenga la base lista a la cual se quiera aplicar la imputacion de datos.
#El data frame a tratar ha de llevar por nombre "psico"

################################Imputing the missing data###########################################

##MICE
install.packages("mice")
library(mice)
imputed<-mice(psico,m=5,method="pmm",seed=500)
psico_imputed<-complete(imputed,1)


##RFIMPUTE
install.packages("randomForest")
library(randomForest)
psico_imputed<-rfImpute(psico, psico$CONSUME,data=psico)


##MISSFOREST
install.packages("missForest")
library(missForest)
psico_imputed<-missForest(psico)
