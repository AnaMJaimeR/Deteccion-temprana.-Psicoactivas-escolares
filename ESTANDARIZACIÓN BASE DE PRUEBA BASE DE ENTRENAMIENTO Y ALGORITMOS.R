#########################################################################
#########################################################################
###ESTANDARIZACION, BASE DE PRUEBA, BASE DE ENTRENAMIENTO Y ALGORITMOS###
########################################################################
########################################################################

#CONTENIDO:
#-ESTANDARIZACION
#-DIVISION DE LA BASE DE DATOS
#-SVM
#-KNN
#-ARBOLES DE DECISION
#-RANDOM FOREST
#-BAGGING
#-NAIVE BAYES (LAPLACE)

#Como primer paso, establezco el directorio en el que deseo trabajar para no tener 
#dificultades mas adelante al cargar la base de datos o guardar mis 
#objetos o archivos. Para ello utilizo la funcion "setwd" y entre parentesis y comillas
#especifico el directorio donde me deseo ubicar.
setwd("E:/BIG DATA/Talleres/PROYECTO FINAL/C覦IGOS AP蒒DICES")

#Asi mismo, para comenzar con un environment limpio utilizo la funcion: 
rm(list=ls())

#Se llama el environment que contenga la base lista a la cual se quiera aplicar la imputacion de datos.
#El data frame a tratar ha de llevar por nombre "psico" o "psico_imputed"

###############OJO#################
###############OJO#################
###############OJO#################
###############OJO#################
#si se usa un environment que contenga la base ya imputada cambiar todos los "psico" de este
#codigo por "psico_imputed"

################################################################################################

###ESTANDARIZACION
#Se aplica la funcion a cada variable explicativa de la base de datos
#"lapply" aplica una funcion a todos los elementos de una LISTA
#Se excluye la columna 41 que es la variable a predecir y la 1 que es string
#as.data.frame dice que el objeto creado debe ser identificado como un data frame
psico_NUM<-psico[,-1]
psico_NUM<-psico_NUM[,-40]
psico_z<-as.data.frame(scale(psico_NUM))
psico_z<-cbind(psico_z,psico[1],psico[41])
psico_z$CONSUME[psico_z$CONSUME==1]<-"SI"
psico_z$CONSUME[psico_z$CONSUME==2]<-"NO"
psico_z$CONSUME<-as.factor(psico_z$CONSUME)

################################################################################################

#DIVISION DE LA BASE DE DATOS

#Establezco una semilla a partir de la cual generare una serie de numeros aleatorios a traves del
#comando "set.seed"
set.seed(1996)

#Creo un objeto "a" que contiene el numero total de observaciones de la clase. Para hallar dicho numero
#hago uso de la funcion "length"
a<-length(psico$departam)

#Creo un objeto "b" que contiene el numero de observaciones que corresponde al 80% del total 
b<-round(0.8*a)

#Creo un vector que contiene "b" numeros aleatorios entre 1 y "a"
train_sample<-sample(a,b)
str(train_sample)

#Creo una base de entrenamiento con una muestra aleatoria a partir del vector de numeros aleatorios
#creado anteriormente. Para ello especifico entre brackets que las observaciones a tomar son aquellas
#especificads en el vector de numeros aleatorios. La base de prueba es creada de manera analoga pero
#con las observaciones restantes.
psico_z_train<-psico_z[train_sample,]
psico_z_test<-psico_z[-train_sample,]

#Mediante la funcion "prop.table" puedo verificar que las muestras son aleatorias dado que las proporciines
#de cada base (prueba y entrenamiento) tienen una distribucion similar a la de la base completa
prop.table(table(psico_z_train$CONSUME))
prop.table(table(psico_z_test$CONSUME))
prop.table(table(psico_z$CONSUME))


#################################################################################################

###SVM

#Se instala y se carga el paquete a utilizar
install.packages("kernlab")
library(kernlab)

#Se eliminan las variables no numericas
psico_z_svm<-psico_z[-1]
psico_z_svm_train<-psico_z_train[-1]
psico_z_svm_test<-psico_z_test[-1]
#Quedan 54 variables predictoras y 1 variable de interes

#Se crea un objeto que contenga el algortimo de SVM
scholars_rbfdot<-ksvm(as.matrix(psico_z_svm[,-43]),data=psico_z_svm_train,kernel="rbfdot")

#Se generan las predicciones sobre la base de prueba a partir del objeto scholars_rbfdot
predictions_rbfdot<-predict(scholars_rbfdot,psico_z_svm_test)

#Se verifica la precision del modelo

#a.
#La diagonal son los aciertos, el resto cuando falla
table(predictions_rbfdot,psico_z_svm_test$CONSUME)

#b. 
#Se instala y se carga el paquete "gmodels"
#install.packages("gmodels")
library(gmodels)

#Mediante la funcion "CrossTable" se contrastan las predicciones del modelo contra la clasificacion 
#real de las observaciones contenidas en la base de prueba.
CrossTable(psico_z_svm_test$CONSUME,predictions_rbfdot,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn=c("Consumidores (realidad)","Consumidores (predicci贸n)"))

#c.
#Se genera un vector que toma valor TRUE cuando hay acierto y FALSE cuando hay fallo

#vector logico que sea TRUE si hay acierto
agreement_svm<-predictions_rbfdot==psico_z_svm_test$CONSUME
table(agreement_svm)
prop.table(table(agreement_svm))

###Se halla que el nivel de precision con SVM (kernel=rbfdot) es del 

#################################################################################################

#KNN

#Para poder hacer uso de la funcion "knn" se carga el paquete de R que la contiene mediante
#funcion "library"
library(class)

#Se crea un vector que contenga solamente la variable de interes tanto de la base de prueba como la
#de entrenamietno
#a y b se definen en la seccion "NORMALIZACION"
c<-length(psico$departam)-b
psico_z_train_CONSUME<-psico_z[1:b,44]
psico_z_test_CONSUME<-psico_z[(b+1):a,44]

#Mediante operador "<-" se especifica el nombre del objeto en donde se van a guardar las predicciones
#que se hagan con el metodo knn. Se aplica la funcion "knn": en el primer parametro se especifica
#la base de entrenamiento, en el segundo parametro la base de prueba, en el tercer parametro
#la clasificacion que tiene la base de entrenamiento y, por ultimo, el cuarto parametro especifica
#el numero de "k (vecinos mas cercanos=raiz obs train)" a utilizar.
predictions_knn<-knn(train=psico_z_train, test=psico_z_test,cl=psico_z_train_CONSUME,k=273)

#La funcion table permite tabular los resultados de las predicciones hechas.
table(predictions_knn)

#Las predicciones hechas en base a la base de prueba arrojaron 


#Se verifica la precision del modelo

#a.
#La diagonal son los aciertos, el resto cuando falla
table(predictions_knn,psico_z_test$CONSUME)

#b. 
#Se instala y se carga el paquete "gmodels"
#install.packages("gmodels")
#library(gmodels)

#Mediante la funcion "CrossTable" se contrastan las predicciones del modelo contra la clasificacion 
#real de las observaciones contenidas en la base de prueba.
CrossTable(psico_z_test$CONSUME,predictions_knn,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn=c("Consumidores (realidad)","Consumidores (predicci贸n)"))

#c.
#Se genera un vector que toma valor TRUE cuando hay acierto y FALSE cuando hay fallo

#vector logico que sea TRUE si hay acierto
agreement_knn<-predictions_knn==psico_z_test$CONSUME
table(agreement_knn)
prop.table(table(agreement_knn))

###Se halla que el nivel de precision con KNN es del 

###############################################################################################

#ARBOLES DE DECISION

#Como primer paso se carga el paquete C50 que permitira construir el arbol de decision mediante la 
#funcion "library"
install.packages("C50")
library(C50)

#Se hace uso de la funcion "C5.0" especificando las variables de prediccion y los outcomes
#de la base de entrenamiento. Asi se crea el arbol de decision.
scholars_tree<- C5.0(psico_z_train[-44], as.factor(psico_z_train$CONSUME))

#Se llama el objeto y se inspecciona mediante la funcion "summary"
scholars_tree
summary(scholars_tree)

#Mediante la funcion "predict" se hace uso del objeto "scholars_tree" creado para hacer la prediccion sobre
#la base de prueba. Se almacena el vector de predicciones en un objeto llamado "predictions_tree".
predictions_tree<-predict(scholars_tree,psico_z_test)


#Se verifica la precision del modelo

#a.
#La diagonal son los aciertos, el resto cuando falla
table(predictions_tree,psico_z_test$CONSUME)

#b. 
#Se instala y se carga el paquete "gmodels"
install.packages("gmodels")
library(gmodels)

#Mediante la funcion "CrossTable" se contrastan las predicciones del modelo contra la clasificacion 
#real de las observaciones contenidas en la base de prueba.
CrossTable(psico_z_test$CONSUME,predictions_tree,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn=c("Consumidores (realidad)","Consumidores (predicci贸n)"))

#c.
#Se genera un vector que toma valor TRUE cuando hay acierto y FALSE cuando hay fallo

#vector logico que sea TRUE si hay acierto
agreement_tree<-predictions_tree==psico_z_test$CONSUME
table(agreement_tree)
prop.table(table(agreement_tree))

###Se halla que el nivel de precision con Arboles de Decision es del 


##############################################################################################

#RANDOM FOREST

#Se instala y se carga el paquete que se va a utilizar
#install.packages("randomForest")
library(randomForest)

#Se generan las muestras aleatorias y los grupos de variables aleatorias que van a ser utilizadas
#para la creacion de los distintos arboles de decision que conformaran el Random Forest
scholars_rf<-randomForest(x=psico_z_train[-44], y=psico_z_train$CONSUME, importance=TRUE)
#En este caso no se especifica nada en mtry para que use el default que es m=sqrt(p)

#Se visualiza el objeto
scholar_rf

#Se grafican las tasas de error
plot(scholars_rf,main="Tasa de Error")

#Se grafican las 10 variables mas importantes
varImpPlot(scholars_rf,sort=TRUE,main="Importancia de las Variables", type=2, n.var=10)

#Se genera la prediccion sobre la base de prueba
predictions_rf<-predict(scholars_rf,psico_z_test[-44])


#Se verifica la precision del modelo

#a.
#La diagonal son los aciertos, el resto cuando falla
table(predictions_rf,psico_z_test$CONSUME)

#b. 
#Se instala y se carga el paquete "gmodels"
#install.packages("gmodels")
#library(gmodels)

#Mediante la funcion "CrossTable" se contrastan las predicciones del modelo contra la clasificacion 
#real de las observaciones contenidas en la base de prueba.
CrossTable(psico_z_test$CONSUME,predictions_rf,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn=c("Consumidores (realidad)","Consumidores (predicci贸n)"))

#c.
#Se genera un vector que toma valor TRUE cuando hay acierto y FALSE cuando hay fallo

#vector logico que sea TRUE si hay acierto
agreement_rf<-predictions_rf==psico_z_test$CONSUME
table(agreement_rf)
prop.table(table(agreement_rf))

###Se halla que el nivel de precision con Random Forest es del 


##############################################################################################

#BAGGING

#Se instala y se carga el paquete que se va a utilizar
#install.packages("randomForest")
library(randomForest)

#Se generan las muestras aleatorias y los grupos de variables aleatorias que van a ser utilizadas
#para la creacion de los distintos arboles de decision que conformaran el Random Forest
scholars_bagging<-randomForest(x=psico_z_train[-44], y=psico_z_train$CONSUME, importance=TRUE,mtry=43)
#En este caso se especifica mtry=43 para que en cada arbol de decision se tome la totalidad de 
#las variables y de esa manera no sea Random Forest sino Bagging

#Se visualiza el objeto
scholars_bagging

#Se grafican las tasas de error
plot(scholars_bagging,main="Tasa de Error")

#Se grafican las 10 variables mas importantes
varImpPlot(scholars_bagging,sort=TRUE,main="Importancia de las Variables", type=2, n.var=10)

#Se genera la prediccion sobre la base de prueba
predictions_bagging<-predict(scholars_bagging,psico_z_test[-44])


#Se verifica la precision del modelo

#a.
#La diagonal son los aciertos, el resto cuando falla
table(predictions_bagging,psico_z_test$CONSUME)

#b. 
#Se instala y se carga el paquete "gmodels"
#install.packages("gmodels")
#library(gmodels)

#Mediante la funcion "CrossTable" se contrastan las predicciones del modelo contra la clasificacion 
#real de las observaciones contenidas en la base de prueba.
CrossTable(psico_z_test$CONSUME,predictions_bagging,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn=c("Consumidores (realidad)","Consumidores (predicci贸n)"))

#c.
#Se genera un vector que toma valor TRUE cuando hay acierto y FALSE cuando hay fallo

#vector logico que sea TRUE si hay acierto
agreement_bagging<-predictions_bagging==psico_z_test$CONSUME
table(agreement_bagging)
prop.table(table(agreement_bagging))


###Se halla que el nivel de precision con Bagging es del 


##################################################################################################

#NAIVE BAYES

#Se instala y se carga el paquete que se va a utilizar
install.packages("e1071")
library(e1071)

#Se genera un objeto que contiene el objeto con el algoritmo "naiveBayes" especificando que se debe
#utilizar el estimador de Laplace a fin de evitar inconvenientes por probabilidades iguales a 0.
#Para ello se escribe la opcion: laplace=1
scholars_naiveb_LAPLACE<-naiveBayes(psico_z_train[-44],psico_z_train$CONSUME, laplace = 1)

#Se hace uso del objeto creado para hacer las predicciones sobre la base de prueba
predictions_naiveb_LAPLACE<-predict(scholars_naiveb_LAPLACE,psico_z_test[-44])


#Se verifica la precision del modelo

#a.
#La diagonal son los aciertos, el resto cuando falla
table(predictions_naiveb_LAPLACE,psico_z_test$CONSUME)

#b. 
#Se instala y se carga el paquete "gmodels"
#install.packages("gmodels")
#library(gmodels)

#Mediante la funcion "CrossTable" se contrastan las predicciones del modelo contra la clasificacion 
#real de las observaciones contenidas en la base de prueba.
CrossTable(psico_z_test$CONSUME,predictions_naiveb_LAPLACE,prop.chisq = FALSE,prop.c = FALSE,prop.r = FALSE,dnn=c("Consumidores (realidad)","Consumidores (predicci贸n)"))

#c.
#Se genera un vector que toma valor TRUE cuando hay acierto y FALSE cuando hay fallo

#vector logico que sea TRUE si hay acierto
agreement_naiveb_LAPLACE<-predictions_naiveb_LAPLACE==psico_z_test$CONSUME
table(agreement_naiveb_LAPLACE)
prop.table(table(agreement_naiveb_LAPLACE))

###Se halla que el nivel de precision con Naive Bayes (LAPLACE) es del
