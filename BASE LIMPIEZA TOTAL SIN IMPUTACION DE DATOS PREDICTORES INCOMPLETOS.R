#########################################################################
#########################################################################
#BASE LIMPIEZA TOTAL. SIN IMPUTACIÓN DE DATOS. PREDICTORES INCOMPLETOS.#
########################################################################
########################################################################

#Como primer paso, establezco el directorio en el que deseo trabajar para no tener 
#dificultades mas adelante al cargar la base de datos o guardar mis 
#objetos o archivos. Para ello utilizo la funcion "setwd" y entre parentesis y comillas
#especifico el directorio donde me deseo ubicar.
setwd("E:/BIG DATA/Talleres/PROYECTO FINAL/CÓDIGOS APÉNDICES")

#Asi mismo, para comenzar con un environment limpio utilizo la funcion: 
rm(list=ls())


#Llamo la base de datos psicoactivas 2011
psico<-read.csv("psico.csv")

####INFORMACION RELEVANTE VARIABLES INDICADORAS (para posterior interpretacion)
##(tipo de ordenamiento)
#p15<-1 es muy contento
#p17<-1 es muy estricto
#p18<-1 es muy probable
#p31<-1 es nunca
#p32<-1 es nunca le han ofrecido, a partir de 2 se aleja (2:en la casa, 3:en el colegio.....)
#p55<-1 es nunca
#p57<-1 es mucho
#p74<-1 es excelente


######################################################################
####ELIMINACION PRIMARIA Y SECUNDARIA DE VARIABLES (ETAPAS I Y II)####
######################################################################

##Se eliminan aquellas variables que no se consideran pertinentes.

#Para ello, se crea un vector que contiene el nombre de cada una de las variables a eliminar.
var_eliminar<-c("region","municipi","subregio","cod_dept","particin","dane","munic","dominio","upm",
                "codigoi","serie","orden","fexp","nombre_c","p13","p14","p24","p34_1",
                "p34_2","p34_3","p34_4","p34_5","p34_6","p34_7","p34_8","p34_9","p34_10",
                "p34_11","p34_12","p34_13","p34_14","p34_15","p34_16","p34_a","p34_b","p34_c",
                "p34_d","p34_e","p34_f","p34_g","p34_h","p34_i","p34_j","p34_k","p34_l","p34_m",
                "p34_n","p34_o","p34_p","p35_a","p35_b","p35_c","p35_d","p35_e","p35_f",
                "p35_g","p35_h","p35_i","p35_j","p35_k","p35_l","p35_m","p35_n","p35_o",
                "p35_p","p37_a","p37_b","p37_c","p37_d","p37_e","p37_f","p37_g","p37_h",
                "p37_i","p37_j","p37_k","p37_l","p37_m","p37_n","p37_o","p37_p","p41_a","p41_b",
                "p41_c","p41_d","p42_a","p42_b","p42_c","p42_d","p42_e","p42_f","p42_g","p42_i",
                "p42_k","p42_l","p42_m","p42_n","p42_o","p42_p","p42_q","p42_r","p45","p46",
                "p52_1","p52_2","p52_3","p52_4","p52_5","p52_6","p52_7","p52_8","p52_9",
                "p52_10","p52_11","p52_12","p52_13","p56","p58","p64","p66","p73_a","p73_b",
                "p73_c","p73_d","p73_e","p73_f","p73_g","num_estu","rechedad","rechotro",
                "p22p","p22m","p36_a","p36_b","p36_p","p43_a","p43_a","p43_b","p43_c","p43_d",
                "p43_e","p43_f","p43_g","p43_h","p43_i","p43_j","p43_k","p43_l","p43_m","p43_n",
                "p43_o","p43_p","p44_a","p44_b","p44_c","p44_d","p44_e","p44_f","p44_g","p44_h",
                "p44_i","p44_j","p44_k","p44_l","p44_m","p44_n","p44_o","p44_p","p47_a","p47_b",
                "p47_c","p47_d","p51_1","p51_2","p51_3","p51_4","p51_5","p51_6","p53m","p53p",
                "p54","p63_1","p63_2","p63_3","p63_4","p65_1","p65_2","p65_3","p67_a","p67_b",
                "p67_c","p68_a","p68_b","p68_c","p68_d","p68_e","p68_f","p68_g","p68_h","p68_i",
                "p68_j","p69","p70","p72","p48_1","p48_2","p48_3","p48_4","p48_5","p48_6","p48_7",
                "p48_8","p48_9","p48_10","p48_11","p48_12")


#A partir de ese vector se redefine el data frame psico con el que se va a trabajar.
psico<-psico[,!names(psico)%in%var_eliminar]

####################################
####REDEFINICION DE LOS MISSINGS####
####################################

##Se reemplazan los missing por 0 en variables de conteo (p13c,p14c)

#Explicacion: aquellos que no respondieron corresponden a alumnos que no perdieron ningun año
#ni en primaria (p13c) ni en bachillerato (p14c) y no observaciones con datos faltantes.

psico$p13c[is.na(psico$p13c)]<-0
psico$p14c[is.na(psico$p14c)]<-0

#Se verifica que se haya dado el cambio
psico$p13c
psico$p14c

##Se reemplazan los 9 o 99 por NA

#Explicacion: la base de datos original contenia el numero 9 o 99 para referirse a observaciones con
#missings en determinadas variables. Se hace el reemplazo para que R entienda que esos valores
#corresponden a missings.

pasar_missing_9<-list("p9","p11","p12","p15","p16","p17","p18","p19","p23","p25","p27",
                      "p28","p29","p30","p31","p32","p33_a","p33_b","p33_c","p33_d","p33_e",
                      "p36_c","p36_d","p36_e","p36_f","p36_g","p36_h","p36_i","p36_j","p36_k",
                      "p36_l","p36_m","p36_n","p36_o","p38_a","p38_b","p38_c","p38_d","p38_e",
                      "p38_f","p39_a","p39_b","p39_c","p39_d","p40_a","p40_b","p40_c","p40_d",
                      "p40_e","p40_f","p49","p50p","p50m","p55","p57","p59","p60","p61","p62_a",
                      "p62_b","p62_c","p62_d","p62_e","p62_f","p62_g","p74","p75","p13c","p14c",
                      "p26_1","p26_2","p26_3","p26_4","p26_5","p71_1","p71_2","p71_3","p71_4","p71_5")


pasar_missing_99<-list("p21")

for (i in pasar_missing_9){
  psico[[i]][psico[[i]]==9]<-NA
}

for (i in pasar_missing_99){
  psico[[i]][psico[[i]]==99]<-NA
}

#Se prueba que el loop no haya generado cambios adicionales a los deseados
table(psico$p62_d)
table(psico$p21)
table(psico$p13c)


##############################################################
####CREACION DE LA VARIABLE DE INTERES CONSUME (ETAPA III)####
##############################################################

####1<-Consume/2<-No Consume (alcohol y drogas)

##p36_: 
#1<-reporta haber consumido alcohol O drogas en los ultimos 30 dias
#2<-reporta no haber consumido ni alcohol ni drogas en los ultimos 30 dias

#Se pasan los NA a cero para que no se generen problemas al evaluar condicionales en 
#todas as variables p36_ a la vez.

p36_<-list("p36_c","p36_d","p36_e","p36_f","p36_g","p36_h","p36_i","p36_j",
           "p36_k","p36_l","p36_m","p36_n","p36_o")
for (i in p36_){
  psico[[i]][is.na(psico[[i]])]<-0
}

psico$CONSUME<-vector(mode="integer",length(length(psico$departam)))

for (i in c(1:length(psico$departam))){
  if(psico$p36_c[i]==2&&psico$p36_d[i]==2&&psico$p36_e[i]==2&&psico$p36_f[i]==2&&psico$p36_g[i]==2&&psico$p36_h[i]==2
     &&psico$p36_i[i]==2&&psico$p36_j[i]==2&&psico$p36_k[i]==2&&psico$p36_l[i]==2&&psico$p36_m[i]==2&&psico$p36_n[i]==2
     &&psico$p36_o[i]==2){psico$CONSUME[i]<-2}
}

for (i in c(1:length(psico$departam))){
  if(psico$p36_c[i]==1||psico$p36_d[i]==1||psico$p36_e[i]==1||psico$p36_f[i]==1||psico$p36_g[i]==1||psico$p36_h[i]==1
     ||psico$p36_i[i]==1||psico$p36_j[i]==1||psico$p36_k[i]==1||psico$p36_l[i]==1||psico$p36_m[i]==1||psico$p36_n[i]==1
     ||psico$p36_o[i]==1){psico$CONSUME[i]<-1}
}

##p39_:
#1<-aquellos que reporten en p39_ que si se han sentido como las afirmaciones

#Se pasan los NA a cero para que no se generen problemas al evaluar condicionales en 
#todas as variables p39_ a la vez.

p39_<-list("p39_a","p39_b","p39_c","p39_d")
for (i in p39_){
  psico[[i]][is.na(psico[[i]])]<-0
}

for (i in c(1:length(psico$departam))){
  if(psico$p39_a[i]==1||psico$p39_b[i]==1||psico$p39_c[i]==1||psico$p39_d[i]==1)
  {psico$CONSUME[i]<-1}
}


##p40_
#1<-cualquiera que haya reportado experimentar las sensaciones descritas en la pregunta
#con una frecuencia igual o superir a "de vez en cuando" durante los ultimos 12 meses

#Se pasan los NA a cero para que no se generen problemas al evaluar condicionales en 
#todas as variables p40_ a la vez.
p40_<-list("p40_a","p40_b","p40_c","p40_d","p40_f")
for (i in p40_){
  psico[[i]][is.na(psico[[i]])]<-0
}

for (i in c(1:length(psico$departam))){
  if(psico$p40_a[i]==3||psico$p40_a[i]==4||psico$p40_a[i]==5||psico$p40_b[i]==3||psico$p40_b[i]==4||
     psico$p40_b[i]==5||psico$p40_c[i]==3||psico$p40_c[i]==4||psico$p40_c[i]==5||psico$p40_d[i]==3||
     psico$p40_d[i]==4||psico$p40_d[i]==5||psico$p40_f[i]==3||psico$p40_f[i]==4||psico$p40_f[i]==5)
    {psico$CONSUME[i]<-1}
}

##p62_
#1<-cualquiera que haya reportado beber alcohol durante los ultimos 30 dias 
#con una frecuencia igual o superior a "1 a 2 veces por semana"

#Se pasan los NA a cero para que no se generen problemas al evaluar condicionales en 
#todas as variables p62_ a la vez.

p62_<-list("p62_a","p62_b","p62_c","p62_d","p62_e","p62_f","p62_g")
for (i in p62_){
  psico[[i]][is.na(psico[[i]])]<-0
}

for (i in c(1:length(psico$departam))){
  if(psico$p62_a[i]==3||psico$p62_a[i]==2||psico$p62_a[i]==1||psico$p62_b[i]==3||psico$p62_b[i]==2||
     psico$p62_b[i]==1||psico$p62_c[i]==3||psico$p62_c[i]==2||psico$p62_c[i]==1||psico$p62_d[i]==3||
     psico$p62_d[i]==2||psico$p62_d[i]==1||psico$p62_e[i]==3||psico$p62_e[i]==2||psico$p62_e[i]==1||
     psico$p62_f[i]==3||psico$p62_f[i]==2||psico$p62_f[i]==1||psico$p62_g[i]==3||psico$p62_g[i]==2||
     psico$p62_g[i]==1){psico$CONSUME[i]<-1}
}


##p71_
#2<-cualquiera que haya reportado nunca haber probado las drogas en mencion en p71_ ni haber
#ingerido bebidas alcoholicas durante los ultimos 30 dias p36_c, el no consumo de drogas
#se corrobora con la pregunta p36_ de consumo durante los ultimos 30 dias

#Se pasan los NA a cero para que no se generen problemas al evaluar condicionales en 
#todas las variables p71_ a la vez.

p71_<-list("p71_1","p71_2","p71_3","p71_4","p71_5")
for (i in p71_){
  psico[[i]][is.na(psico[[i]])]<-0
}

for (i in c(1:length(psico$departam))){
  if(psico$p71_1[i]==0&&psico$p71_2[i]==0&&psico$p71_3[i]==0&&psico$p71_4[i]==0
     &&psico$p71_5[i]==0&&psico$p36_c[i]==2&&psico$p36_d[i]==2&&psico$p36_e[i]==2&&psico$p36_f[i]==2&&psico$p36_g[i]==2&&psico$p36_h[i]==2
     &&psico$p36_i[i]==2&&psico$p36_j[i]==2&&psico$p36_k[i]==2&&psico$p36_l[i]==2&&psico$p36_m[i]==2&&psico$p36_n[i]==2
     &&psico$p36_o[i]==2){psico$CONSUME[i]<-2}
}


##Se eliminan todas las variables que fueron de utilidad para construir la variable de interes
##pero que ya no se requieren

eliminar_varinteres<-c("p36_c","p36_d","p36_e","p36_f","p36_g","p36_h","p36_i","p36_j",
                       "p36_k","p36_l","p36_m","p36_n","p36_o","p39_a","p39_b","p39_c","p39_d",
                       "p40_a","p40_b","p40_c","p40_d","p40_e","p40_f","p62_a","p62_b","p62_c",
                       "p62_d","p62_e","p62_f","p62_g","p71_1","p71_2","p71_3","p71_4","p71_5")
psico<-psico[,!names(psico)%in%eliminar_varinteres]

#se vuelven a pasar los 0 a NA
psico$CONSUME[psico$CONSUME==0]<-NA

##Se eliminan las observaciones a las cuales no se les pudo imputar una categoria para la 
##variable de interes
cc=is.na(psico$CONSUME)
M=which(cc==c("TRUE"))
psico=psico[-M,]


#Se hayan las proporciones de la variable de interÃ©s
prop.table(table(psico$CONSUME))


########################################################
####LIMPIEZA DE LOS PREDICTORES ESCOGIDOS (ETAPA IV)####
########################################################

##P19
#CAMBIO:se pasande 4 niveles a 2
#ahora la variable responde a la pregunta Ã‚Â¿cree o no?
#EXPLICACION: muchas opciones puedes distorsionar la informacion que se quiere capturar
#1,2y 3 pasan a 1=creyente; 4 pasa a 2=no creyente
psico$p19

psico$p19[psico$p19 == 2] <- 1
psico$p19[psico$p19 == 3] <- 1
psico$p19[psico$p19 == 4] <- 2
table(psico$p19)


##p21 
#CAMBIO:reajuste por intervalos. El numero de horas trabajadas a la semana oscila de 1 a 56.
#(1 a 5) pasa a ser 1<-"trabaja maximo 1h al dia en promedio"
#(6 a 10) pasa a ser 2<-"trabaja de 1h a 2h al dia en promedio"
#(11 a 15) pasa a ser 3<-"trabaja de 2h a 3h al dia en promedio"
#(16 a 20) pasa a ser 4<-"trabaja de 3h a 4h al dia en promedio"
#(21 a 25) pasa a ser 5<-"trabaja de 4h a 5h al dia en promedio"
#(26 a 30) pasa a ser 6<-"trabaja de 5h a 6h al dia en promedio"
#(31 a 35) pasa a ser 7<-"trabaja de 6h a 7h al dia en promedio"
#(36 a 40) pasa a ser 8<-"trabaja de 7h a 8h al dia en promedio"
#(41 a 45) pasa a ser 9<-"trabaja de 8h a 9h al dia en promedio"
#(46 a 50) pasa a ser 10<-"trabaja de 9h a 10h al dia en promedio"
#(51 a 56) pasa a ser 11<-"trabaja de 10h a 11h al dia en promedio"

#EXPLICACION:Ya que la persona reporta el numero total de horas que trabaja
#a la semana, los niveles son muchos y pueden resumirse en intervalos que arrojen informacion 
#similar frente a la carga laboral que asume.

counter<-0
for (i in c(1,6,11,16,21,26,31,36,41,46)){
  counter<-counter+1
  psico$p21[psico$p21<=(i+4)&psico$p21>=i]<-counter
} 

psico$p21[psico$p21<=56&psico$p21>=51]<-11
psico$p21[psico$p20==2]<-0

#se eliminan la variable p20 que ya no es de utilidad 
eliminarp20<-c("p20")
psico<-psico[,!names(psico)%in%eliminarp20]


#Se tabula para comprobar la creacion de intervalos
table(psico$p21)

##p26
#CAMBIO: eliminar categoria "otros" y
#resumir varias variables en 1, reducir el numero de categorias

#si responde 1 en cualquiera de las variable 26_ pasa a ser 1<- "nadie"
#si responde 2 y/o 5 (sin haber respondido 3 y/o 4) pasa a ser 2<- "adulto"
#si responde 3 y/o 4 (sin hacer respondido 2 y/o 5) pasa a ser 3<- "de edad similar"
#si responde (2 o 5) y (3 o 4) pasa a ser 4<-"tanto adulto como de edad similar"

#EXPLICACION: todas las var. p26_ dan cuenta de una misma informacion

#Se pasan los NA a cero para que no se generen problemas al evaluar condicionales en 
#todas as variables p26_ a la vez.6 que se quiere eliminar tambien pasa a 0

p26_<-list("p26_1","p26_2","p26_3","p26_4","p26_5")
for (i in p26_){
  psico[[i]][is.na(psico[[i]])]<-0
  psico[[i]][psico[[i]]==6]<-0
}

psico$p26<-vector(mode="integer",length(length(psico$p26_1)))

for (i in c(1:length(psico$p26_1))){
  if(psico$p26_1[i]!=1||psico$p26_2[i]!=1||psico$p26_3[i]!=1||psico$p26_4[i]!=1||psico$p26_5[i]!=1){psico$p26[i]<-4}
}

for (i in c(1:length(psico$p26_1))){
  if((psico$p26_1[i]!=1&&psico$p26_1[i]!=3&&psico$p26_1[i]!=4)&(psico$p26_2[i]!=1&&psico$p26_2[i]!=3&&psico$p26_2[i]!=4)
     &(psico$p26_3[i]!=1&&psico$p26_3[i]!=3&&psico$p26_3[i]!=4)&(psico$p26_4[i]!=1&&psico$p26_4[i]!=3&&psico$p26_4[i]!=4)
     &(psico$p26_5[i]!=1&&psico$p26_5[i]!=3&psico$p26_5[i]!=4)){psico$p26[i]<-2}
}

for (i in c(1:length(psico$p26_1))){
  if((psico$p26_1[i]!=1&&psico$p26_1[i]!=2&&psico$p26_1[i]!=5)&(psico$p26_2[i]!=1&&psico$p26_2[i]!=2&&psico$p26_2[i]!=5)
     &(psico$p26_3[i]!=1&&psico$p26_3[i]!=2&&psico$p26_3[i]!=5)&(psico$p26_4[i]!=1&&psico$p26_4[i]!=2&&psico$p26_4[i]!=5)
     &(psico$p26_5[i]!=1&&psico$p26_5[i]!=2&psico$p26_5[i]!=5)){psico$p26[i]<-3}
}

for (i in c(1:length(psico$p26_1))){
  if(psico$p26_1[i]==1||psico$p26_2[i]==1||psico$p26_3[i]==1||psico$p26_4[i]==1||psico$p26_5[i]==1){psico$p26[i]<-1}
}

#se eliminan las variables p26_ que ya no son de utilidad 
eliminarp26<-c("p26_1","p26_2","p26_3","p26_4","p26_5")
psico<-psico[,!names(psico)%in%eliminarp26]


##p33
#CAMBIO:
#4 pasa a 0 ya que indica que el individuo no posee informacion para responder
#se resumen todas las variables como una suma ponderada. De esta manera se responde a la pregunta
#Ã‚Â¿que tan facil o dificil es conseguir drogas?
#Siendo 1<- es muy facil conseguir todo tipo de drogas
#y 3<-no se consigue ningun tipo de drogas
#OJO: se hace una ponderacion porque se entiende que hay drogas mas asequibles que otras. Asi,
#si es muy facil conseguir heroina, esta droga tiene mayor peso en el puntaje final y este se
#acercara mas a 1 respecto a una respuesta "muy facil" frente a la marihuana.

#EXPLICACION: todas las variables p33_ dan respuesta a la misma pregunta por lo que es necesario
#resumirlas en 1 sola variable

lista_p33<-list("p33_a","p33_b","p33_c","p33_d","p33_e")

for (i in lista_p33){
  psico[[i]][psico[[i]]==4]<-0
}

psico$p33<-((psico$p33_a)*0.05)+((psico$p33_b)*0.1)+((psico$p33_c)*0.15)+((psico$p33_d)*0.25)+((psico$p33_e)*0.45)

#se verifica que el intervalo se cumpla
summary(psico$p33)
print(psico$p33[41750])
#se eliminan las variables p33_ que ya no son de utilidad 
eliminarp33<-c("p33_a","p33_b","p33_c","p33_d","p33_e")
psico<-psico[,!names(psico)%in%eliminarp33]

#p38
#cambio:
#4 pasa a 0 ya que indica que el individuo no posee informacion para responder
#se resumen todas las variables como una suma ponderada. De esta manera se responde a la pregunta
#Ã‚Â¿que tan facil o dificil es comprar licor?
#Siendo 1<- es muy facil conseguir todo tipo de trago
#y 3<-no se consigue ningun tipo de trago
#OJO: se hace una ponderacion porque se entiende que hay licores mas asequibles que otras. Asi,
#si es muy facil conseguir whisky, este licor tiene mayor peso en el puntaje final y este se
#acercara mas a 1 respecto a una respuesta "muy facil" frente a la cerveza.
#EXPLICACION: todas las variables p33_ dan respuesta a la misma pregunta por lo que es necesario
#resumirlas en 1 sola variable

#eliminamos la opcion f ya que "otros licores" no aporta mucho al analisis
which(colnames(psico)=="p38_f")
psico<-psico[,-54]


lista_p38<-list("p38_a","p38_b","p38_c","p38_d","p38_e")

for (i in lista_p38){
  psico[[i]][psico[[i]]==4]<-0
}

psico$p38<-((psico$p38_a)*0.05)+((psico$p38_b)*0.1)+((psico$p38_c)*0.15)+((psico$p38_d)*0.25)+((psico$p38_e)*0.45)

#se verifica que el intervalo se cumpla
summary(psico$p38)
print(psico$p38[41750])
#se eliminan las variables p38_ que ya no son de utilidad 
eliminarp38<-c("p38_a","p38_b","p38_c","p38_d","p38_e")
psico<-psico[,!names(psico)%in%eliminarp38]



