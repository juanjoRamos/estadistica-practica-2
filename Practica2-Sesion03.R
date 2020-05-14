
# Establecemos el workspace
setwd("D:/jotap/Google Drive/jpramez/UAL/ESTADISTICA/Practica02/Practica02")

# Importamos los datos del csv a la variable datos, indicando que
# tiene una cabecera y están separados por ";"
datos <- read.csv("notasP2.csv", header = TRUE, sep = ";")

# A pesar de que tienen cabecera, se guarda mal porque no está escrito en UTF-8. Vamos a 
# renombrar los nombres de las columnas
colnames(datos) = c("Grupo", "P1", "P2", "P3", "TP", "EP", "ExJ", "NFJ", "ExS", "NFS")

# Importamos las columnas deseadas para resolver la sesion 02
Grupo = datos[,"Grupo"]
P1 = datos[,"P1"]
P2 = datos[,"P2"]
P3 = datos[,"P3"]
TP = datos [,"TP"]
EP = datos[,"EP"]
ExJ = datos[,"ExJ"]
NFJ = datos[, "NFJ"]

#### 1. Utiliza las variables cuantitativas sin transformar asumiendo que se distribuyen de forma
####    Normal dada la variable a predecir y evalúa si esto da lugar a mejores predicciones.

# Cambiamos el formato format a numeric y guardamos, esto nos pondrá los campos no inicializados a 
# NA, por lo que el siguiente paso sería tratar estas posiciones para iniciazliarlas a 0
P1 <- as.numeric(sub(",", ".", P1))
P2 <- as.numeric(sub(",", ".", P2))
P3 <- as.numeric(sub(",", ".", P3))
TP <- as.numeric(sub(",", ".", TP))
EP <- as.numeric(sub(",", ".", EP))
ExJ <- as.numeric(sub(",", ".", ExJ))
NFJ <- as.numeric(sub(",", ".", NFJ))

# Rellenamos con 0s las posiciones NA
P1[is.na(P1)] <- 0
P2[is.na(P2)] <- 0
P3[is.na(P3)] <- 0
TP[is.na(TP)] <- 0
EP[is.na(EP)] <- 0
ExJ[is.na(ExJ)] <- 0
NFJ[is.na(NFJ)] <- 0

# Guardamos todo en un dataframe
db <- data.frame(Grupo,P1,P2,P3,TP,EP,ExJ,NFJ)

# Primero vamos a instalar los paquetes necesarios para realizar este apartado

library(e1071)
library(lattice)
library(ggplot2)
library(caret)

# Renombramos la variable NFJ del data.frame que acabamos de crear, y la hacemos cualitativa
db$NFJ <- cut(db$NFJ, breaks = c(0,4.99,10))

# Creamos el modelo naiveBayes y lo probamos en las mismas condiciones que en el objetivo 1
modeloCuantitativo <- naiveBayes(db$NFJ ~ ., data = db[,])

######### a. Utilizando solo información de grupo

modeloCuantitativo <- naiveBayes(db$NFJ ~ ., data = db[c("Grupo")])
pred1 <- predict(modeloCuantitativo, db)
(tab1 <- table(pred1, db$NFJ))

######### b. Utilizando información de grupo, de Práctica 1 y de Cuestionario Tema 1 y 2.

modeloCuantitativo <- naiveBayes(db$NFJ ~ ., data = db[c("Grupo","P1","P2","P3","TP")])
pred1 <- predict(modeloCuantitativo, db)
(tab1 <- table(pred1, db$NFJ))

######### c. Utilizando información de grupo, de Práctica 1, de Práctica 2, de Cuestionario
#########    Tema 1 y 2 y de Cuestionario Tema 3 y 4.

modeloCuantitativo <- naiveBayes(db$NFJ ~ ., data = db[c("Grupo","P1","P2","P3","TP","EP","ExJ")])
pred1 <- predict(modeloCuantitativo, db)
(tab1 <- table(pred1, db$NFJ))

######### d. Utilizando toda la información disponible de grupo, prácticas y cuestionarios.

modeloCuantitativo <- naiveBayes(db$NFJ ~ ., data = db[,1:7])
pred1 <- predict(modeloCuantitativo, db)
(tab1 <- table(pred1, db$NFJ))


#### 2. Analiza si es posible mejorar la capacidad de predicción del clasificador transformando
####    algunas variables en aprobado/suspenso y dejando otras sin transformar.

# Transformamos las variables cuantitativas que vamos a seleccionar a cualitativas creando una función
Cuantitativa_to_Cualitativa <- function(x,y){
  for(i in 1:length(x)) {
    if(x[i]<y){
      x[i]="Suspenso"
    }else{
      x[i]="Aprobado"
    }
  }
  return(x)
}

P1 <- Cuantitativa_to_Cualitativa(P1,0.66)
P2 <- Cuantitativa_to_Cualitativa(P2,0.66)
P3 <- Cuantitativa_to_Cualitativa(P3,0.66)
TP <- Cuantitativa_to_Cualitativa(TP,2)
EP <- Cuantitativa_to_Cualitativa(C1,2)


dbCualitative <- data.frame(Grupo,P1,P2,P3,TP,EP,ExJ,NFJ)
dbCualitative$NFJ <- cut(dbCualitative$NFJ, breaks = c(0,4.99,10))

# Creamos el modelo naiveBayes y lo probamos en las mismas condiciones que en el objetivo 1
modelo <- naiveBayes(dbCualitative$NFJ ~ ., data = dbCualitative[,])

######### a. Utilizando solo información de grupo

modelo <- naiveBayes(dbCualitative$NFJ ~ ., data = dbCualitative[c("Grupo")])
pred1 <- predict(modelo, dbCualitative)
(tab1 <- table(pred1, dbCualitative$NFJ))

######### b. Utilizando información de grupo, de Práctica 1 y de Cuestionario Tema 1 y 2.

modelo <- naiveBayes(dbCualitative$NFJ ~ ., data = dbCualitative[c("Grupo","P1")])
pred1 <- predict(modelo, dbCualitative)
(tab1 <- table(pred1, dbCualitative$NFJ))

######### c. Utilizando información de grupo, de Práctica 1, de Práctica 2 y examen parcial

modelo <- naiveBayes(dbCualitative$NFJ ~ ., data = dbCualitative[c("Grupo","P1","P2","EP")])
pred1 <- predict(modelo, dbCualitative)
(tab1 <- table(pred1, dbCualitative$NFJ))

######### d. Utilizando toda la información disponible de grupo, prácticas y cuestionarios.

modelo <- naiveBayes(dbCualitative$NFJ ~ ., data = dbCualitative[,1:7])
pred1 <- predict(modelo, dbCualitative)
(tab1 <- table(pred1, dbCualitative$NFJ))


#### 3. Calcula la predicción de Aprobar en Junio para cada uno de los miembros del grupo
####    que ofrece el clasificador Naive Bayes que habéis construido en base a las notas que ya
####    tenéis disponibles de esta asignatura.


# Juan Pedro Ramos

Grupo <- c("B")
P1 <- c(9)
P2 <- c(0)
P3 <- c(0)
TP <- c(0)
EP <- c(1.25)
ExJ <- c(0)
NFJ <- c(0)
db <- data.frame(Grupo,P1,P2,P3,TP,EP,ExJ,NFJ)
pred1 <- predict(modeloCuantitativo, db)
(tab1 <- table(pred1, db$NFJ))


# Javier Valero

Grupo <- c("B")
P1 <- c(9)
P2 <- c(0)
P3 <- c(0)
TP <- c(0)
EP <- c(1.5)
ExJ <- c(0)
NFJ <- c(0)
db <- data.frame(Grupo,P1,P2,P3,TP,EP,ExJ,NFJ)
pred1 <- predict(modeloCuantitativo, db)
(tab1 <- table(pred1, db$NFJ))

# Javier Sanchez

Grupo <- c("B")
P1 <- c(9)
P2 <- c(0)
P3 <- c(0)
TP <- c(0)
EP <- c(1)
ExJ <- c(0)
NFJ <- c(0)
db <- data.frame(Grupo,P1,P2,P3,TP,EP,ExJ,NFJ)
pred1 <- predict(modeloCuantitativo, db)
(tab1 <- table(pred1, db$NFJ))

#Juan Jose

Grupo <- c("B")
P1 <- c(9)
P2 <- c(0)
P3 <- c(0)
TP <- c(0)
EP <- c(1)
ExJ <- c(0)
NFJ <- c(0)
db <- data.frame(Grupo,P1,P2,P3,TP,EP,ExJ,NFJ)
pred1 <- predict(modeloCuantitativo, db)
(tab1 <- table(pred1, db$NFJ))

#Joaquin

Grupo <- c("B")
P1 <- c(9)
P2 <- c(0)
P3 <- c(0)
TP <- c(0)
EP <- c(2)
ExJ <- c(0)
NFJ <- c(0)
db <- data.frame(Grupo,P1,P2,P3,TP,EP,ExJ,NFJ)
pred1 <- predict(modeloCuantitativo, db)
(tab1 <- table(pred1, db$NFJ))


