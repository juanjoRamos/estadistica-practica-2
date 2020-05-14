
# Establecemos el workspace
setwd("D:/jotap/Google Drive/jpramez/UAL/ESTADISTICA/Practica02/Practica02")

# Importamos los datos del csv a la variable datos, indicando que
# tiene una cabecera y están separados por ";"
datos <- read.csv("notasP2.csv", header = TRUE, sep = ";")

# A pesar de que tienen cabecera, se guarda mal porque no está escrito en UTF-8. Vamos a 
# renombrar los nombres de las columnas
colnames(datos) = c("Grupo", "P1", "P2", "P3", "TP", "EP", "ExJ", "NFJ", "ExS", "NFS")

# Importamos las columnas deseadas para resolver la sesion 01
Grupo = datos[,"Grupo"]
P1 = datos[,"P1"]
P2 = datos[,"P2"]
P3 = datos[,"P3"]
TP = datos [,"TP"]
EP = datos[,"EP"]
ExJ = datos[,"ExJ"]
NFJ = datos[, "NFJ"]

#### Transformar  las  variables  cuantitativas  en  variables  cualitativas  con  valores  
#### aproba-do/suspenso. Asume que un alumno no presentado a una pr??actica o a la evaluacion
#### parcial ha suspendido.

# Cambiamos el formato format a numeric y guardamos, esto nos pondrá los campos no inicializados a 
# NA, por lo que el siguiente paso ser tratar estas posiciones para iniciazliarlas a 0
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

# Transformamos las variables cuantitativas a cualitativas creando una función
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
P1
P1 <- Cuantitativa_to_Cualitativa(P1,0.5)
P2 <- Cuantitativa_to_Cualitativa(P2,0.5)
P3 <- Cuantitativa_to_Cualitativa(P3,0.5)
TP <- Cuantitativa_to_Cualitativa(TP,1)
EP <- Cuantitativa_to_Cualitativa(EP,0.5)
ExJ <- Cuantitativa_to_Cualitativa(ExJ,3.5)
NFJ <- Cuantitativa_to_Cualitativa(NFJ,5)

# Visualizamos los vectores modificados, para revisar que esté todo correcto
P1
P2
P3
TP
EP
ExJ
NFJ

# Guardamos todo en un dataframe
db <- data.frame(Grupo,P1,P2,P3,EP,ExJ,NFJ)

#### 2. Aplicar un clasificador Naive Bayes y evaluar su capacidad de predicción calculando
####    las siguientes medidas:

# Primero vamos a instalar los paquetes necesarios para realizar este apartado

library(e1071)
library(lattice)
library(ggplot2)
library(caret)
  
modelo <- naiveBayes(db$NFJ ~ ., data = db)
modelo
pred1 <- predict(modelo, db)
(tab1 <- table(pred1, db$NFJ))

#### 3. Evalúa cómo van cambiando las medidas anteriores cuando el clasificador Naive Bayes
####    usa los siguientes conjuntos de variables:

######### a. Utilizando solo información de grupo

modelo <- naiveBayes(db$NFJ ~ ., data = db[c("Grupo")])
pred1 <- predict(modelo, db)
(tab1 <- table(pred1, db$NFJ))

######### b. Utilizando información de grupo, de Práctica 1 y de Cuestionario Tema 1 y 2.

modelo <- naiveBayes(db$NFJ ~ ., data = db[c("Grupo","P2","P2")])
pred1 <- predict(modelo, db)
(tab1 <- table(pred1, db$NFJ))

######### c. Utilizando información de grupo, de Práctica 1, de Práctica 2, de Cuestionario
#########    Tema 1 y 2 y de Cuestionario Tema 3 y 4.

modelo <- naiveBayes(db$NFJ ~ ., data = db[c("P1","P2","EP")])
pred1 <- predict(modelo, db)
(tab1 <- table(pred1, db$NFJ))

######### d. Utilizando toda la información disponible de grupo, prácticas y cuestionarios.

modelo <- naiveBayes(db$NFJ ~ ., data = db[,1:7])
pred1 <- predict(modelo, db)
(tab1 <- table(pred1, db$NFJ))

