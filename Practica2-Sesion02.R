
# Establecemos el workspace
setwd("D:/jjrm1996/Google Drive/jjrm1996/UAL/ESTADISTICA/Practica02/Practica02")

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

#### 1. Transforma los variables cuantitaivas en variables cualitativas con valores aprobado/suspenso.
####    Los alumnos con notas no presentada contara como suspenso
NFJ

# Cambiamos el formato format a numeric y guardamos, esto nos pondrá los campos no inicializados a 
# NA, por lo que el siguiente paso sería tratar estas posiciones para iniciazliarlas a 0
P1 <- as.numeric(sub(",", ".", P1))
P2 <- as.numeric(sub(",", ".", P2))
P3 <- as.numeric(sub(",", ".", P3))
TP <- as.numeric(sub(",", ".", TP))
EP <- as.numeric(sub(",", ".", EP))
ExJ <- as.numeric(sub(",", ".", ExJ))
NFJ <- as.numeric(sub(",", ".", NFJ))

table(ExJ)

# Rellenamos con 0s las posiciones NA
P1[is.na(P1)] <- "No Presentado"
P2[is.na(P2)] <- "No Presentado"
P3[is.na(P3)] <- "No Presentado"
TP[is.na(TP)] <- "No Presentado"
EP[is.na(EP)] <- "No Presentado"
ExJ[is.na(ExJ)] <- "No Presentado"
NFJ[is.na(NFJ)] <- "No Presentado"


#### 2. Transforma las variables cuantitativas en variables cualitativas con valores suspenso,
####    aprobado, notable y sobresaliente 
ExJ
# Transformamos las variables cuantitativas a cualitativas creando una función
Cuantitativa_to_Cualitativa <- function(x){
  # Calculamos la escala de la variable con los datos dados
  a <- max(x[x!="No Presentado"])
  if(a<=1){
    maximo=1
  }else if(a<=2){
    maximo=2
  }else if(a<=7){
    maximo=7
  }else{
    maximo=10
  }
  # Asignamos valores Suspenso/Aprobado/Notable/Sobresaliente
  for(i in 1:length(x)) {
    if(x[i]=="No Presentado"){
      x[i]="Suspenso"
    }else if(x[i]<(maximo*0.5)){
      x[i]="Suspenso"
    }else if(x[i]<(maximo*0.7)){
      x[i]="Aprobado"
    }else if(x[i]<(maximo*0.9)){
      x[i]="Notable"
    }else{
      x[i]="Sobresaliente"
    }
  }
  return(x)
}

P1 <- Cuantitativa_to_Cualitativa(P1)
P2 <- Cuantitativa_to_Cualitativa(P2)
P3 <- Cuantitativa_to_Cualitativa(P3)
TP <- Cuantitativa_to_Cualitativa(TP)
EP <- Cuantitativa_to_Cualitativa(EP)
ExJ <- Cuantitativa_to_Cualitativa(ExJ)
NFJ <- Cuantitativa_to_Cualitativa(NFJ)

table(NFJ)
# Guardamos todo en un dataframe
db <- data.frame(Grupo,P1,P2,P3,TP,EP,ExJ,NFJ)

#### 3. Obtén las tablas de probabilidad condicionada estimadas por el clasificador Naive Bayes y comenta las más
####    relevantes:

# Primero vamos a instalar los paquetes necesarios para realizar este apartado

library(e1071)
library(lattice)
library(ggplot2)
library(caret)

modelo <- naiveBayes(db$NFJ ~ ., data = db)
modelo
pred1 <- predict(modelo, db)
(tab1 <- table(pred1, db$NFJ))

modelo <- naiveBayes(db$NFJ ~ ., data = db[c("P1","P2","P3","TP")])
pred2 <- predict(modelo, db)
(tab2 <- table(pred2, db$NFJ))

modelo <- naiveBayes(db$NFJ ~ ., data = db[c("EP")])
pred2 <- predict(modelo, db)
(tab2 <- table(pred2, db$NFJ))

modelo <- naiveBayes(db$NFJ ~ ., data = db[c("ExJ")])
pred2 <- predict(modelo, db)
(tab2 <- table(pred2, db$NFJ))
