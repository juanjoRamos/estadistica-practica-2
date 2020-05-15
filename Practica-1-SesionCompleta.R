##PRACTICA 01
#PAQUETES
install.packages("data.table")
install.packages("modes")
install.packages("moments")

library(data.table)
library(modes)
library(moments)
#!PAQUETES
#SCRIPT
#DATOS
##IMPORTAR DATOS
notas <- "/home/jpramez/Documentos/ESTADISTICA/P01/Notas.csv"
dfNotas <- read.csv(notas,sep = ';', dec = ',' ,header = TRUE, stringsAsFactors = FALSE)
dtNotas <- data.table(dfNotas)
##PREPROCESAMIENTO
###NOMBRES DE COLUMNAS
names(dtNotas)
colnames(dtNotas) <- c("grupo","p1","p2","p3","totalPracticas","EParcial","EJunio","FJunio","ESeptiembre","FSeptiembre")
###FILTRAR DATOS BASURA
#dtNotas[is.na(dtNotas)] <- -1
dtNotas[dtNotas<0] <- NA
dtNotas

##### MAIN #####
#### TABLA DE FRECUENCIAS ABSOLUTAS ####
### UNIDIMENSIONAL

#-- ES UN PROCESO DIRECTO. LA FRECUENCIA ABSOLUTA DE LA VARIABLE GRUPO
freqGrupo <- table(dtNotas$grupo)
freqGrupo

### BIDIMENSIONAL ###

#-- EXTRAEMOS LOS VALORES DE, POR EJEMPLO, EL EXAMEN DE JUNIO, SIN PONDERAR
eJunio10 <- dtNotas$EJunio
eJunio10 <- eJunio10/0.6
#-- LOS VALORES NA LOS SUSTITUIMOS POR UN VALOR A MEDIDA Y QUE NUNCA PODRÁ SER CONSECUENCIA DE UN PROCESO VALIDO (-1, POR EJEMPLO)
eJunio10[is.na(eJunio10)] <- -1
labels <- c("NO PRESENTADO","SUSPENSO","APROBADO","NOTABLE","SOBRESALIENTE")
cutNotas <- cut(eJunio10, breaks = c(-1,0,5,7,9,10), right = F,include.lowest = T,labels = labels)
freqGrupoJunio <- table(dtNotas$grupo, cutNotas, dnn = c("Grupo","Notas"))

#### GRÁFICOS ####
### DIAGRAMA DE BARRAS  ###
#-- BARPLOT DE NOTAS DEL GRUPO A EN JUNIO
freqGrupoJunio[1,]
png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_BARRAS_A_JUNIO.png")
barplot(freqGrupoJunio[1,], xlab="Resultado",ylab = "Nº de alumnos", col = c("black","red","orange","yellow","green"), cex.lab = 1, cex.axis = 1, cex.names = 0.8)
dev.off()
#-- BARPLOT DE NOTAS DEL GRUPO B EN JUNIO
freqGrupoJunio[2,]
png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_BARRAS_B_JUNIO.png")
barplot(freqGrupoJunio[2,], xlab="Resultado",ylab = "Nº de alumnos", col = c("black","red","orange","yellow","green"), cex.lab = 1, cex.axis = 1, cex.names = 0.8)
dev.off()

#-- BARPLOT DE NOTAS DE AMBOS GRUPOS EN JUNIO
png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_BARRAS.png")
barplot(freqGrupoJunio,beside = T, xlab="Resultado",ylab = "Nº de alumnos", col = c("cyan","lightgreen"), cex.lab = 1, cex.axis = 1, cex.names = 0.8)
legend("topright", legend = c("GRUPO A", "GRUPO B"), fill = c("cyan", "lightgreen"))
dev.off()

### DIAGRAMA DE SECTORES ###
png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_SECTORES.png")
labels <- table(dtNotas$grupo[dtNotas$FSeptiembre >= 5.0])
pie(table(dtNotas$grupo[dtNotas$FSeptiembre >= 5.0]), label= labels , col= c("cyan","lightgreen"))
legend("topright", legend = c("GRUPO A", "GRUPO B"), fill = c("cyan", "lightgreen"))
dev.off()

### HISTOGRAMA ###

png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/Histograma_Jun.png")
h <- hist(eJunio10[eJunio10>0], col= c("white","gray"),main="Histograma de frecuencia",xlab="Notas de Junio",ylab = "Cantidad de alumnos", cex.lab =1.5)
dev.off()

#-- EXTRAEMOS LOS VALORES DE, POR EJEMPLO, EL EXAMEN DE JUNIO, SIN PONDERAR
eSept10 <- dtNotas$ESeptiembre
eSept10 <- eSept10/0.8
#-- LOS VALORES NA LOS SUSTITUIMOS POR UN VALOR A MEDIDA Y QUE NUNCA PODRÁ SER CONSECUENCIA DE UN PROCESO VALIDO (-1, POR EJEMPLO)
eSept10[is.na(eSept10)] <- -1
eSept10 <- eSept10[eSept10>0]
png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/Histograma_Sep.png")
h <- hist(eSept10, col= c("white","gray"),main="Histograma de frecuencia",xlab="Notas de Septiembre",ylab = "Cantidad de alumnos", cex.lab =1.5)
dev.off()

### DIAGRAMA DE CAJA ###
png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/D_CAJA.png")
boxplot(formula = dtNotas$FJunio ~ dtNotas$grupo, col="cyan",xlab="Notas de Junio",ylab="Alumnos", cex.lab =1.5)
dev.off()

### DIAGRAMA DE DISPERSIÓN ###
#- MAL AJUSTE
png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/Dispersion_Practicas_Junio.png")
plot(dtNotas$totalPracticas, dtNotas$FJunio, col="blue", xlab="Notas finales de practicas",ylab="Notas junio")
abline(lm(dtNotas$FJunio~dtNotas$totalPracticas),col="black")
dev.off()

#- BUEN AJUSTE
regresion <- lm(dtNotas$FJunio~dtNotas$EJunio)
summary(regresion)
regresion$coefficients[2]
formulaReg <- sprintf("y = %f + %fx",regresion$coefficients[1], regresion$coefficients[2])

png("/home/jpramez/Documentos/ESTADISTICA/P01/MEDIA/Dispersion_Junio_FinalJunio.png")
plot(dtNotas$EJunio, dtNotas$FJunio, col="blue", xlab="Notas Examen Junio",ylab="Notas junio")
abline(regresion,col="black")
legend("topleft", legend = formulaReg)
dev.off()

#### CALCULOS ####
fJunioDiscrete <- as.integer(dtNotas$FJunio)
fSeptiembreDiscrete <- as.integer(dtNotas$FSeptiembre)
### MEDIA ARITMETICA ###
mean(dtNotas$FJunio, na.rm = T)
mean(dtNotas$FSeptiembre, na.rm = T)
### MEDIANA ###
median(dtNotas$FJunio, na.rm = T)
median(dtNotas$FSeptiembre, na.rm = T)
### MODA ###
dtNotas$FJunio
modes(dtNotas$FJunio)
modes(dtNotas$FSeptiembre)
### ANALISIS DE LOS CUANTILES ###
quantile(dtNotas$FJunio, na.rm = T)
quantile(dtNotas$FSeptiembre, na.rm = T)

### VARIANZA ###
var(dtNotas$totalPracticas, na.rm = T)
var(dtNotas$FJunio, na.rm = T)
var(dtNotas$FSeptiembre, na.rm = T)

### DESVIACION TIPICA ###
sd(dtNotas$totalPracticas, na.rm = T)
sd(dtNotas$FJunio, na.rm = T)
sd(dtNotas$FSeptiembre, na.rm = T)

### COEFICIENTE DE VARIACION DE PEARSON ###
sd(dtNotas$FJunio)/mean(dtNotas$FJunio)
sd(dtNotas$FSeptiembre,na.rm = T)/mean(dtNotas$FSeptiembre, na.rm = T)
### COEFICIENTE DE ASIMETRIA DE FISHER ###
myFisher <- function(data){
  suma <- sum(data)
  media <- mean(data)
  desvEstandar <- sd(data)
  tam <- length(data)

  result <- (suma-media)^(desvEstandar/media)/tam
  return(result)
}
myFisher(dtNotas$FJunio)
sept <- table(dtNotas$FSeptiembre, exclude = NA)

myFisher(sept)

### COEFICIENTE DE CURTOSIS ###
?kurtosis
kurtosis(dtNotas$FJunio, na.rm = T)
kurtosis(dtNotas$FSeptiembre, na.rm = T)
### COVARIANZA ###
?cov
cov(dtNotas$EJunio, dtNotas$FJunio, use ="na.or.complete")
cov(dtNotas$ESeptiembre, dtNotas$FSeptiembre, use = "na.or.complete")
