install.packages("readxl")
install.packages("tidyverse")
library("readxl")
library("tidyverse")

data_types<-c("text","text","date","text","text","text","text","numeric","numeric","numeric","numeric","numeric","text","text","text")
db = readxl::read_xlsx("Reporte ANS.xlsx", col_names = TRUE, na = "n/a", col_types = data_types)
db = data.frame(db)
db = na.omit(db)
summary(db)

########################################################
# Manipulación de Datos ################################
########################################################

#Paso 1: Cambiamos los estados cerrados a finalizados para concordancia y trabajamos sólo con estas solicitudes
db$Trato[db$Trato=="Cerrado"] = "Finalizado"
db = db[db$Trato=="Finalizado",]

db %>%
  distinct(Trato)

#Paso 2: Reemplazamos los indicadores de las solicitudes finalizadas que estén en "amarillo" o "naranjo" 
#y las pasamos a "verde"
db$Indicador[db$Indicador=="AMARILLO" & db$Trato=="Finalizado"] = "VERDE"
db$Indicador[db$Indicador=="NARANJO" & db$Trato=="Finalizado"] = "VERDE"

db %>%
  distinct(Indicador)

#Paso 3: Agregamos nuevas columnas (mes y año)
db$Mes = format(db$Fecha.ingreso.al.HUB,"%B")

db %>%
  distinct(Mes)

#Paso 4: Agregamos una nueva columna para categorizar los importes
db$Importe = ""
db$Importe[db$Importe.Neto.en.Euros < 1000000] = "< 1.000.000"
db$Importe[db$Importe.Neto.en.Euros >= 1000000] = ">= 1.000.000"

db %>%
  distinct(Importe)

#Paso 5: Agregamos una nueva columna para categorizar los importes
db$ANS = ""
db$ANS[db$ANS.Comprometido >= 40] = ">= 40"
db$ANS[db$ANS.Comprometido <= 40] = "<= 40"

db %>%
  distinct(ANS.Comprometido)

db %>%
  distinct(ANS)

summary(db)

#Paso 6: Seteamos como factores las variables que corresponden
db$Indicador = as.factor(db$Indicador)
db$Importe = as.factor(db$Importe)
db$ANS = as.factor(db$ANS)
db$Categoría.según.Gestor = as.factor(db$Categoría.según.Gestor)
db$Mes = as.factor(db$Mes)
db$País = as.factor(db$País)
db$Gestor.de.Compras = as.factor(db$Gestor.de.Compras)

#Paso 7: Guardamos en la variable datos, la información que es de interés para el estudio
datos = data.frame(db[,c(1,5,6,15:18)])
str(datos)
summary(datos)

#Paso 8: Realizamos un Test de Chi cuadrado para verificar dependencias.
tabla1 = table(datos$Gestor.de.Compras,datos$Indicador)
tabla2 = table(datos$País,datos$Indicador)
tabla3 = table(datos$Categoría.según.Gestor,datos$Indicador)
tabla4 = table(datos$Importe,datos$Indicador)
tabla5 = table(datos$ANS,datos$Indicador)
tabla6 = table(datos$Mes,datos$Indicador)


chisq.test(tabla1, simulate.p.value = T, B = 5000)
chisq.test(tabla2, simulate.p.value=T, B = 5000)
chisq.test(tabla3, simulate.p.value=T, B = 5000)
chisq.test(tabla4, simulate.p.value=T, B = 5000)
chisq.test(tabla5, simulate.p.value=T, B = 5000)
chisq.test(tabla6, simulate.p.value=T, B = 5000)

#Según los resultados del p-value, todas las asociaciones son dependientes,
#salvo la relación Indicador / Importe (tabla4)

#Paso 9: Para medir la asociación entre factores utilizamos V de Cramer
#El resultado toma valores entre 0 (independencia) y 1 (dependencia). 
#Resultado entre 0 y 0,2 indica que no hay asociación
#Resultado de 0,2 indica una asociación débil
#Resultado entre 0,2 y 0,6 indica una asociación moderada
#Resultado entre 0,6 y 1 indica una asociación fuerte.

assocstats(tabla1)#*
assocstats(tabla2)#*
assocstats(tabla3)
assocstats(tabla4)
assocstats(tabla5)
assocstats(tabla6)

#Según los resultados, existe una asociación moderada en la tabla 1 y la tabla 2,
#es decir, entre Indicador y Gestor de Compras e Indicador y País.

########################################################
# Modelamiento #########################################
########################################################
#Necesitamos un set de entrenamiento para generar un modelo predictivo.
n<-nrow(datos)
trainIndex<-sample(1:n,size=round(0.8*n),replace=F)

train.s<-data.frame(datos[trainIndex,])
test.s<-data.frame(datos[-trainIndex,])

########################################################
#REGRESIÓN LOGÍSTICA####################################
########################################################
glm.1 = glm(Indicador ~ ., data=train.s, family = "binomial")
glm.2 = glm(Indicador ~ País+Gestor.de.Compras+Mes, data=train.s, family = "binomial")
glm.3 = glm(Indicador ~ País+Gestor.de.Compras+Categoría.según.Gestor+Mes, data=train.s, family = "binomial")
AIC(glm.1,glm.2,glm.3)

summary(glm.1)


prediccion_glm.1 <- predict(glm.1,test.s[,2:7], type = 'response')
# confusion matrix
confusionMatrix_glm.1 <- table(test.s[,1], prediccion_glm.1 > 0.5)
confusionMatrix_glm.1

accuracy_glm.1 <- sum(diag(confusionMatrix_glm.1)) / sum(confusionMatrix_glm.1)
accuracy_glm.1
#La matriz de confusión nos entrega un Accuracy de 76%.