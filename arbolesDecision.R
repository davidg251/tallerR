install.packages("party")
install.packages("rpart.plot")
install-packages("readr")
install.packages("dplyr")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("ROCR")

library(party)

dataSet <- read.table('/code/breastCancerData.csv', header = TRUE, sep = ",")
#dim(dataSet)
#names(dataSet)
#head(dataSet)

indexes <- sample(2, nrow(dataSet), replace=TRUE, prob=c(0.7, 0.3))
trainSet <- dataSet[indexes==1,]
testSet <- dataSet[indexes==2,]
#dim(trainSet)
#dim(testSet)

# Formula para predecir si una paciente necesita
# quimio dependiendo de su edad, el tamaño del tumor
# y el grado del tumor

Formula <- Quimio ~ Edad + 
                    Tamaño.tumor +
                    Invasion + 
                    Grado.Maligno

ctree <- ctree(Formula, data=set_entrenamiento)

table(predict(ctree), set_prueba$Quimio)

print(ctree)

plot(ctree)

# para mira otras graficas
library(rpart)
library(rpart.plot)


ctree2 <- rpart(Formula, 
                method = "class",
                data = dataSet,
                minsplit = 2,
                minbucket = 1
                )
attributes(ctree2)
rpart.plot(ctree2)
plot(ctree2)
text(ctree2, use.n=T)

