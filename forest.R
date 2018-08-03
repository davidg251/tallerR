#install.packages('randomForest')
dataSet <- read.csv('breastCancerData.csv', header = TRUE, sep = ",")
library(randomForest)

Formula <- Quimio ~ Edad + 
                    Tamanio.tumor +
                    Invasion + 
                    Grado.Maligno


rf <- randomForest(Quimio ~ Edad + 
                    Tamanio.tumor +
                    Invasion + 
                    Grado.Maligno, 
                    data=dataSet, ntree=100, proximity=TRUE)

table(predict(rf), dataSet$Quimio)


bayes_classifier <- naiveBayes(Formula, data=dataSet)
preditec_bayes <- predict(bayes_classifier, dataSet)
print(preditec_bayes)
table(preditec_bayes, dataSet$Quimio)