# LDA & QDA using Diabetes 
library(MASS)
library(ggplot2)
library(caret)

data = read.csv(choose.files())
head(data)
str(data)
colSums(is.na(data))
data$Outcome = as.factor(data$Outcome)
boxplot(data)

#Linear Discriminant Analysis
actual_lda=lda(Outcome~.,data=data)
predicted_lda=predict(actual_lda)
mean(predicted_lda$class==data$Outcome)
a=confusionMatrix(predicted_lda$class,data$Outcome)$table
a

#Quadratic Discriminant Analysis
actual_qda=qda(Outcome~.,data=data)
predicted_qda=predict(actual_qda)
mean(predicted_qda$class==data$Outcome)
b=confusionMatrix(predicted_qda$class,data$Outcome)$table
b











# LDA & QDA using Breast Cancer
library(MASS)
library(ggplot2)
library(caret)

#importing Breast Cancer Dataset
data=read.csv(choose.files())
head(data)
View(data)
data=data[2:32]
colSums(is.na(data))
str(data)
data$diagnosis=as.factor(data$diagnosis)
boxplot(data)

#Linear Discriminant Analysis
actual_lda=lda(diagnosis~.,data=data)
predicted_lda=predict(actual_lda)
mean(predicted_lda$class==data$diagnosis)
a=confusionMatrix(predicted_lda$class,data$diagnosis)$table
a

#Quadratic Discriminant Analysis
actual_qda=qda(diagnosis~.,data=data)
predicted_qda=predict(actual_qda)
mean(predicted_qda$class==data$diagnosis)
b=confusionMatrix(predicted_qda$class,data$diagnosis)$table
b

