#checking purpose(no need to run)
tree.conf <- table(tree.pred, df$grade)
merging
summary(tree.student)
min.idx

#library
library(ISLR)
library(tree)
library(nnet)
library(e1071)
library(neuralnet)
mat<-read.csv("student-mat.csv",header=TRUE, sep=";")
por<-read.csv("student-por.csv",header=TRUE, sep=";")
#merge
merging <- merge(mat,por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","G1","G2","G3"), all = TRUE)
merging
print(colSums(is.na(merging)))
summary(merging)
#Set grade
grade <- with(merging, ifelse(G1 >=16.1 & G3<=20, "A", G1))
grade <- with(merging, ifelse(G1 >=12.1 & G3<=16, "B", grade))
grade <- with(merging, ifelse(G1 >=8.1 & G3<=12, "C", grade))
grade <- with(merging, ifelse(G1 >=4.1 & G3<=8, "D", grade))
grade <- with(merging, ifelse(G1 >=0 & G3<=4, "E", grade))
student <- data.frame(merging, grade)
#Prediction
tree.student <- tree::tree(grade ~. -G1 , student)
set.seed(3)
train <- sample(1:nrow(student), nrow(student)*0.75)
student.test <- student[-train,]
grade.test <- student$grade[-train]
tree.student <- tree(grade ~ . -G1 , student , subset=train)
tree.pred <- predict(tree.student,student.test,type="class")
table(tree.pred,grade.test)
tree.pred
tree.pred <- predict(prune.student, student.test, type="class")
tree.conf <- table(tree.pred, grade.test,dnn=c("Prediction","Actual"))
tree.conf
#1 as 100% ,0.1 as 10%
#A,1 as 100% ,0.1 as 10%
tree.conf <- table(predicted.nn.values$net.result[,1],unlist(testData[70]),dnn=c("Prediction","Actual"))
tree.conf
sum(diag(tree.conf)) / sum(tree.conf)

#B,1 as 100% ,0.1 as 10%
tree.conf <- table(predicted.nn.values$net.result[,2],unlist(testData[65]),dnn=c("Prediction","Actual"))
tree.conf
sum(diag(tree.conf)) / sum(tree.conf) 

#C,1 as 100% ,0.1 as 10%
tree.conf <- table(predicted.nn.values$net.result[,3],unlist(testData[60]),dnn=c("Prediction","Actual"))
tree.conf
sum(diag(tree.conf)) / sum(tree.conf)


#Naive Bayes
set.seed(1234)
dataset<-student[,1:20] 
dataset <- data.frame(dataset, grade)
split <- sample(1:nrow(dataset), nrow(dataset)*0.7)
class <- dataset[split,]
pred <- dataset[-split,]
classifier <- naiveBayes(grade ~.-grade, class)
classifier
prediction <- predict(classifier, pred)
prediction
naive.conf <-table(prediction, pred$grade ,dnn=c("predict", "actual"))
naive.conf
#1 as 100% ,0.1 as 10%
sum(diag(naive.conf)) / sum(naive.conf)