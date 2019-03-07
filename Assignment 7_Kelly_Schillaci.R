library(readr)
setwd("C:\\Users\\kdoyl\\OneDrive\\Documents\\IST 707")
filename="Kaggle-digit-train-sample-small-1400.csv"
Kaggle_digit_train_sample_small_1400 <- read.csv(filename, header = TRUE, stringsAsFactors = TRUE)
dim(Kaggle_digit_train_sample_small_1400)
str(Kaggle_digit_train_sample_small_1400)
anyNA(Kaggle_digit_train_sample_small_1400)
sum(duplicated(Kaggle_digit_train_sample_small_1400))
table(Kaggle_digit_train_sample_small_1400$label)

Kaggle_digit_train_sample_small_1400$label=as.factor(Kaggle_digit_train_sample_small_1400$label)

(every4_rows<-seq(1,nrow(Kaggle_digit_train_sample_small_1400),4))

Digit_Test=Kaggle_digit_train_sample_small_1400[every4_rows, ]
Digit_Train=Kaggle_digit_train_sample_small_1400[-every4_rows, ]
## View the created Test and Train sets
(head(Digit_Train))
(table(Digit_Test$label))
(table(Digit_Train$label))

dim(Digit_Train)
plot(Digit_Train$label, col="purple")

dim(Digit_Test)
plot(Digit_Test$label, col="red")

Digit_Train_noLabel<-Digit_Train[-c(1)]
Digit_Train_justLabel<-Digit_Train$label
Digit_Test_noLabel<-Digit_Test[-c(1)]
Digit_Test_justLabel<-Digit_Test$label

str(Digit_Test)
dim(Digit_Train)
dim(Digit_Test_noLabel)


#kNN
Digit_Train_noLabel[1:784] <- lapply(Digit_Train[1:784], as.numeric)
Digit_Test_noLabel[1:784] <- lapply(Digit_Test_noLabel[1:784], as.numeric)
train_label = Digit_Train_justLabel
library(class)
library(caret)

predKNN <- knn(train=Digit_Train_noLabel, test=Digit_Test_noLabel, cl=train_label, k=3)
confusionMatrix(predKNN,Digit_Test_justLabel)

predKNN1 <- knn(train=Digit_Train_noLabel, test=Digit_Test_noLabel, cl=train_label, k=1)
confusionMatrix(predKNN,Digit_Test_justLabel)

predKNN5 <- knn(train=Digit_Train_noLabel, test=Digit_Test_noLabel, cl=train_label, k=5)
confusionMatrix(predKNN,Digit_Test_justLabel)

predKNN7 <- knn(train=Digit_Train_noLabel, test=Digit_Test_noLabel, cl=train_label, k=7)
confusionMatrix(predKNN,Digit_Test_justLabel)

predKNN9 <- knn(train=Digit_Train_noLabel, test=Digit_Test_noLabel, cl=train_label, k=9)
confusionMatrix(predKNN,Digit_Test_justLabel)

#SVMs
library("e1071")
SVM_fit_P <- svm(label~., data=Digit_Train, 
                 kernel="polynomial", cost=.1, 
                 scale=FALSE)
print(SVM_fit_P)
(pred_P <- predict(SVM_fit_P, Digit_Test, type="class"))
confusionMatrix(pred_P,Digit_Test_justLabel)

SVM_fit_L <- svm(label~., data=Digit_Train, 
                 kernel="linear", cost=.1, 
                 scale=FALSE)
print(SVM_fit_L)
(pred_L <- predict(SVM_fit_L, Digit_Test, type="class"))
confusionMatrix(pred_L,Digit_Test_justLabel)

SVM_fit_R <- svm(label~., data=Digit_Train, 
                 kernel="radial", cost=.1, 
                 scale=FALSE)
print(SVM_fit_R)
(pred_R <- predict(SVM_fit_R, Digit_Test, type="class"))
confusionMatrix(pred_R,Digit_Test_justLabel)

#random forest
library(randomForest)
digit_fit_RF <- randomForest(label ~ . , data = Digit_Train)
print(digit_fit_RF)
pred_RF<-predict(digit_fit_RF, Digit_Test)
confusionMatrix(pred_RF,Digit_Test_justLabel)
hist(treesize(digit_fit_RF))
