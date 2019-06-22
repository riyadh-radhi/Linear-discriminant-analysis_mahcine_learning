rm(list = ls())


library(MASS)
library(dplyr)
library(ggplot2)
library(rtf)
library(scales)
library(caret)
library(class)
library(pROC)
library(ROCR)



#1

df <- read.csv("System administrators.csv")
set.seed(175191)

index <- createDataPartition(df$Completed.task, p = 0.75,list = F)
train <- df[index,]
test <- df[-index,]


#2

ggplot(df, aes(Training, Experience))+
  geom_point(aes(col = df$Completed.task))

#3

train.lda <- lda(Completed.task ~ Training + Experience, data = train)
train.lda



#4

pred <- predict(train.lda, newdata = test)


head(pred$class)
round(head(pred$posterior),4)
head(pred$x)


#5

confusionMatrix(test$Completed.task, pred$class)



predd <- prediction(pred$x, test$Completed.task)

perf <- performance(predd, "tpr", "fpr")

plot(perf)


performance(predd, "auc")@y.values

#6


pred <- predict(train.lda, newdata = data.frame(Experience = 4,Training =6))
pred
#Not gonna finish the job

