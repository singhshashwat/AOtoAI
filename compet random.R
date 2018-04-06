library(randomForest)
library(unbalanced)
library(smotefamily)
library(e1071)
library(caret)
library(ROCR)
library(pROC)
library(dplyr)
library(caTools)
library(varSelRF)
library(adabag)
library(mlr)
library(GGally)
rm(list=ls())
ra <- read.csv("mens_train_file.csv", TRUE, ',')
sa <- read.csv("mens_test_file.csv", TRUE, ',')
sa1 <- read.csv("mens_test_file.csv", TRUE, ',')
sa
head(ra)
ncol(ra)
plot(ra$outcome)
table(ra$outcome)

summary(ra)

cor(ra[,-c(25,26,27,28,24,21,22,8,9,3)],use="everything", method = c("pearson","kendall","spearman"))
cor(ra[,-c(25,26,27,28,24,21,22,8,9,3,1,2)],use="everything", method = c("pearson"))


glimpse(ra)
glimpse(ra[,-c(25,26,27,28,24,21,22,8,9,3)])
# ra5 <- upSample(X,Y,list = FALSE, yname = "outcome")  
# nrow(ra5)
# table(ra5$outcome)

ggcorr(ra[,-c(25,26,27,28,24,21,22,8,9,3,1,2)],method = c("pairwise", "pearson"))

plot(ra$player.impact.depth,ra$player.depth)
abline(ra$player.impact.depth,ra$player.depth)
plot(ra$player.impact.depth,ra$previous.depth)
plot(ra$player.impact.depth,ra$previous.time.to.net)
plot(ra$previous.speed,ra$previous.time.to.net)
plot(ra$previous.net.clearance,ra$previous.time.to.net)
plot(ra$player.impact.distance.from.center,ra$previous.distance.from.sideline)

ncol(ra)
colnames(ra)
X=ra[,-c(25,26,28,27)]
colnames(X)
ncol(X)

Y = ra$outcome
head(Y)
ra1 <- upSample(X,Y,list = FALSE, yname = "outcome")  

table(ra1$outcome)



set.seed(123)
ind = sample.split(ra1,SplitRatio=0.75)

#ind <- sample_n(ra,nrow(ra), replace = TRUE, weight = NULL)
#ind <- sample(2,nrow(ra), replace = T)
head(ind)
nrow(ind)
ra.data <- subset(ra1, ind==TRUE)
ra.vdata <- subset(ra1, ind==FALSE)

head(ra.vdata)
#ra.data <- ra[ind==1,]
nrow(ra.data)
#ra.vdata <- ra[ind==2,]
nrow(ra.vdata)

#plot(ra$depth,ra$speed)

plot(ra1$outcome)
head(ra)
head(ra.vdata)
nrow(ra.data)
ncol(ra.vdata)
###----###
#nrow(ra5)
glimpse(ra.data)

plot(ra.data$rally,ra.data$outcome)




X1=ra.vdata[,-c(25,26,27,28)]
colnames(X1)
ncol(X1)
Y1 = ra.vdata$outcome
head(Y1)


ra5 = ra.data



head(ra5)
plot(ra5$outcome)
table(ra.data$outcome)
# rand1 <- randomForest(formula = outcome~.,data = ra5,ntree=200, replace=TRUE,cutoff=c(0.4,0.2,0.4))
# rand1

ncol(ra5)
colnames(ra5)
 abc <- varUsed(rand,by.tree = FALSE,count = TRUE)
# 
plot(abc)
View(abc)
# round(importance(rand1), 2)
# 
rand <- randomForest(formula= outcome~.,ntree=1000,data = ra5,
                        replace=FALSE,xtext= X1, ytest =Y1,keep.forest = TRUE)

rand <- randomForest(formula= outcome~.,ntree=1000,data = ra5,
                     replace=FALSE)

rand
round(importance(rand), 2)

outlier(rand$err.rate)

rand$err.rate

rand <- update(rand,subset=c(-1854))

rand1 = tuneRF(ra5[,-25],ra5$outcome,mtryStart = 4,ntreeTry = 1000,stepFactor = 2,improve = 0.001,trace = TRUE,plot = TRUE)


###---###

varImpPlot(rand)
str(ra5)


rand <- randomForest(formula = outcome ~ speed + net.clearance + distance.from.sideline + 
                        depth + outside.sideline + outside.baseline + player.distance.travelled + 
                        player.impact.depth + player.distance.from.center+player.depth + previous.speed + 
                        previous.net.clearance +previous.distance.from.sideline + previous.depth + opponent.depth +
                        previous.time.to.net, ntree=5000,data = ra5,replace=FALSE)
 
rand

rand <- randomForest(formula = outcome ~ speed + net.clearance + distance.from.sideline + 
                       depth + outside.sideline + outside.baseline + player.distance.travelled + 
                       player.impact.depth + previous.speed + previous.distance.from.sideline + 
                       opponent.depth + previous.time.to.net, ntree=1000,data = ra5,mtry = 5,replace=TRUE,
                       xtext= X1, ytest =Y1,keep.forest = TRUE)
#cutoff=c(0.5,0.24,0.26)

ra5$outside.baseline <- as.factor(ra5$outside.baseline)
ra5$outside.sideline <- as.factor(ra5$outside.sideline)
ra5$server.is.impact.player <- as.factor(ra5$server.is.impact.player)
ra5$server.is.impact.player
rand
rand <- randomForest(formula = outcome ~ speed + net.clearance  + 
                       depth + outside.sideline + outside.baseline + player.distance.travelled + 
                       player.depth+previous.speed + previous.distance.from.sideline +
                       opponent.depth , mtry = 4,ntree=1000,data = ra5,replace=TRUE)

rand



head(ra5)
round(importance(rand),2)

vb<- data.frame(ra5)
ncol(vb)

vb <- scale(vb[,-c(25,24,22,21,3,8,9)],center = TRUE,scale = TRUE)

vb

ra5 <- cbind(vb,ra5[,c(25,24,22,21,3,8,9)])

# adarand <- boosting(outcome~.,data=ra5,boos = TRUE)
# 
# adarand <- boosting(outcome ~ speed + net.clearance + distance.from.sideline + 
#                       depth + outside.sideline + outside.baseline + player.distance.travelled + 
#                       player.impact.distance.from.center+player.depth+previous.depth+
#                       player.impact.depth + previous.speed + previous.distance.from.sideline +
#                       opponent.depth + previous.time.to.net,data=ra5,boos = TRUE)
# 
# adarand$importance


table(rand$predicted)
#table(ra.data$outcome)
table(ra5$outcome)
round(importance(rand), 2)

rand$err.rate

adarand$class

varImpPlot(rand)

####----####

predd <- predict.boosting(adarand,ra.vdata,newmfinal = 100)

predd$confusion
1-predd$error

preddd <- boosting.cv(outcome~.,ra5, v = 10, boos = TRUE, mfinal = 50)

preddd$confusion
1-preddd$error
preddd$class
##################------------###########

predx <- predict(rand,ra.vdata,interval = "confidance")
table(ra.vdata$outcome)

table(predx,ra.vdata$outcome)
tabl <- table(predx,ra.vdata$outcome)
tabl
table(predx)

sum(diag(tabl))/sum(tabl)
1-sum(diag(tabl))/sum(tabl)

#predx <- as.numeric(predict(rand,ra.vdata,interval = "confidance"))
myRoc <- multiclass.roc(ra.vdata$outcome, as.numeric(predict(rand,ra.vdata,interval = "confidance")), percent = TRUE, levels = c("UE","FE"))

# plot(myRoc)
myRoc
myRoc <- roc(ra.vdata$outcome, as.numeric(predict(rand,ra.vdata,interval = "confidance")),plot=TRUE,smooth = TRUE, percent = TRUE, levels = c("FE","UE"))

plot(myRoc)
myRoc
myRoc1 <- smooth(myRoc,method="density",bw="nrd0",n=50)

plot(myRoc1)


# partialPlot(rand, ra5, speed, "W")
# partialPlot(rand, ra5, speed, "UE")
# partialPlot(rand, ra5, speed, "FE")
confusionMatrix(ra.vdata$outcome, predx)

plot(ra5$outcome)

auc(myRoc)

# library(mlr)
# asROCRPrediction(rand)
# head(predx,10)
predx <- (predict(rand,sa,type = "prob"))

sa[c("FE","UE","W")] <- predx
sa

predx <- predict(rand,sa,interval = "confidance")

sa[c("Confida")] <- predx
sa

write.csv(sa, file = "mens_test_file.csv",row.names=FALSE)
plot(sa$Confida)
plot(ra$outcome)


library(pROC)
library(ROCR)

pred.rand <- as.numeric(predict(rand1,ind,type="response"))
pred.rand

roc1 <- multiclass.roc(ind$outcome, pred.rand)
print(roc1$auc, digits = 3)
confusionMatrix(predx,ra.vdata$outcome)

head(predx)

ra$outcome
predx
# ROCcurve1 = prediction(ra5$outcome,predx)
# ROCcurve = performance(predx,"tpr","fpr")




predxe <- predict(rand1,sa,type="vote",norm.votes = TRUE, se.fit=TRUE)
table(predxe)
plot(predxe)
confusionMatrix(predxe,ra.vdata$outcome)
head(predxe)
sa1[c("a","b","c")] <- predxe
head(sa1)

####----####

predy <- predict(rand,ind,type="response")
table(predy)
plot(predy)
confusionMatrix(predy,ra.vdata$outcome)

####################__________##############################


library(pROC)
library(ROCR)

pred.rand <- as.numeric(predict(rand1,ra.vdata,type="response"))
table(pred.rand)
table(ra.vdata$outcome)

abc <- as.numeric(ind$outcome)
abc
roc1 <- multiclass.roc(ra.vdata$outcome, pred.rand)
print(roc1$auc, digits = 3)


pred.rand1 <- as.numeric(predict(rand1,ind,type="response"))
pred.rand

roc1 <- multiclass.roc(ind$outcome, pred.rand1)
print(roc1$auc, digits = 3)

#####-----------------################

X1=ra[,-ncol(ra)]
head(X1)
X=X1[,-ncol(X1)]
head(X)
Y = ra$outcome
head(Y)
up_train <- upSample(X,Y)                         
table(up_train$Class) 
ra6 <- up_train
head(ra6)

###----###



rand2 <- randomForest(formula = Class~.,data = ra6,ntree=175)
rand2
round(importance(rand2), 2)

###---###



rand3 <- randomForest(formula = Class ~ rally + serve + speed + distance.from.sideline + 
                        depth + outside.sideline + outside.baseline + player.distance.travelled + 
                        player.impact.depth + player.depth + previous.net.clearance + 
                        previous.distance.from.sideline + previous.depth + previous.hitpoint + 
                        previous.time.to.net + server.is.impact.player, ntree=175,data = ra6)

rand3

round(importance(rand3), 2)

####----####

predx <- predict(rand2,sa,type="response")
table(predx)
predx
plot(predx)
sa$outcome <- predx
head(sa)

confusionMatrix(predx,sa$outcome)
head(sa)



####----####

predy <- predict(rand3,sa,type="response")
table(predy)
plot(predy)
sa1$outcome <- predy
head(sa1)
confusionMatrix(predy,sa1$outcome)

library(pROC)
library(ROCR)

pred.rand <- as.numeric(predict(rand3,ind,type="response"))
pred.rand

roc1 <- multiclass.roc(ind$outcome, pred.rand)
print(roc1$auc, digits = 3)


write.csv(sa1, file = "mens_test_file1.csv",row.names=FALSE)

