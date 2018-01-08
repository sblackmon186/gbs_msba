library("nnet")


#libraries
library(data.table)
library(glmnet)

df <- fread('short_TrainingData.csv')

#random sample function
randomSample = function(df,n) { 
  return (df[sample(nrow(df), n),])
}

# trainsample = df

set.seed(99)
trainsample = randomSample(df, 50000)
# View(head(trainsample))
trainsample = subset(trainsample, select = -c(id, app_id))


#removing redundant columns
trainsample[(site_domain %in% c("f3845767")),site_domain:="0"]
# trainsample[(app_id %in% c("f3845767")),app_id:="1"]
# trainsample[(C16 %in% c(36)),C16:=0]



#converting type of factors.........................................................................................
trainsample$date = as.factor(trainsample$date)
trainsample$hour = as.factor(trainsample$hour)
trainsample$C1 = as.factor(trainsample$C1)
trainsample$banner_pos = as.factor(trainsample$banner_pos)
trainsample$site_id = as.factor(trainsample$site_id)
trainsample$site_domain = as.factor(trainsample$site_domain)
trainsample$site_category = as.factor(trainsample$site_category)
trainsample$app_domain = as.factor(trainsample$app_domain)
trainsample$site_category = as.factor(trainsample$site_category)
# trainsample$app_id = as.factor(trainsample$app_id)
trainsample$app_domain = as.factor(trainsample$app_domain)
trainsample$app_category = as.factor(trainsample$app_category)
trainsample$device_id = as.factor(trainsample$device_id)
trainsample$device_model = as.factor(trainsample$device_model)
trainsample$device_type = as.factor(trainsample$device_type)
trainsample$device_conn_type = as.factor(trainsample$device_conn_type)
trainsample$C15 = as.factor(trainsample$C15)
trainsample$C16 = as.factor(trainsample$C16)
trainsample$C17 = as.factor(trainsample$C17)
trainsample$C18 = as.factor(trainsample$C18)
trainsample$C19 = as.factor(trainsample$C19)
trainsample$C20 = as.factor(trainsample$C20)
trainsample$C21 = as.factor(trainsample$C21)




#creating training and validation sets
TrainInd <- ceiling(nrow(trainsample)*0.6)
TrainData <- trainsample[1:TrainInd,]
ValData <- trainsample[(TrainInd+1):nrow(trainsample),]



# running the neural net.........................................


TrainData5 = as.data.frame(TrainData)
ValData5 = as.data.frame(ValData)

#creating dummy variables
library(caret)
dmy_train <- dummyVars(" ~ .", data = TrainData5)
TrainData5 <- data.frame(predict(dmy_train, newdata = TrainData5))
# View(TrainData5)


dmy_val <- dummyVars(" ~ .", data = ValData5)
ValData5 <- data.frame(predict(dmy_val, newdata = ValData5))


library("nnet")
wmin()
# Need to scale the X's to 0,1.
Scale2UI <- function(x) {
  out <- (x-min(x))/(max(x)-wmin(x))
  return(out)
}
# X <- apply(TrainData5,2,FUN=Scale2UI)

# The Y needs to be converted into a matrix of dummy variables.

# class.ind() does this (a function in nnet)

Y = TrainData5$click

Y <- class.ind(Y)

X = subset(TrainData5, select = -c(click))

XVal = subset(ValData5, select = -c(click))

# TrainData5$click = as.da

library(randomForest)
out2 <- randomForest(click ~ .,data=TrainData5,
                     sampsize=ceiling(nrow(TrainData5)/4),
                     mtry=1,ntree=500,maxnodes=50)

PredProbs_nn <- predict(out2,newdata=ValData5)
# ypred <- matrix(ypred,nrow=n)

# 
# # This is taking about 3 minutes to run on my computer.
StartTime <- Sys.time()
out <- nnet(x=X,y=Y,size=20,softmax = T,MaxNWts=1000000,maxit=1000, linout = TRUE)
EndTime <- Sys.time()
round(EndTime-StartTime,2)
if(out$convergence==1) cat("Optimization did not converge!\n")

# To make the predictions, the Validation data needs to be scaled.
# NewX <- apply(PCompVal50d,2,FUN=Scale2UI)

PredProbs_nn <- predict(out,newdata=XVal, type = 'raw')
PredClass_nn <- predict(out,newdata=XVal)
# hist(apply(PredProbs_nn,1,max))

dim(PredProbs_nn)




df = as.data.frame(PredProbs_nn)
# df = subset(df, select = -c(df$`0`))
df = cbind(df, ValData5$click)



# df$`1` = as.numeric(df$`1`)
# df$`ValData5$click` = as.numeric(df$`ValData5$click`)
table(df$PredProbs_nn)
table(df$`ValData5$click`)


View(df)
# logloss function
LogLoss<-function(actual, predicted)
{
  result<- -1/length(actual)*(sum((actual*log(predicted)+(1-actual)*log(1-predicted))))
  return(result)
}

# 
# PredProbs_nn = as.data.frame(PredProbs_nn)
# 
# # View(PredProbs_nn)
# 
# df$`1`[df$`1`==0 ]<-0.0000000000000000000000000000000000000000000000000000000000000000000000000001
# 
# val_loss_logit_old = LogLoss(actual=ValData5$click, predicted=PredProbs_nn$`1`)
# train_loss_logit_old = LogLoss(actual= TrainData$click, predicted = pred_train)
# 
# View(df)
