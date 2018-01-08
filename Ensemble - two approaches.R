#Ensembling

pred_train_dtree2 = as.data.frame(pred_train_dtree)
pred_train_dtree2 = subset(pred_train_dtree2, select = -c(`0`))
pred_train_logit2 = as.data.frame(pred_train_logit)


ens1_df = cbind(TrainData$click, pred_train_logit2, pred_train_dtree2)

colnames(ens1_df)[1] = 'click'
colnames(ens1_df)[2] = 'pred_logit'
colnames(ens1_df)[3] = 'pred_dtree'


# dim(ens1_df)



# using a decision tree as the ensemble

ens1_df$click = as.factor(ens1_df$click)


# library("rpart")

ens1_df$pred_logit = log(ens1_df$pred_logit)
ens1_df$pred_dtree = log(ens1_df$pred_dtree)


rpc <- rpart.control(minsplit=20,maxdepth=20,cp=0)
ens_tree <- rpart(click ~ .,data=ens1_df,control=rpc)

# Find the cp parameter for the best model and prune the tree.
bestcp <- ens_tree$cptable[which.min(ens_tree$cptable[,"xerror"]),"CP"]

# Store the "best" model in out1
ens_tree2 <- prune(ens_tree,cp=bestcp)



#predicting log loss for the validation dataset
pred_val_dtree2 = as.data.frame(pred_val_dtree)
pred_val_dtree2 = subset(pred_val_dtree2, select = -c(`0`))
pred_val_logit2 = as.data.frame(pred_val_logit)



val_ens1_df = cbind(pred_val_logit2, pred_val_dtree2)

colnames(val_ens1_df)[1] = 'pred_logit'
colnames(val_ens1_df)[2] = 'pred_dtree'

ens1_pred = predict(ens_tree2,newdata=val_ens1_df,type="prob")

# extracting it out and appending it to the dataframe
ens1_pred = as.data.frame(ens1_pred)
ens1_pred = subset(ens1_pred, select = -c(`0`))


val_loss_dtree = LogLoss(actual= ValData$click, predicted = ens1_pred$`1`)



# using averages or ma 



# PredClass <- predict(out1,newdata=ValData,type="class")
pred_val_ens_dtree <- predict(out1,newdata=ValData,type="prob")
pred_train_dtree <- predict(out1,newdata=TrainData,type="prob")

# dim(PredProbs)

#taking out 1 as final probabilities for prediction

# View(as.data.frame(PredProbs)


pred_val_dtree = as.data.frame(pred_val_dtree)


# ValData$click = as.numeric(ValData$click)
val_loss_dtree = LogLoss(actual= ValData$click, predicted = pred_val_dtree$`1`)




#using one output in the other input - logistic regression output in decision tree

TrainData3 = TrainData
pred_train_logit2 = as.data.frame(pred_train_logit)
TrainData3$pred_logit = pred_train_logit2$pred_train_logit


ValData3 = ValData
pred_val_logit = as.data.frame(pred_val_logit)
ValData3$pred_logit = pred_val_logit$pred_val_logit



# Using it in the tree.............................................





TrainData3$click = as.factor(TrainData3$click)


# library("rpart")
rpc <- rpart.control(minsplit=20,maxdepth=20,cp=0)
ens2_dtree <- rpart(click ~ .,data=TrainData3,control=rpc)

# Find the cp parameter for the best model and prune the tree.
bestcp <- ens2_dtree$cptable[which.min(ens2_dtree$cptable[,"xerror"]),"CP"]

# Store the "best" model in out1
ens2_dtree2 <- prune(ens2_dtree,cp=bestcp)
#summary(out1)




# PredClass <- predict(out1,newdata=ValData,type="class")
pred_val_ens2_dtree <- predict(ens2_dtree2,newdata=ValData3,type="prob")
pred_train_ens2_dtree <- predict(ens2_dtree2,newdata=TrainData3,type="prob")

# dim(PredProbs)

pred_val_ens2_dtree = as.data.frame(pred_val_ens2_dtree)


# ValData$click = as.numeric(ValData$click)
val_loss_dtree = LogLoss(actual= ValData$click, predicted = pred_val_ens2_dtree$`1`)


# View(PredProbs)
























