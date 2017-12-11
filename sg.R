rm(list = ls(all = T))


setwd("C:\\Users\\hp\\Desktop\\Hacker Earth\\SocGen")
list.files()
tr<-read.csv("train.csv",header = T,na.strings=c(" ", "NA"),sep = ",")
dim(tr)
ts<-read.csv("test.csv")
dim(ts)
sum(is.na(tr))
summary(tr)
str(ts)
sum(is.na(ts))

library(DMwR)

tr_imputed <- centralImputation(data = tr) #Cenral Imputation
sum(is.na(tr_imputed))
ts_imputed <- centralImputation(data = ts) #Cenral Imputation
sum(is.na(ts_imputed))
summary(ts)
summary(tr)

#View(tr)
tr_imputed$portfolio_id=NULL
tr_imputed$status=NULL
tr_imputed$hedge_value=NULL
tr_imputed$indicator_code=NULL
ts_imputed$portfolio_id=NULL
ts_imputed$hedge_value=NULL
ts_imputed$status=NULL
ts_imputed$indicator_code=NULL
ts_imputed$desk_id=NULL
tr_imputed$desk_id=NULL
str(tr_imputed)

summary(tr_imputed)

summary(ts_imputed)

dim(tr_imputed)
dim(ts_imputed)
library(zoo)

tr_imputed$start_date=as.Date(as.character(tr_imputed$start_date), "%Y%m%d")

tr_imputed$creation_date=as.Date(as.character(tr_imputed$creation_date), "%Y%m%d")
tr_imputed$sell_date=as.Date(as.character(tr_imputed$sell_date), "%Y%m%d")


ts_imputed$start_date=as.Date(as.character(ts_imputed$start_date), "%Y%m%d")

ts_imputed$creation_date=as.Date(as.character(ts_imputed$creation_date), "%Y%m%d")
ts_imputed$sell_date=as.Date(as.character(ts_imputed$sell_date), "%Y%m%d")
str(tr_imputed)
sum(is.na(tr_imputed))
ts$libor_rate

model<-lm(return~pf_category+euribor_rate+type+libor_rate,tr_imputed)
summary(model)


par(mfrow = c(2,2))
plot(model)

prd<-predict(model,tr_imputed)
predict(model,ts_imputed)->pred.tst



cbind(ts,pred.tst)->output
names(ts)

output

names(output)

#View(output)

output[,-c(2:17)]->output1

output1
names(output1)

#View(output1)
#write.csv()

write.csv(x = output1,file = "jreddy2.csv",row.names=F)



#write.csv(x = output1,file = "jassureddy.csv",row.names=F)



names(ts_imputed)

names(tr_imputed)
str(ts_imputed)
##feature selection
library(MASS)
model_aic=stepAIC(model)
model_aic$coefficients
summary(model_aic)
plot(model_aic)


predict(model_aic,ts_imputed)->return

tr$return

cbind(ts,pred.tst1)->output
names(ts)

output

#View(output)

output[,-c(2:17)]->output1

names(output1)

#View(output1)
#write.csv()

write.csv(x = output1,file = "jreddy1.csv",row.names=F)



#write.csv(x = output1,file = "jassureddy.csv",row.names=F)


library(rpart)
rpart(return~.,data =tr_imputed)->rf
summary(rf)

plot(rf)
prediction1 = predict(rf, ts_imputed)

plot(prediction1)
library(DMwR)

regr.eval(tr$return, prediction1)

prediction.test = predict(rf, ts_imputed)

prediction.test

cbind(ts,prediction.test)->output4

output4[,-c(2:17)]->output5

names(output5)

write.csv(x = output5,file = "jreddy4.csv",row.names=F)



regr.eval(Test_df$Sales.In.ThousandDollars., prediction.test)

rf$cptable[which.min(rf$cptable[,"xerror"]),"CP"]->min.xerror

rt.pruned <- prune(rf,cp = min.xerror) 

train.pred.rtree.p <- predict(rt.pruned,tr_imputed)

#test.pred.rtree.p <- predict(train.pred.rtree.p,Test_df)

test.pred.rtree.p <- predict(rt.pruned,ts_imputed)

cbind(ts,test.pred.rtree.p)->output3
names(ts)

#View(output)

output3[,-c(2:17)]->output1

names(output1)

#View(output1)
#write.csv()

write.csv(x = output1,file = "jreddy3.csv",row.names=F)








