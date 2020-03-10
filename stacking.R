#Model Stacking
rm(list = ls())
# Load libraries
# 2 layer stacking
library(mlbench)
library(caret)
library(caretEnsemble)
set.seed(20)

#read data
train_data<-read.csv("train.csv")
test_data <- read.csv("test.csv")

dataFolds <- createFolds(train_data$y,5)
pred_RF_id_1 <- dataFolds$Fold1
pred_RF_id_2 <- dataFolds$Fold2
pred_RF_id_3 <- dataFolds$Fold3
pred_RF_id_4 <- dataFolds$Fold4
pred_RF_id_5 <- dataFolds$Fold5

# fold1<-train_data[dataFolds$Fold1,]
# fold2<-train_data[dataFolds$Fold2,]
# fold3<-train_data[dataFolds$Fold3,]
# fold4<-train_data[dataFolds$Fold4,]
# fold5<-train_data[dataFolds$Fold5,]

#first layer stacking
#Random Forrest
#fold 1 as test data
data_process_RF<-function(x){
  train <- x
  age <- train$age
  #breaks <- c(10, 20, 30, 40, 50, 60, 70,150)
  #age.cut <- cut(age, breaks, right=FALSE)
  #age<-as.factor(age.cut)
  #age<-ifelse(age=="[10,20)",0,age)
  age <- data.frame(age) 
  duration <- train$duration
  #breaks <- c(0, 60, 120, 180, 240, 300, 1200,5000)
  #duration.cut = cut(duration, breaks, right=FALSE)
  #duration<-as.factor(duration.cut)
  #duration<-ifelse(duration=="[0,60)",0,duration)
  duration <- data.frame(duration)
  camp <- train$campaign
  camp <- ifelse(camp>=8,8,camp)
  camp <- data.frame(camp)
  var<-train$emp.var.rate
  #var<-as.factor(var)
  #breaks <- c(-5, -2, -0.5, 0.5, 5)
  #var.cut = cut(var, breaks, right=FALSE)
  var<-data.frame(var)
  y<-train$y
  y<-ifelse(y=="yes",1,0)
  y<-as.factor(y)
  
  job<-train$job 
  job<-as.factor(job)
  #job<-ifelse(job=="admin.",0,job)
  job<-data.frame(job)
  
  edu<-train$education
  edu<-as.factor(edu)
  #edu<-ifelse(edu=="basic.4y",0,edu)
  edu<-data.frame(edu)
  
  marital<-train$marital
  marital<-as.factor(marital)
  #marital<-ifelse(marital=="divorced",0,marital)
  marital<-data.frame(marital)
  
  default<-train$default 
  default<-as.factor(default)
  #default<-ifelse(default=="no",0,default)
  default<-data.frame(default)
  
  housing<-train$housing 
  housing<-as.factor(housing)
  #housing<-ifelse(housing=="no",0,housing)
  housing<-data.frame(housing)
  
  loan<-train$loan 
  loan<-as.factor(loan)
  #loan<-ifelse(loan=="no",0,loan)
  loan<-data.frame(loan)
  
  contact<-train$contact 
  contact<-as.factor(contact)
  #contact<-ifelse(contact=="cellular",0,contact)
  contact<-data.frame(contact)
  
  month<-train$month
  month<-as.factor(month)
  #month<-ifelse(month=="apr",0,month)
  month<-data.frame(month)
  
  pdays<-train$pdays
  #pdays<-ifelse(pdays==999,0,1)
  pdays<-data.frame(pdays)
  
  dw<-train$day_of_week
  dw<-as.factor(dw)
  #dw<-ifelse(dw=="fri",0,dw)
  dw<-data.frame(dw)
  
  previous<-train$previous 
  previous<-ifelse(previous>0,1,0)
  previous<-data.frame(previous)
  
  poutcome<-train$poutcome 
  #poutcome<-ifelse(poutcome=="failure",0,poutcome)
  poutcome<-data.frame(poutcome)
  
  cpi<-train$cons.price.idx
  #cpi<-as.factor(cpi)
  #cpi<-ifelse(cpi=="92.201",0,cpi)
  cpi<-data.frame(cpi)
  
  conf<-train$cons.conf.idx
  conf<-data.frame(conf)
  
  euribor<-train$euribor3m
  euribor<-data.frame(euribor)
  
  employed<-train$nr.employed
  employed<-data.frame(employed)
  
  merge<-cbind(job,month,camp,previous,contact,age,edu,pdays,duration,poutcome,euribor,y)
  return(merge)
}

whole_train<- data_process_RF(train_data)
whole_test<- data_process_RF(test_data)
# seperate_train_test <-function(x){
#   test_RF<<-whole_train[x,]
#   train_RF <<-whole_train[-x,]
# }
# 
# seperate_train_test(dataFolds$Fold1)

pred_RF<-function(x){
  test_RF<<-whole_train[x,]
  train_RF <<-whole_train[-x,]

  num_of_yes <- sum(train_RF$y ==1)
  library("caret")
  
  training_data_yes <-train_RF[train_RF$y==1, ]
  training_data_no <- train_RF[train_RF$y==0, ] 
  
  num_of_yes <- num_of_yes *3
  library("plyr")
  indices <- sample(nrow(training_data_no), num_of_yes)
  training_data_no <- training_data_no[indices, ]
  training_data <- rbind(training_data_yes, training_data_no)
  
  train_RF <- training_data
  y <- training_data$y
  
  #x1 <- subset(test_vector, select=c(-id,-y))
  
  #rfmodel <- randomForest(y ~ ., data=x)
  #print
  
  require(randomForest)
  
  rfmodel <- randomForest(y ~ ., data=train_RF,ntree=500,importance=TRUE)
  
  ###
  pred_RF <- as.data.frame(predict(rfmodel,test_RF))
  pred_test_RF<-as.data.frame(predict(rfmodel,whole_test))
  pred<-c(pred_RF,pred_test_RF)
  return(pred)
}

a<-pred_RF(dataFolds$Fold1)
pred_RF_1<-data.frame(a[1])
colnames(pred_RF_1) <- c("F_RF")
pred_test_1<-data.frame(a[2])
id<-pred_RF_id_1
id<-as.data.frame(id)
pred_RF_1<-cbind(id,pred_RF_1)

a<-pred_RF(dataFolds$Fold2)
pred_RF_2<-data.frame(a[1])
colnames(pred_RF_2) <- c("F_RF")
pred_test_2<-data.frame(a[2])
id<-pred_RF_id_2
id<-as.data.frame(id)
pred_RF_2<-cbind(id,pred_RF_2)

a<-pred_RF(dataFolds$Fold3)
pred_RF_3<-data.frame(a[1])
colnames(pred_RF_3) <- c("F_RF")
pred_test_3<-data.frame(a[2])
id<-pred_RF_id_3
id<-as.data.frame(id)
pred_RF_3<-cbind(id,pred_RF_3)

a<-pred_RF(dataFolds$Fold4)
pred_RF_4<-data.frame(a[1])
colnames(pred_RF_4) <- c("F_RF")
pred_test_4<-data.frame(a[2])
id<-pred_RF_id_1
id<-as.data.frame(id)
pred_RF_4<-cbind(id,pred_RF_4)

a<-pred_RF(dataFolds$Fold5)
pred_RF_5<-data.frame(a[1])
colnames(pred_RF_5) <- c("F_RF")
pred_test_5<-data.frame(a[2])
id<-pred_RF_id_5
id<-as.data.frame(id)
pred_RF_5<-cbind(id,pred_RF_5)

pred_RF_final<-rbind(pred_RF_1,pred_RF_2,pred_RF_3,pred_RF_4,pred_RF_5)
pred_RF_final <- pred_RF_final[order(pred_RF_final$id),]
pred_test_RF<-cbind(pred_test_1,pred_test_2,pred_test_3,pred_test_4,pred_test_5)

a<-as.numeric(pred_test_RF[[1]])-1
b<-as.numeric(pred_test_RF[[2]])-1
c<-as.numeric(pred_test_RF[[3]])-1
d<-as.numeric(pred_test_RF[[4]])-1
e<-as.numeric(pred_test_RF[[5]])-1

pred_test_RF<-(a+b+c+d+e)/5


#Adaboost

data_process_Ada<-function(x){
  train <- x
  age <- train$age
  #breaks <- c(10, 20, 30, 40, 50, 60, 70,150)
  #age.cut <- cut(age, breaks, right=FALSE)
  #age<-as.factor(age.cut)
  #age<-ifelse(age=="[10,20)",0,age)
  age <- data.frame(age) 
  duration <- train$duration
  #breaks <- c(0, 60, 120, 180, 240, 300, 1200,5000)
  #duration.cut = cut(duration, breaks, right=FALSE)
  #duration<-as.factor(duration.cut)
  #duration<-ifelse(duration=="[0,60)",0,duration)
  duration <- data.frame(duration)
  camp <- train$campaign
  camp <- ifelse(camp>=8,8,camp)
  camp <- data.frame(camp)
  var<-train$emp.var.rate
  #var<-as.factor(var)
  #breaks <- c(-5, -2, -0.5, 0.5, 5)
  #var.cut = cut(var, breaks, right=FALSE)
  var<-data.frame(var)
  y<-train$y
  y<-ifelse(y=="yes",1,0)
  y<-as.factor(y)
  
  job<-train$job 
  job<-as.factor(job)
  #job<-ifelse(job=="admin.",0,job)
  job<-data.frame(job)
  
  edu<-train$education
  edu<-as.factor(edu)
  #edu<-ifelse(edu=="basic.4y",0,edu)
  edu<-data.frame(edu)
  
  marital<-train$marital
  marital<-as.factor(marital)
  #marital<-ifelse(marital=="divorced",0,marital)
  marital<-data.frame(marital)
  
  default<-train$default 
  default<-as.factor(default)
  #default<-ifelse(default=="no",0,default)
  default<-data.frame(default)
  
  housing<-train$housing 
  housing<-as.factor(housing)
  #housing<-ifelse(housing=="no",0,housing)
  housing<-data.frame(housing)
  
  loan<-train$loan 
  loan<-as.factor(loan)
  #loan<-ifelse(loan=="no",0,loan)
  loan<-data.frame(loan)
  
  contact<-train$contact 
  contact<-as.factor(contact)
  #contact<-ifelse(contact=="cellular",0,contact)
  contact<-data.frame(contact)
  
  month<-train$month
  month<-as.factor(month)
  #month<-ifelse(month=="apr",0,month)
  month<-data.frame(month)
  
  pdays<-train$pdays
  #pdays<-ifelse(pdays==999,0,1)
  pdays<-data.frame(pdays)
  
  dw<-train$day_of_week
  dw<-as.factor(dw)
  #dw<-ifelse(dw=="fri",0,dw)
  dw<-data.frame(dw)
  
  previous<-train$previous 
  previous<-ifelse(previous>0,1,0)
  previous<-data.frame(previous)
  
  poutcome<-train$poutcome 
  #poutcome<-ifelse(poutcome=="failure",0,poutcome)
  poutcome<-data.frame(poutcome)
  
  cpi<-train$cons.price.idx
  #cpi<-as.factor(cpi)
  #cpi<-ifelse(cpi=="92.201",0,cpi)
  cpi<-data.frame(cpi)
  
  conf<-train$cons.conf.idx
  conf<-data.frame(conf)
  
  euribor<-train$euribor3m
  euribor<-data.frame(euribor)
  
  employed<-train$nr.employed
  employed<-data.frame(employed)
  
  #merge<-cbind(job,month,camp,previous,contact,age,edu,pdays,duration,poutcome,euribor,y)
  #merge<-cbind(duration,employed,euribor,var,cpi,previous,poutcome,edu,age,job,month,camp,pdays,contact,y)
  merge<-cbind(job,month,camp,previous,contact,age,edu,pdays,duration,poutcome,euribor,y)
  return(merge)
}


whole_train<- data_process_Ada(train_data)
whole_test<- data_process_Ada(test_data)
pred_Ada<-function(x){
  test_Ada<<-whole_train[x,]
  train_Ada <<-whole_train[-x,]
  
  num_of_yes <- sum(train_Ada$y ==1)
  library("caret")
  
  training_data_yes <-train_Ada[train_Ada$y==1, ]
  training_data_no <- train_Ada[train_Ada$y==0, ] 
  
  num_of_yes <- num_of_yes *3
  library("plyr")
  indices <- sample(nrow(training_data_no), num_of_yes)
  training_data_no <- training_data_no[indices, ]
  training_data <- rbind(training_data_yes, training_data_no)
  
  train_Ada <- training_data
  y <- training_data$y
  
  library("rpart")
  library("ada")
  require(ada)
  control <- rpart.control(cp = -1, maxdepth = 15,maxcompete = 1,xval = 0)
  Adamodel <- ada(y~., data = train_Ada, type = "gentle", control = control, iter = 100)
  
  ###
  pred_Ada <- as.data.frame(predict(Adamodel,test_Ada))
  pred_test_Ada<-as.data.frame(predict(Adamodel,whole_test))
  pred<-c(pred_Ada,pred_test_Ada)
  return(pred)
}

a<-pred_Ada(dataFolds$Fold1)
pred_Ada_1<-data.frame(a[1])
colnames(pred_Ada_1) <- c("F_Ada")
pred_test_1<-data.frame(a[2])
id<-pred_RF_id_1
id<-as.data.frame(id)
pred_Ada_1<-cbind(id,pred_Ada_1)

a<-pred_Ada(dataFolds$Fold2)
pred_Ada_2<-data.frame(a[1])
colnames(pred_Ada_2) <- c("F_Ada")
pred_test_2<-data.frame(a[2])
id<-pred_RF_id_2
id<-as.data.frame(id)
pred_Ada_2<-cbind(id,pred_Ada_2)

a<-pred_Ada(dataFolds$Fold3)
pred_Ada_3<-data.frame(a[1])
colnames(pred_Ada_3) <- c("F_Ada")
pred_test_3<-data.frame(a[2])
id<-pred_RF_id_3
id<-as.data.frame(id)
pred_Ada_3<-cbind(id,pred_Ada_3)

a<-pred_Ada(dataFolds$Fold4)
pred_Ada_4<-data.frame(a[1])
colnames(pred_Ada_4) <- c("F_Ada")
pred_test_4<-data.frame(a[2])
id<-pred_RF_id_1
id<-as.data.frame(id)
pred_Ada_4<-cbind(id,pred_Ada_4)

a<-pred_Ada(dataFolds$Fold5)
pred_Ada_5<-data.frame(a[1])
colnames(pred_Ada_5) <- c("F_Ada")
pred_test_5<-data.frame(a[2])
id<-pred_RF_id_5
id<-as.data.frame(id)
pred_Ada_5<-cbind(id,pred_Ada_5)

pred_Ada_final<-rbind(pred_Ada_1,pred_Ada_2,pred_Ada_3,pred_Ada_4,pred_Ada_5)
pred_Ada_final <- pred_Ada_final[order(pred_Ada_final$id),]
pred_test_Ada<-cbind(pred_test_1,pred_test_2,pred_test_3,pred_test_4,pred_test_5)

a<-as.numeric(pred_test_Ada[[1]])-1
b<-as.numeric(pred_test_Ada[[2]])-1
c<-as.numeric(pred_test_Ada[[3]])-1
d<-as.numeric(pred_test_Ada[[4]])-1
e<-as.numeric(pred_test_Ada[[5]])-1

pred_test_Ada<-(a+b+c+d+e)/5





#xgboosts

pred_RF<-function(x){
  whole_train<- train_data
  whole_test<- test_data
  
  test_Xgb<<-whole_train[x,]
  train_Xgb <<-whole_train[-x,]

  df_train_Xgb<- data.table(train_Xgb,keep.rownames = F)
  df_train_Xgb<-subset(df_train_Xgb,select= -c(id))
  df_train_Xgb$y<-ifelse(df_train_Xgb$y=="yes",1,0)
  df_train_no <- df_train_Xgb[df_train_Xgb$y==0,]
  df_train_yes <- df_train_Xgb[df_train_Xgb$y==1,]
  num_yes <- sum(df_train_Xgb$y==1)
  num_no <- num_yes * 2
  require("plyr")
  indices <- sample(nrow(df_train_no), num_no)
  df_train_no <- df_train_no[indices, ]
  df_train <- rbind(df_train_yes, df_train_no)
  df_train <- df_train[sample(nrow(df_train)),]
  
  #***************************************************************
  #   Use train data with logistic regression
  #***************************************************************
  # train<-read.csv("train_data_LogReg_1-1.csv")
  # df_train<- data.table(train,keep.rownames = F)
  # df_train<-subset(df_train,select= -c(id))
  # df_train$y<-ifelse(df_train$y=="yes",1,0)
  
  
  #***************************************************************
  cat("Print the dataset\n")
  print(df_train)
  
  #output_vector<-as.numeric(df_train$y)-1
  output_vector<-as.numeric(df_train_Xgb$y)
  #ouput_vector<-as.matrix(output_vector)
  sparse_matrix_train = sparse.model.matrix(y~.-1, data = df_train_Xgb)
  sparse_matrix_test = sparse.model.matrix(y~.-1, data = test_Xgb)
  whole_test$y=1
  sparse_matrix_wholetest = sparse.model.matrix(y~.-1, data = whole_test)

  
  cat("Learning...\n")
  xgb <- xgboost(data=sparse_matrix_train,label=output_vector, max_depth=15,eta=0.14,nthread=50,nrounds=20,objective="binary:logistic",seed=20)
  
  importance <- xgb.importance(feature_names = colnames(sparse_matrix_train), model = xgb)
  print(importance)
  
  
  pred_Xgb<- as.data.frame(ifelse(predict(xgb,sparse_matrix_test)>0.51,1,0))
  pred_test_Xgb<-as.data.frame(ifelse(predict(xgb,sparse_matrix_wholetest)>0.51,1,0))
  pred<-c(pred_Xgb,pred_test_Xgb)
  return(pred)
}

a<-pred_RF(dataFolds$Fold1)
pred_RF_1<-data.frame(a[1])
colnames(pred_RF_1) <- c("F_Xgb")
pred_test_1<-data.frame(a[2])
id<-pred_RF_id_1
id<-as.data.frame(id)
pred_RF_1<-cbind(id,pred_RF_1)

a<-pred_RF(dataFolds$Fold2)
pred_RF_2<-data.frame(a[1])
colnames(pred_RF_2) <- c("F_Xgb")
pred_test_2<-data.frame(a[2])
id<-pred_RF_id_2
id<-as.data.frame(id)
pred_RF_2<-cbind(id,pred_RF_2)

a<-pred_RF(dataFolds$Fold3)
pred_RF_3<-data.frame(a[1])
colnames(pred_RF_3) <- c("F_Xgb")
pred_test_3<-data.frame(a[2])
id<-pred_RF_id_3
id<-as.data.frame(id)
pred_RF_3<-cbind(id,pred_RF_3)

a<-pred_RF(dataFolds$Fold4)
pred_RF_4<-data.frame(a[1])
colnames(pred_RF_4) <- c("F_Xgb")
pred_test_4<-data.frame(a[2])
id<-pred_RF_id_1
id<-as.data.frame(id)
pred_RF_4<-cbind(id,pred_RF_4)

a<-pred_RF(dataFolds$Fold5)
pred_RF_5<-data.frame(a[1])
colnames(pred_RF_5) <- c("F_Xgb")
pred_test_5<-data.frame(a[2])
id<-pred_RF_id_5
id<-as.data.frame(id)
pred_RF_5<-cbind(id,pred_RF_5)

pred_Xgb_final<-rbind(pred_RF_1,pred_RF_2,pred_RF_3,pred_RF_4,pred_RF_5)
pred_Xgb_final <- pred_Xgb_final[order(pred_Xgb_final$id),]
pred_test_Xgb<-cbind(pred_test_1,pred_test_2,pred_test_3,pred_test_4,pred_test_5)

a<-as.numeric(pred_test_Xgb[[1]])-1
b<-as.numeric(pred_test_Xgb[[2]])-1
c<-as.numeric(pred_test_Xgb[[3]])-1
d<-as.numeric(pred_test_Xgb[[4]])-1
e<-as.numeric(pred_test_Xgb[[5]])-1

pred_test_Xgb<-(a+b+c+d+e)/5

train_final<-cbind(pred_RF_final[-1],pred_Ada_final[-1],pred_Xgb_final[-1])
test_final<-cbind(pred_test_RF,pred_test_Ada,pred_test_Xgb)

train_final<-cbind(train_final,train_data$y)



