#how to change file : set working directory (setwd) then use file.rename
library(doParallel)
library(stepPlr)
library(pROC)
library(ROCR)
library(Deducer)
library(psych)
library(ROSE)
library(caret)
library(Hmisc)
library(plyr)
library(dplyr)# for %>% 
library(data.table)
library(sqldf)
library(gdata)
library(utils)
library(car)# for multicollinearity
library(xlsx)# output file to excel
library(MASS)#chi-square test of independence
options(scipen=999)
#find perl path for utils package
prl <- "/PERL/bin/perl.exe"


#import the data
trainning_set <- read.csv("/trainning_set.txt", 
                          header = TRUE, sep = ",")
#View(trainning_set)


#see if any missing data
#is.na(trainning_set)
#sum(is.na(trainning_set))#number of missing values


# see which columns has missing data
#trainning_na_count <- sapply(trainning_set, function(x){
#  sum(is.na(x))
#})
#trainning_na_count <- data.frame(trainning_na_count)

#set data types
trainning_set$subtype <- factor(trainning_set$subtype,levels = 1:41,ordered = FALSE)
trainning_set$cust_type <- factor(trainning_set$cust_type,levels = 1:10, ordered = FALSE)
trainning_set$ave_age <- ordered(trainning_set$ave_age, levels = 1:6)
trainning_set$houses <- ordered(trainning_set$houses, levels = 1:10)
trainning_set$household_size <- ordered(trainning_set$household_size, levels = 1:6)
trainning_set$mobile_home_insurance <- factor(ifelse(trainning_set$mobile_home_insurance == 1,"y","n"))
trainning_set[6:64] <- lapply(trainning_set[6:64], ordered, levels = 0:9)
trainning_set[65:85]<- lapply(trainning_set[65:85], ordered)
trainning_set$purchase_power <- factor(trainning_set$purchase_power, levels = 1:8, ordered = TRUE)
str(trainning_set)


#split trainning set 70:30 ratio
set.seed(1234)
splitindex <- createDataPartition(trainning_set$mobile_home_insurance, 
                                  p = 0.7, 
                                  list = FALSE)
train_set <- trainning_set[splitindex,]
valid_set <- trainning_set[-splitindex,]

#balance data set
## before
print(prop.table(table(trainning_set$mobile_home_insurance))*100)
##balance data
trainning_set_bal <- ovun.sample(mobile_home_insurance~., 
                                 data = train_set, 
                                 p = 0.20,
                                 seed = 1,
                                 method = "both")$data
##after
print(prop.table(table(trainning_set_bal$mobile_home_insurance))*100)


#ordial vs oridal :spearman correlation
ordial_tab <- trainning_set_bal[c(2:4,6:85)]
ordial_tab<-data.frame(lapply(ordial_tab, as.numeric))
ordial_tab_2 <- cor(ordial_tab, method = "spearman")
ordial_tab_3 <- findCorrelation(ordial_tab_2, 
                                cutoff = 0.9,
                                names = TRUE, 
                                exact =TRUE, 
                                verbose = TRUE) %>%
  print()

#trainning_set_bal[,ordial_tab_3] <- list(NULL)
trainning_set_bal$subtype<- NULL
trainning_set_bal$cust_type <- NULL

#valid_set[,ordial_tab_3] <- list(NULL)
valid_set$subtype<- NULL
valid_set$cust_type <- NULL
#establish train parameters
train_control <- trainControl(savePredictions = TRUE,
                              classProbs = TRUE,
                              summaryFunction = twoClassSummary)

#create model based on different methods:
#set time
ptm <- proc.time()
set.seed(1045)
#parallel
cl <- makeCluster(detectCores())
registerDoParallel(cl)
model <- train(data = trainning_set_bal,
               mobile_home_insurance~.,
               trControl = train_control,
               method = "nnet",
               tuneLength = 5,
               preProcess = c("center", "scale","pca"),
               allowParallel = TRUE,
               verbose = TRUE,
               size = 3)
summary(model)
stopCluster(cl)
time <- proc.time() - ptm

#train set performance
probs2 <- predict(model,newdata = trainning_set_bal, type ="prob")
pred2  <- factor(ifelse(probs2[,"y"] > 0.5,"y","n"))
summary(pred2)
matrix1 <- confusionMatrix(data = pred2, 
                           trainning_set_bal$mobile_home_insurance,
                           positive = levels(trainning_set_bal$mobile_home_insurance)[2]) %>% 
  print()
rocCurve1  <- roc(response = trainning_set_bal$mobile_home_insurance,
                  predictor = probs2[,"y"],
                  levels = levels(trainning_set_bal$mobile_home_insurance))
curve11 <- plot(rocCurve1, print.thres = c(.5), type = "S",
                print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
                print.thres.cex = .8,
                legacy.axes = TRUE)
curve11#AUC

#valid_set performance
probs <- predict(model,newdata = valid_set, type ="prob")
pred  <- factor(ifelse(probs[,"y"] > 0.25,"y","n"))
summary(pred)
levels(valid_set$mobile_home_insurance)
matrix <- confusionMatrix(data = pred, 
                          valid_set$mobile_home_insurance,
                          positive = levels(valid_set$mobile_home_insurance)[2]) %>% 
  print()

rocCurve  <- roc(response = valid_set$mobile_home_insurance,
                 predictor = probs[,"y"],
                 levels = levels(valid_set$mobile_home_insurance))
curve1 <- plot(rocCurve, print.thres = c(.25), type = "S",
               print.thres.pattern = "%.3f (Spec = %.2f, Sens = %.2f)",
               print.thres.cex = .8,
               legacy.axes = TRUE)
curve1


#import the test data
test_set <- read.csv("D:/Drive/Data Warehousing/GROUP ASSIGNMENT/test_set.txt",
                     header = TRUE, sep = ",") 
#view the data (V in uppercase)
#View(test_set)

#see if any missing data
#is.na(test_set)
#sum(is.na(test_set))
test_set$subtype <- factor(test_set$subtype,levels = 1:41,ordered = FALSE)
test_set$cust_type <- factor(test_set$cust_type,levels = 1:10, ordered = FALSE)
test_set$ave_age <- ordered(test_set$ave_age, levels = 1:6)
test_set$houses <- ordered(test_set$houses, levels = 1:10)
test_set$household_size <- ordered(test_set$household_size, levels = 1:6)
test_set$mobile_home_insurance <- factor(ifelse(test_set$mobile_home_insurance == 1,"y","n"))
#trainning_set$mobile_home_insurance <- factor(trainning_set$mobile_home_insurance, ordered = FALSE)
test_set[6:64] <- lapply(test_set[6:64], ordered, levels = 0:9)
test_set[65:85]<- lapply(test_set[65:85], ordered)
test_set$purchase_power <- factor(test_set$purchase_power, levels = 1:8, ordered = TRUE)
str(test_set)
#test_set[,ordial_tab_3] <- list(NULL)
test_set$subtype<- NULL
test_set$cust_type <- NULL

for ( j in c(1:83) ) {
  i = which( !( test_set[[j]] %in% levels(trainning_set_bal[[j]] ) ) )
  test_set[i,j] <- NA
}
#test set performance
test_fin <- predict(model,newdata = test_set, type = "prob")
pred_test  <- factor(ifelse(test_fin[,"y"] > 0.25,"y","n"))
summary(pred_test)
