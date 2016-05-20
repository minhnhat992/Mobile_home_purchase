#how to change file : set working directory (setwd) then use file.rename
library(Deducer)
library(psych)
library(ROSE)
library(caret)
library(Hmisc)
library(dplyr)# for %>% 
library(data.table)
library(sqldf)
library(gdata)
library(utils)
library(car)# for multicollinearity
library(xlsx)# output file to excel
library(MASS)#chi-square test of independence
library(polycor)#polychoric correlation for ordial vs ordial
#find perl path
prl <- "D:/PERL/bin/perl.exe"


#import the data
trainning_set <- read.csv("D:/Drive/Data Warehousing/GROUP ASSIGNMENT/trainning_set.txt", 
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

# since subtype and custype are nomial categorical variables (values not in order)
#and ones with code table are ordial (ranked numerical categorical variables)
#while ones with number of are discrete
trainning_set$subtype <- factor(trainning_set$subtype,levels = 1:41,ordered = FALSE)
trainning_set$cust_type <- factor(trainning_set$cust_type,levels = 1:10, ordered = FALSE)
#trainning_set$ave_age <- ordered(trainning_set$ave_age, levels = 1:6)
#trainning_set$houses <- ordered(trainning_set$houses, levels = 1:10)
#trainning_set$household_size <- ordered(trainning_set$household_size, levels = 1:6)
#trainning_set[6:85] <- lapply(trainning_set[6:85], ordered)
#trainning_set[65:85] <- lapply(trainning_set[65:85], as.integer)#Hmisc package
#trainning_set$mobile_home_insurance <- factor(trainning_set$mobile_home_insurance, ordered = FALSE)

#trainning_set[6:42] <- lapply(trainning_set[6:42], ordered, levels = 0:9)
#trainning_set[44:64]<- lapply(trainning_set[44:64],ordered, levels = 0:9)
#trainning_set$purchase_power <- factor(trainning_set$purchase_power, levels = 1:8, ordered = TRUE)
#str(trainning_set)
trainning_set[c(2:4,6:85)] <- lapply(trainning_set[c(2:4,6:85)], as.integer)


#split 
set.seed(1234)
splitindex <- createDataPartition(trainning_set$mobile_home_insurance, 
                                  p = 0.8, 
                                  list = FALSE)
train_set <- trainning_set[splitindex,]
valid_set <- trainning_set[-splitindex,]

#balance data set
## before
print(prop.table(table(trainning_set$mobile_home_insurance))*100)
##balance data
trainning_set_bal <- ovun.sample(mobile_home_insurance~., 
                                 data = train_set, 
                                 p = 0.9, 
                                 seed = 1,
                                 method = "both")$data
##after
print(prop.table(table(trainning_set_bal$mobile_home_insurance))*100)


#perform exploratory data analysis
#discrete vs discrete : correaltion (results : most variables are not highly correlated)
#discrete_tab <- trainning_set_bal[2:3,65:85]
#discrete_tab <- cor(discrete_tab, method = "pearson")%>% 
#  findCorrelation(cutoff = 0.5, 
#                  names = TRUE,
#                  exact = TRUE,
#                  verbose = TRUE) %>% 
#  print()

#dis_tab <- trainning_set[c(2:3, 65:85)]
#dis_tab[,discrete_tab] <- list(NULL)

#write.xlsx(discrete_tab, "D:/discrete.xlsx")
#nomial vs nomial : chi-square test of independence
chi_test <- chisq.test(trainning_set_bal$subtype, trainning_set_bal$cust_type, simulate.p.value = TRUE) %>% 
  print()#p value < 2.2e -16, means rejecting null hypothesis = these two variables are not independent
#ordial vs oridal :polychoric correlation
ordial_tab <- trainning_set_bal[c(2:4,6:85)]
ordial_tab<-data.frame(lapply(ordial_tab, as.numeric))
ordial_tab_2 <- cor(ordial_tab, method = "spearman")
ordial_tab_3 <- findCorrelation(ordial_tab_2, 
                                cutoff = 0.8,
                                names = TRUE, 
                                exact =TRUE, 
                                verbose = TRUE) %>%
  print()
#ord_tab <- trainning_set[c(4,6:64)]
#ord_tab[,ordial_tab_3] <- list(NULL)
#nomial vs ordial
#nomial Vs discrete : anova
#ordial vs discrete: 
#ord_vs_dis <- biserial(y = dis_tab, x = ord_tab)


#remove variables
#fulldrop <- c(as.vector(discrete_tab),
#              as.vector(ordial_tab_3),
#              "subtype")
#print(fulldrop)

#trainning_set_bal[,fulldrop] <- list(NULL)

trainning_set_bal[,ordial_tab_3] <- list(NULL)
trainning_set_bal$subtype<- NULL
trainning_set_bal$cust_type <- NULL

valid_set[,ordial_tab_3] <- list(NULL)
valid_set$subtype<- NULL
valid_set$cust_type <- NULL

test_set[,"mobile_home_insurane"] <- NA


##logistic regression

logit_bal <- glm(mobile_home_insurance~.,
                 data = trainning_set_bal,
                 family = "binomial")


summary_logit_bal <- summary(logit_bal) %>% 
  print()
#r-squared
library(pscl)
pR2(logit_bal)

library(Deducer)
ROC <- rocplot(logit_bal, pred.prob.labels = TRUE, AUC = TRUE)
ROC
prob <- predict(logit_bal,
                newdata = test_set,
                type = "response",
                allow.new.levels = TRUE)
pred <- prediction(prob,test_set$monile_home_insurance)
auc <- performance(pred, measure = "auc")
f1 <- roc(logit_bal, data = test_set)

id <- which(!(valid_set$houses %in% levels(trainning_set_bal$houses)))
valid_set$houses[id] <- NA
