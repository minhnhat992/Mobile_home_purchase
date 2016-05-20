
#import the data
test_set <- read.csv("D:/Drive/Data Warehousing/GROUP ASSIGNMENT/test_set.txt",
                     header = TRUE, sep = ",") 
#view the data (V in uppercase)
#View(test_set)

#see if any missing data
is.na(test_set)
sum(is.na(test_set))
test_set$subtype <- factor(test_set$subtype,levels = 1:41,ordered = FALSE)
test_set$cust_type <- factor(test_set$cust_type,levels = 1:10, ordered = FALSE)
test_set$ave_age <- ordered(test_set$ave_age, levels = 1:6)
test_set$houses <- ordered(test_set$houses, levels = 1:10)
test_set$household_size <- ordered(test_set$household_size, levels = 1:6)
#trainning_set[6:85] <- lapply(trainning_set[6:85], ordered)
#trainning_set[65:85] <- lapply(trainning_set[65:85], as.integer)#Hmisc package
test_set$mobile_home_insurance <- factor(test_set$mobile_home_insurance, ordered = FALSE)

test_set[6:42] <- lapply(test_set[6:42], ordered, levels = 0:9)
test_set[44:85]<- lapply(test_set[44:64],ordered, levels = 0:9)
test_set$purchase_power <- factor(test_set$purchase_power, levels = 1:8, ordered = TRUE)
str(test_set)

test_set[,ordial_tab_3] <- list(NULL)
test_set$subtype<- NULL
test_set$cust_type <- NULL




#setNA
id <- which(!(test_set[1:29] %in% levels(trainning_set_bal[1:29])))

#predict
pred <- predict(logit_bal, newdata = test_set, type = "response")
fitted_results <- ifelse(pred>0.5,1,0)
misClasifierror <- mean(fitted_results != test_set$mobile_home_insurance)
print(paste('accuracy',1- misClasifierror))

id <- which(!(train_set[1:60] %in% levels(valid_set[1:60])))
valid_set[1:60][id] <- NA