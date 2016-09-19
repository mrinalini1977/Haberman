library(readr)
###importing the data set###
habermanData <- read.csv("C:\\Jigsaw\\R\\Haberman\\HabermanData.csv", header=TRUE)

###Exploring the data###
str(habermanData)

summary(habermanData)

#box plots 

boxplot(Patient_Age~Survival, data=habermanData)

boxplot(Op_Year~Survival, data=habermanData)

boxplot(Pos_Axillary_Nodes~Survival, data=habermanData)

boxplot(habermanData$pos_axillary_nodes,data=habermanData)

boxplot(habermanData$Patient_Age, data=habermanData)

boxplot(habermanData$Op_Year, data=habermanData)

boxplot(habermanData$Pos_Axillary_Nodes, data=habermanData)

# Distribution of Data with survival rate > 5 and survival rate <= 5
# 1 - Patient Survived >= 5
# 2 - Patient Survived < 5

#Actual Count
table(habermanData$Survival)

#Proportion
round(prop.table(table(habermanData$Survival)),2)
round(table(habermanData$Survival)/306,2)


###Data Preparation###


#Split the data into training and validation samples

index <- sample(nrow(habermanData), 0.70 * nrow(habermanData), replace = FALSE)

train <- habermanData[index,]
test <- habermanData[-index,]

# The response variable can only take the values 0 and 1.
# Update the train sample to make Patient Survived < 5 as 1
# Survived >= 5 as 0

# 1 = patient survived < 5 
# 0 = patient survived >= 5

train$Survival[train$Survival==1] <- 0
train$Survival[train$Survival==2] <- 1

table(train$Survival)
round(table(train$Survival)/214,2)

table(test$Survival)
round(table(test$Survival)/92,2)


###Building the Model###

hb_model <- glm(Survival~Patient_Age + Op_Year + Pos_Axillary_Nodes, data = train, family = "binomial")

# For alpha level 0.05, only pos_axillary_nodes is significant. 
# op_year and patient_age are not significant.

summary(hb_model)

#confidence interval
confint(hb_model)

exp(confint(hb_model))

#Plotting the S curve

names(hb_model)

predicted <- hb_model$fitted.values

range(habermanData$Pos_Axillary_Nodes)

xnodes <- seq(0,60,5)

ynodes <- predict(hb_model, list(Pos_axillary_nodes = xnodes),type="response")

plot(habermanData$pos_axillary_nodes,habermanData$Survival, pch = 16, xlab = "POS_AXILLARY_NODES", ylab = "Survival")

lines(xnodes, ynodes)

#predicting the probability of survival with value 2 in validation data set.

predictTest <- predict(hb_model, test, type="response")


#creating confusion matric

predictTestbkt <- ifelse(predictTest>0.5, "G", "B")

table(predictTestbkt, test$Survival)

correctPredictionRatio_test <- (71+3)/(71+16+2+3)

correctPredictionRatio_test

# ROC curve and computing AUC
library(ROCR)

#among other things prediction calculates cutoffs, tpr, fpr, auc
pred <- prediction(predictTest, test$Survival)
perf<- performance(pred, "tpr", "fpr")
plot(perf)

#AUC

auc <- performance(pred,"auc")
auc<- unlist(slot(auc,"y.values"))
auc


