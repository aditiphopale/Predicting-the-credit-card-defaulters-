getwd()
setwd("C:/Users/Aditi/Desktop/Aditi/Spring 2019/Data Mining 1/DMProject")

#Importing packags
install.packages("C50")
library(C50)# For decision tree induction classification
install.packages("magrittr")
library(magrittr)
install.packages("tree")
library(tree)
library(dplyr)			# For data manipulation
library(ggplot2)		# For plotting
library(gridExtra)
install.packages("caret")
library(caret)		# for data-preprocessing & for data-partitioning
install.packages("FastKNN")
library(FastKNN)
install.packages("forcats")
library(forcats)    # For feature reordering in ggplot
library(readr)
install.packages("funModeling")
library(funModeling)  # For plot_num,df_status & correlation_table
library(reshape2)     # Required to melt the correlation matrix
library(pROC)         # Draw roc graph & calculate AUC
library(stringi)
library(reshape)
library(ggplot2)
install.packages("ROSE")
library(ROSE)
library(caret)
install.packages("corrplot")
library(corrplot)

#Loading the data
credit_card<-read.csv("UCI_Credit_Card.csv")
credit_card.df<-data.frame(credit_card[,2:25])
credit_card.df

str(credit_card.df)
summary(credit_card.df)
dim(credit_card.df)



#To find the correlation
credit_card_quants.df<-credit_card.df[c(2,6,13:24)]
credit_card_corr<-cor(credit_card_quants.df,use="pairwise.complete.obs",method="pearson")
round(credit_card_corr,3)

#Correlation Matrix
credit_card_corr_matrix <- cor(subset(credit_card.df, select = c(LIMIT_BAL,BILL_AMT1,BILL_AMT2,BILL_AMT3,BILL_AMT4,BILL_AMT5,PAY_AMT1,PAY_AMT2,PAY_AMT3,PAY_AMT4,PAY_AMT5,PAY_AMT6)))
corrplot(credit_card_corr_matrix, method="number")

#HeatMap
# Change some variables back to numeric
credit_card.df$SEX <- as.numeric(credit_card.df$SEX)
credit_card.df$EDUCATION <- as.numeric(credit_card.df$EDUCATION)
credit_card.df$MARRIAGE <- as.numeric(credit_card.df$MARRIAGE)

cor_matrix<-cor(select_if(credit_card.df, is.numeric))   # select_if() is a dplyr function
melted_cor_matrix = melt(cor_matrix) # melt is a function of package reshape

options(repr.plot.width=15, repr.plot.height=15)
ggplot(data = melted_cor_matrix, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()+
  scale_fill_gradient2(low = "yellow", high = "red") +
  labs(title = "Correlation Matrix", x = "Numeric Variables", y = "Numeric Variables", fill = " Correlation Coefficient") +
  theme(axis.text.x = element_text(angle = 90))
options(repr.plot.width=9, repr.plot.height=7)


#Target Variable count
nrow(credit_card.df)
table(credit_card.df$default.payment.next.month)

#Converting target variable into Paid On-Time or Defaulters
credit_card.df$default.payment.next.month <- as.factor(credit_card.df$default.payment.next.month)
levels(credit_card.df$default.payment.next.month) <- c("Paid On-Time" , "Defaulters")

#Plotting the target variable
ggplot(credit_card.df, aes(x=default.payment.next.month, fill = default.payment.next.month)) + 
  geom_bar() + 
  labs(title="Bar Plot", 
       subtitle="Clients Grouped based on the Target Variable",
       x="Customer Payments", y="Number of Customers",
       fill="Customer Payments")


#Data Preparation

#Testing N/A values
sum(is.na(credit_card.df))


#Testing for outliers
# Box plot of the features, Interpretation: Median difference for default payments of Yes and No for limit_bal
featurePlot(x = credit_card.df[, c(7)], 
            y = credit_card.df$default.payment.next.month, 
            plot = "box",
            auto.key = T)

# Education values 5 and 6 mean unknown. So for the records where it is 0, they can be changed to 5
# At the same time, value 6 is also unknown, change that as well to remove one level
unique(credit_card.df$EDUCATION)
credit_card.df$EDUCATION[credit_card.df$EDUCATION == 0 | credit_card.df$EDUCATION == 6] <- 5
unique(credit_card.df$EDUCATION)

# Marriage value 3 is others. We can change 0s to a new value 4 = Unknown
unique(credit_card.df$MARRIAGE)
credit_card.df$MARRIAGE[credit_card.df$MARRIAGE == 0] <- 4
unique(credit_card.df$MARRIAGE)

str(credit_card.df)
credit_card.df$MARRIAGE<-as.factor(credit_card.df$MARRIAGE)
credit_card.df$SEX<-as.factor(credit_card.df$SEX)
credit_card.df$EDUCATION<-as.factor(credit_card.df$EDUCATION)
credit_card.df$PAY_0<-as.factor(credit_card.df$PAY_0)
credit_card.df$PAY_2<-as.factor(credit_card.df$PAY_2)
credit_card.df$PAY_3<-as.factor(credit_card.df$PAY_3)
credit_card.df$PAY_4<-as.factor(credit_card.df$PAY_4)
credit_card.df$PAY_5<-as.factor(credit_card.df$PAY_5)
credit_card.df$PAY_6<-as.factor(credit_card.df$PAY_6)
credit_card.df$default.payment.next.month<-as.factor(credit_card.df$default.payment.next.month)

str(credit_card.df)

#Cleaning all PAY_ values

#Cleaning PAY_0
unique(credit_card.df$PAY_0)

#Checking for -2 values
PAY_0_minustwo <- credit_card.df %>% filter(PAY_0 == -2)
nrow(PAY_0_minustwo)
table(PAY_0_minustwo$default.payment.next.month)

#Removing -2 values
unique(credit_card.df$PAY_0)
credit_card.df<-credit_card.df[!(credit_card.df$PAY_0==-2),]
unique(credit_card.df$PAY_0)

#Checking for -2 values after changing
PAY_0_minustwo <- credit_card.df %>% filter(PAY_0 == -2)
nrow(PAY_0_minustwo)
nrow(credit_card.df)

#Checking for 0 values
PAY_0_zero <- credit_card.df %>% filter(PAY_0 == 0)
nrow(PAY_0_zero)

table(PAY_0_zero$default.payment.next.month)

#Changing 0 to -1 values
unique(credit_card.df$PAY_0)
credit_card.df$PAY_0[credit_card.df$PAY_0 == 0] <- -1
unique(credit_card.df$PAY_0)

#Checking for 0 values after changing
PAY_0_zero <- credit_card.df %>% filter(PAY_0 == 0)
nrow(PAY_0_zero)


#CLEANING PAY_2
unique(credit_card.df$PAY_2)

#Checking for -2 values
PAY_2_minustwo <- credit_card.df %>% filter(PAY_2 == -2)
nrow(PAY_2_minustwo)
table(PAY_2_minustwo$default.payment.next.month)

#Removing -2 values
unique(credit_card.df$PAY_2)
credit_card.df<-credit_card.df[!(credit_card.df$PAY_2==-2),]
unique(credit_card.df$PAY_2)

#Checking for -2 values after changing
PAY_2_minustwo <- credit_card.df %>% filter(PAY_2 == -2)
nrow(PAY_2_minustwo)
nrow(credit_card.df)

#Checking for 0 values
PAY_2_zero <- credit_card.df %>% filter(PAY_2 == 0)
nrow(PAY_2_zero)
table(PAY_2_zero$default.payment.next.month)

#Changing 0 to -1 values
unique(credit_card.df$PAY_2)
credit_card.df$PAY_2[credit_card.df$PAY_2 == 0] <- -1
unique(credit_card.df$PAY_2)

#Checking for 0 values after changing
PAY_2_zero <- credit_card.df %>% filter(PAY_2 == 0)
nrow(PAY_2_zero)

#CLEANING PAY_3
unique(credit_card.df$PAY_3)

#Checking for -2 values
PAY_3_minustwo <- credit_card.df %>% filter(PAY_3 == -2)
nrow(PAY_3_minustwo)
table(PAY_3_minustwo$default.payment.next.month)

#Removing -2 values
unique(credit_card.df$PAY_3)
credit_card.df<-credit_card.df[!(credit_card.df$PAY_3==-2),]
unique(credit_card.df$PAY_3)

#Checking for -2 values after changing
PAY_3_minustwo <- credit_card.df %>% filter(PAY_3 == -2)
nrow(PAY_3_minustwo)
nrow(credit_card.df)

#Checking for 0 values
PAY_3_zero <- credit_card.df %>% filter(PAY_3 == 0)
nrow(PAY_3_zero)
table(PAY_3_zero$default.payment.next.month)

#Changing 0 to -1 values
unique(credit_card.df$PAY_3)
credit_card.df$PAY_3[credit_card.df$PAY_3 == 0] <- -1
unique(credit_card.df$PAY_3)

#Checking for 0 values after changing
PAY_3_zero <- credit_card.df %>% filter(PAY_3 == 0)
nrow(PAY_3_zero)

#CLEANING PAY_4
unique(credit_card.df$PAY_4)

#Checking for -2 values
PAY_4_minustwo <- credit_card.df %>% filter(PAY_4 == -2)
nrow(PAY_4_minustwo)
table(PAY_4_minustwo$default.payment.next.month)

#Removing -2 values
unique(credit_card.df$PAY_4)
credit_card.df<-credit_card.df[!(credit_card.df$PAY_4==-2),]
unique(credit_card.df$PAY_4)

#Checking for -2 values after changing
PAY_4_minustwo <- credit_card.df %>% filter(PAY_4 == -2)
nrow(PAY_4_minustwo)
nrow(credit_card.df)

#Checking for 0 values
PAY_4_zero <- credit_card.df %>% filter(PAY_4 == 0)
nrow(PAY_4_zero)
table(PAY_4_zero$default.payment.next.month)

#Changing 0 to -1 values
unique(credit_card.df$PAY_4)
credit_card.df$PAY_4[credit_card.df$PAY_4 == 0] <- -1
unique(credit_card.df$PAY_4)

#Checking for 0 values after changing
PAY_4_zero <- credit_card.df %>% filter(PAY_4 == 0)
nrow(PAY_4_zero)

#CLEANING PAY_5
unique(credit_card.df$PAY_5)

#Checking for -2 values
PAY_5_minustwo <- credit_card.df %>% filter(PAY_5 == -2)
nrow(PAY_5_minustwo)
table(PAY_5_minustwo$default.payment.next.month)

#Removing -2 values
unique(credit_card.df$PAY_5)
credit_card.df<-credit_card.df[!(credit_card.df$PAY_5==-2),]
unique(credit_card.df$PAY_5)

#Checking for -2 values after changing
PAY_5_minustwo <- credit_card.df %>% filter(PAY_5 == -2)
nrow(PAY_5_minustwo)
nrow(credit_card.df)

#Checking for 0 values
PAY_5_zero <- credit_card.df %>% filter(PAY_5 == 0)
nrow(PAY_5_zero)
table(PAY_5_zero$default.payment.next.month)

#Changing 0 to -1 values
unique(credit_card.df$PAY_5)
credit_card.df$PAY_5[credit_card.df$PAY_5 == 0] <- -1
unique(credit_card.df$PAY_5)

#Checking for 0 values after changing
PAY_5_zero <- credit_card.df %>% filter(PAY_5 == 0)
nrow(PAY_5_zero)

#No of records after removing -2 and changing 0 to -1
nrow(credit_card.df)

#DATA PARTITIONING
set.seed(1)
#splitting in 70-30
train_partition<-sample(c(1:dim(credit_card.df)[1]),dim(credit_card.df)[1]*0.7)
training_data<-credit_card.df[train_partition,]
test_data<-credit_card.df[-train_partition,]

table(training_data$default.payment.next.month)
table(test_data$default.payment.next.month)


#ROSE() function

#Comment this line anyway
#data.rose <- ROSE(cls ~ ., data = hacide.training_data, seed = 1)$data

#training_data.rose<-ROSE(default.payment.next.month~.,data=training_data,seed=1)$data
#table(training_data.rose$default.payment.next.month)

#test_data.rose<-ROSE(default.payment.next.month~.,data=test_data,seed=1)$data
#table(test_data.rose$default.payment.next.month)

#dim(training_data.rose)
#dim(test_data.rose)

#str(training_data.rose)

#Following 5 lines of code were already commented
#data_drop <- droplevels(data)
#training_data.rose<-droplevels(training_data.rose)
#test_data.rose<-droplevels(test_data.rose)
#str(training_data.rose)
#str(test_data.rose)

library(MASS)
library(dplyr)
library(caret)
library(class)
library(knnp)

#KNN

pred.knn <- knn(training_data, test_data, training_data$default.payment.next.month, k = 1)
table(pred.knn, crim01.test)
mean(pred.knn != crim01.test)

#FastKNN
credit_card_knn<- knnExtract(
  xtr = data.matrix(training_data[,-21]),          
  ytr = training_data[,21],
  xte = data.matrix(test_data[,-21]),
  k = 3
)






#LOGISTIC REGRESSION

#run logistic regression model
logreg<-glm(default.payment.next.month~., data= training_data, family ='binomial' )
summary(logreg)
#predict with training data
predict1<-predict(logreg, training_data, type = 'response')
head(predict1)
#check for accuracy and error percentage
pr1<-ifelse(predict1>0.5,1,0)
table_1<-table(predicted=pr1, Actual=training_data$default.payment.next.month)
table_1
error1 <- 1-sum(diag(table_1))/sum(table_1)
acc1 <- sum(diag(table_1))/sum(table_1)
plot(training_data$default.payment.next.month, predict1)

#predict with test data
predict2<-predict(logreg, test_data, type = 'response')
head(predict2)
#check for accuracy and error percentage
pr2<-ifelse(predict2>0.5,1,0)
table_2<-table(predicted=pr2, Actual=test_data$default.payment.next.month)
table_2
error2 <- 1-sum(diag(table_2))/sum(table_2)
acc2 <- sum(diag(table_2))/sum(table_2)
plot(training_data$default.payment.next.month, predict1)



#DECISION TREE


install.packages("rpart")
install.packages("e1071")
install.packages("rpart.plot")
install.packages("partykit")
install.packages("ipred")
library(rpart.plot)
library(e1071)
library(rpart)


install.packages("rpart")
library(rpart)


set.seed(1)
dectree <- train(default.payment.next.month ~ .,
                 data = training_data,
                 method = "rpart")
dectree
rpart.plot(dectree$finalModel)

dectree.pred <- ModelPredictEvaluateCompareTable(dectree, training_data, test_data, "CART")

#DecisionTree
install.packages("tree")
library(tree)
set.seed(1)
dectree <- train(default.payment.next.month ~ .,
                 data = training_data,
                 method = "rpart")
dectree
rpart.plot(dectree$finalModel)
#long command
summary(dectree$finalModel)

#Old code try
#decision trees
#Partition
#diabetes.df$Outcome<-as.factor(diabetes.df$Outcome)
#set.seed(100)
#diabetes_partitioning<-createDataPartition(y= diabetes.df$Outcome, p = 0.7, list = FALSE)
#training_data<-diabetes.df[diabetes_partitioning,]
#diabetes_validation<-diabetes.df[-diabetes_partitioning,]

#Creating decision trees
ndectree<-tree(default.payment.next.month ~ ., data = training_data)
summary(ndectree)
ndectree

#plotting decision trees
plot(ndectree)
text(ndectree,pretty = 5)
par(mfrow=c(1,1))
ndectree_rpart_tree<-rpart(default.payment.next.month ~ .,data= training_data,cp=0.01)
rpart.plot(ndectree_rpart_tree,box.palette = "RdBu",shadow.col="gray",nn=TRUE)

#validation data decision tree
ndectree_prediction<-predict(ndectree,newdata=test_data, type = "class")
confusionMatrix(ndectree_prediction,test_data$default.payment.next.month)



#dectree.pred <- ModelPredictEvaluateCompareTable(dectree, training_data, test_data, "CART")






#FORWARD LOGISTIC REGRESSION


model_log.fit<-glm(default.payment.next.month~.,data=training_data.rose,family="binomial")

summary(model_log.fit)
model_log.fit


model_log.fit$xlevels[["PAY_4"]] <- union(model_log.fit$xlevels[["PAY_4"]], levels(test_data.rose$PAY_4))

p_1<-predict(model_log.fit,test_data.rose,type='response')

pred1<-ifelse(p_1>0.5,1,0)
tab1<-table(prediction=pred1, Actual=test_data.rose$default.payment.next.month)

accuracy_logit_test_data<-sum(diag(tab1))/sum(tab1)
accuracy_logit_test_data

error_logit_test_data<-1-sum(diag(tab1))/sum(tab1)
error_logit_test_data

confusionMatrix(ifelse(p_1>0.5,1,0),test_data.rose$default.payment.next.month)




#BACKWARD LOGISTIC REGRESSION


model_log.fit2<-glm(default.payment.next.month~.,data=training_data.rose,family="binomial") %>%
  stepAIC(trace=FALSE,direction = "backward")

summary(model_log.fit2)
model_log.fit2

model_log.fit2$xlevels[["PAY_4"]] <- union(model_log.fit2$xlevels[["PAY_4"]], levels(test_data.rose$PAY_4))

p_2<-predict(model_log.fit2,test_data.rose,type='response')

pred2<-ifelse(p_2>0.5,1,0)
tab2<-table(prediction=pred2, Actual=test_data.rose$default.payment.next.month)

accuracy_logit_test_data<-sum(diag(tab2))/sum(tab2)
accuracy_logit_test_data

error_logit_test_data<-1-sum(diag(tab2))/sum(tab2)
error_logit_test_data

confusionMatrix(ifelse(p_2>0.5,1,0),test_data.rose$default.payment.next.month)


#FORWARD AND BACKWARD LOGISTIC REGRESSION MODEL

model_log.fit3<-glm(default.payment.next.month~.,data=training_data.rose,family="binomial") %>%
  stepAIC(trace=FALSE,direction = "both")

summary(model_log.fit3)
model_log.fit3

model_log.fit3$xlevels[["PAY_4"]] <- union(model_log.fit3$xlevels[["PAY_4"]], levels(test_data.rose$PAY_4))

p_3<-predict(model_log.fit3,test_data.rose,type='response')

pred3<-ifelse(p_3>0.5,1,0)
tab3<-table(prediction=pred3, Actual=test_data.rose$default.payment.next.month)

accuracy_logit_test_data<-sum(diag(tab3))/sum(tab3)
accuracy_logit_test_data

error_logit_test_data<-1-sum(diag(tab3))/sum(tab3)
error_logit_test_data

confusionMatrix(ifelse(p_3>0.5,1,0),test_data.rose$default.payment.next.month)

str(credit_card.df_new)

install.packages("factoextra")
library(factoextra)
data_pca <- transform(credit_new[c(1,5,12:20)])
str(data_pca)

all_pca <- prcomp(data_pca[,-1], cor=TRUE, scale = TRUE)
summary(all_pca)

chisq.test_data(credit_new$default.payment.next.month,credit_new$LIMIT_BAL,correct = FALSE)
chisq.test_data(credit_new$default.payment.next.month,credit_new$AGE,correct = FALSE)

