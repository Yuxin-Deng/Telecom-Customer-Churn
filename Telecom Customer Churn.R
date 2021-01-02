#Telco<- read.csv(file = '/Users/yuki0416/Desktop/Intermediate Analytics/Final Project/WA_Fn-UseC_-Telco-Customer-Churn.csv',header = TRUE)
Telco<- read.csv(file = 'qqq.csv',header = TRUE)

#***Preliminary Analysis***
#1) look data type
str(Telco)
#Change SeniorCitizen from int to factor (yes/no)
Telco$SeniorCitizen=as.factor(ifelse(Telco$SeniorCitizen==1, "Yes", "No"))

#2) Find out NA 
colSums(is.na(Telco))
#Remove NA column in TotalCharges
NewTelco <- na.omit(Telco)
sum(is.na(NewTelco))
nrow(NewTelco) #7043-11=7032

#3) Clean the categorical features (PhoneService,InternetService)
#replace column
NewTelco$MultipleLines[which(NewTelco$MultipleLines == "No phone service")] = "No"
for(i in 10:15){
  NewTelco[,i][which(NewTelco[,i] == "No internet service")] = "No"
}
#drop level
NewTelco$MultipleLines <- droplevels(NewTelco$MultipleLines)
for(j in 10:15){
  NewTelco[,j] <- droplevels(NewTelco[,j])
}
#Exanine result
str(NewTelco$MultipleLines)
str(NewTelco[10:15])

#4) Pie chart of Churn 
Churn_y <- sum(NewTelco$Churn=="Yes")
Churn_n <- sum(NewTelco$Churn=="No")
slices <- c(Churn_y,Churn_n)
pct <- round(slices/sum(slices)*100,3)
pie_churn<- data.frame(
  group = c("Yes", "No"),
  value = c(pct)) #create data frame for pie 
#install.packages("ggplot2")
library(ggplot2)
pie = ggplot(pie_churn, aes(x="", y=value, fill=group))+geom_bar(width = 1, stat = "identity")
pie = pie + coord_polar("y", start=0) + geom_text(aes(label =sprintf("%.2f%%", value)), 
                                                  position = position_stack(vjust = 0.5))
pie = pie +scale_fill_brewer(palette="Set2")
pie= pie + labs(x = NULL, y = NULL, fill = "Churn", title = "Pie chart of Churn")
pie

#5) Boxplots for continuous variables (tenure,MonthlyCharges,TotalCharges)
ggplot(NewTelco, aes(y= tenure, x = Churn, fill = Churn))+
  geom_boxplot()+ scale_fill_brewer(palette="Set2") + theme_classic()

ggplot(NewTelco, aes(y= MonthlyCharges, x = Churn, fill = Churn))+
  geom_boxplot()+ scale_fill_brewer(palette="Set2") + theme_classic()

ggplot(NewTelco, aes(y= TotalCharges, x = Churn, fill = Churn))+
  geom_boxplot()+ scale_fill_brewer(palette="Set2") + theme_classic()

#6) Categorical data (gender,SeniorCitizen)
gender_churn = ggplot(NewTelco, aes(gender,fill=Churn))+geom_bar(position = 'fill')+
  scale_fill_brewer(palette="Set2")
gender_churn

SeniorCitizen_churn = ggplot(NewTelco, aes(SeniorCitizen, fill=Churn))+geom_bar(position = 'fill')+
  scale_fill_brewer(palette="Set2")
SeniorCitizen_churn

Contract_churn = ggplot(NewTelco, aes(Contract, fill=Churn))+geom_bar(position = 'fill')+
  scale_fill_brewer(palette="Set2")
Contract_churn

#***ANALYSIS***
#A) HYPOTHESIS TESTING-prop.test
#1) Do male and female samples have the same churn?
# H0: male churn  = female churn
# H1: male churn  != female churn
sum(NewTelco$gender=="Male" )
sum(NewTelco$Churn=="Yes" & NewTelco$gender=="Male" )

sum(NewTelco$gender=="Female" )
sum(NewTelco$Churn=="Yes" & NewTelco$gender=="Female" )

library(MASS)
prop.test( c(930, 939) , c(3549,3483) )


#2) Do senior citizen churn less than non-senior citizen churn?
# H0: senior citizen churn  <= non-senior citizen churn
# H1: senior citizen churn  >  non-senior citizen churn
sum(NewTelco$SeniorCitizen=="Yes" )
sum(NewTelco$Churn=="Yes" & NewTelco$SeniorCitizen=="Yes" )

sum(NewTelco$SeniorCitizen=="No" )
sum(NewTelco$Churn=="Yes" & NewTelco$SeniorCitizen=="No" )

prop.test( c(476, 1393) , c(1142,5890), alternative = c("greater") )


#3-a) Do monthly contract churn greater than yearly contract churn?
# H0: monthly contract churn <= yearly contract churn  
# H1: monthly contract churn >  yearly contract churn

sum(NewTelco$Contract=="Month-to-month" )
sum(NewTelco$Churn=="Yes" & NewTelco$Contract=="Month-to-month" )

sum(NewTelco$Contract=="One year",NewTelco$Contract=="Two year")
sum(NewTelco$Churn=="Yes" & NewTelco$Contract=="One year" )
sum(NewTelco$Churn=="Yes" & NewTelco$Contract=="Two year" )

prop.test( c(1655, 214) , c(3875,3157), alternative = c("greater") )

#3-b) Do one year contract churn greater than two year contract churn?
# H0: one year contract churn <= two year contract churn  
# H1: one year contract churn >  two year contract churn  

sum(NewTelco$Contract=="One year")
sum(NewTelco$Churn=="Yes" & NewTelco$Contract=="One year" )

sum(NewTelco$Contract=="Two year")
sum(NewTelco$Churn=="Yes" & NewTelco$Contract=="Two year")

prop.test( c(166, 48) , c(1472,1685), alternative = c("greater") )


#B) HYPOTHESIS TESTING-for continuous variables (tenure,MonthlyCharges,TotalCharges)

#1-1) t-test for tenure
# Do the mean of tenure in non-churn samples higher than the mean of tenure in churn samples?
# H0:  the mean of tenure in non-churn samples <=  the mean of tenure in churn samples
# H1:  the mean of tenure in non-churn samples >  the mean of tenure in churn samples

#separate vectors for non-churn and churn
Churn_Yes <-subset(NewTelco, subset=(NewTelco$Churn =="Yes"))
Churn_Yes
Churn_No <-subset(NewTelco, subset=(NewTelco$Churn =="No"))
Churn_No

t.test(Churn_No$tenure,Churn_Yes$tenure, alternative = c("greater"))

#1-2) f-test for tenure
# to test for tenure in non-churn samples and tenure in churn samples
# H0: the variance of  tenure in non-churn samples <= the variance of  tenure in churn samples
# H1: the variance of  tenure in non-churn samples >  the variance of  tenure in churn samples

var.test(Churn_No$tenure,Churn_Yes$tenure, alternative = c("greater"))


#2-1) t-test for MonthlyCharge
# Do  the mean of monthly charge in non-churn samples less than the mean of  monthly charge in churn sample?
# H0:  the mean of monthly charge in non-churn samples >=  the mean of monthly charge in churn samples
# H1:  the mean of monthly charge in non-churn samples <   the mean of monthly charge in churn samples

t.test(Churn_No$MonthlyCharges,Churn_Yes$MonthlyCharges, alternative = c("less"))

#2-2) f-test for MonthlyCharge
# to test for the variance of  monthly charge in non-churn samples and monthly charge in churn samples
# H0: the variance of monthly charge in non-churn samples <= the variance of monthly charge in churn samples
# H1: the variance of monthly charge in non-churn samples >  the variance of monthly charge in churn samples

var.test(Churn_No$MonthlyCharges,Churn_Yes$MonthlyCharges, alternative = c("greater"))


#3-1) t-test for TotalCharge
# Do  the mean of total charge in non-churn samples higher than the mean of  total charge in churn samples?
# H0:  the mean of total charge in non-churn samples <=  the mean of total charge in churn samples
# H1:  the mean of total charge in non-churn samples >   the mean of total charge in churn samples

t.test(Churn_No$TotalCharges,Churn_Yes$TotalCharges, alternative = c("greater"))

#3-2) f-test for TotalCharge
# to test for the variance of  total charge in non-churn samples and total charge in churn samples
# H0: the variance of total charge in non-churn samples <= the variance of total charge in churn samples
# H1: the variance of total charge in non-churn samples >  the variance of total charge in churn samples

var.test(Churn_No$TotalCharges,Churn_Yes$TotalCharges, alternative = c("greater"))


#C) Logistic Regression: predict Churn
#1) Change Churn type: factor -> numreric (0/1)
NewTelco$Churn=as.numeric(ifelse(NewTelco$Churn=="Yes", 1, 0))
prop.table(table(NewTelco$Churn))

#2) Create Training and Test Samples
#samples: remove customer ID
samples <- NewTelco[-1]

#split into training and testing data
set.seed(0109)
ind <- sample(2, nrow(samples), replace=T, prob=c(0.7, 0.3))
train.samples <- samples[ind==1, ]
test.samples<- samples[ind==2, ]


#3) Double check if the churn rates of two sets are close
prop.table(table(train.samples$Churn))
prop.table(table(test.samples$Churn))


#4) Compute Information Values
library(smbinning)
# segregate continuous and factor variables
factor_vars <- c ("gender", "SeniorCitizen", "Partner", 
                  "Dependents", "PhoneService", "MultipleLines", 
                  "InternetService", "OnlineSecurity", "OnlineBackup",
                  "DeviceProtection", "TechSupport", "StreamingTV",
                  "StreamingMovies", "Contract", "PaperlessBilling", "PaymentMethod")
continuous_vars <- c("tenure", "MonthlyCharges","TotalCharges")
iv_df <- data.frame(VARS=c(factor_vars, continuous_vars), IV=numeric(19)) #19 variables

#Compute Information Values for categoricals (factor) variables
for(i in factor_vars){
  smb <- smbinning.factor(train.samples, y="Churn", x=i) #WOE table
  if(class(smb) != "character"){ 
    iv_df[iv_df$VARS == i, "IV"] <- smb$iv
  }
}

#Compute Information Values for continuous variables
for(j in continuous_vars){
  smb <- smbinning(train.samples, y="Churn", x=j) #WOE table
  if(class(smb) != "character"){
    iv_df[iv_df$VARS == j, "IV"] <- smb$iv
  }
}

#Descending order by Information Values (IV)
iv_df <- iv_df[order(-iv_df$IV), ]
iv_df


#5) Logit Model 1
# Predictors: use IV > 0.1
log.Mod1 <- glm(Churn ~ Contract + tenure + InternetService +
                  MonthlyCharges + PaymentMethod + TotalCharges +
                  PaperlessBilling + OnlineSecurity + Dependents + 
                  TechSupport + Partner + SeniorCitizen,
                data=train.samples, family=binomial(link="logit"))
summary(log.Mod1) #AIC: 4104.6

# Predict results
log.Mod1.predict <- plogis(predict(log.Mod1, test.samples)) 

# Decide on optimal prediction probability cutoff for the model 1
library(InformationValue)
optCutOff1 <- optimalCutoff(test.samples$Churn, log.Mod1.predict)[1] 
optCutOff1 #0.522115: score above this is "Yes", below is "No"

#6) Logit Model 1 diagnostics
#a) use VIF function to check multicollinearity
library(car)
vif(log.Mod1) #since GVIF of tenure and TotalCharges > 10 

#b) check for correlation
library(ggplot2)
library(ggcorrplot)
multi.var <- c(5,18,19)
corr <- round(cor(samples[multi.var]), 2) 
ggcorrplot(corr, type="lower",lab = TRUE) 
#TotalCharges has high correlation with tenure and MonthlyCharges

#7) Logit Model 2 
# Predictors: remove TotalCharges from Model 1
log.Mod2 <- glm(Churn ~ Contract + tenure + InternetService +
                  MonthlyCharges + PaymentMethod +
                  PaperlessBilling + OnlineSecurity + Dependents + 
                  TechSupport + Partner + SeniorCitizen ,
                data=train.samples, family=binomial(link="logit"))
summary(log.Mod2) #AIC: 4114.1

# Predict results
log.Mod2.predict <- plogis(predict(log.Mod2, test.samples)) #predic results: 0~1

# Decide on optimal prediction probability cutoff for the model 2
optCutOff2 <- optimalCutoff(test.samples$Churn, log.Mod2.predict)[1] 
optCutOff2 #0.5200026: score above this is "Yes", below is "No"

#8) Logit Model 2 diagnostics
#a) use VIF function to check multicollinearity
vif(log.Mod2) #All GVIF < 10 

#b) ROC Curve: Greater the area, better the predictive ability
plotROC(test.samples$Churn, log.Mod2.predict) #0.8397

#c) Sensitivity and Specificity
sensitivity(test.samples$Churn, log.Mod2.predict, threshold = optCutOff2) #0.4956672
specificity(test.samples$Churn, log.Mod2.predict, threshold = optCutOff2) #0.9135959

#d) Accuracy
predict2.table <- confusionMatrix(test.samples$Churn, log.Mod2.predict, threshold = optCutOff2)
log.Mod2.accuracy <- (predict2.table[1,1]+predict2.table[2,2])/(sum(predict2.table))
log.Mod2.accuracy #0.8014877


#D) Decision Tree: predict Churn
#1) Build decision tree: use same predictors from Logit Model 2
library(rpart.plot)
library(rpart)
tree.formula <- Churn~ Contract + tenure + InternetService + 
  MonthlyCharges + PaymentMethod + PaperlessBilling + 
  OnlineSecurity + Dependents + TechSupport + Partner + SeniorCitizen
tree.rpart <- rpart(tree.formula, data=train.samples,method = 'class')
rpart.plot(tree.rpart)

#2) Predict
#predict results: 0(No)/1(Yes)
tree.predict <- predict(tree.rpart,newdata = test.samples,type ="class")

#3)Diagnose decision tree 
#a) Sensitivity, Specificity, and Accuracy rate
tree1.table <- table(test.samples$Churn, tree.predict)
tree1.sensitivity <- sum(tree1.table[2,2])/sum(test.samples$Churn=="1")
tree1.specificity <- sum(tree1.table[1,1])/sum(test.samples$Churn=="0")
tree1.accuracy <- (tree1.table[1,1]+tree1.table[2,2])/(sum(tree1.table))
tree1.sensitivity #0.3882149
tree1.specificity #0.9415502
tree1.accuracy #0.7931195

#b) ROC Curve
#change tree.predict type: factor -> numeric
tree.predict=as.numeric(ifelse(tree.predict=="1", 1, 0))
plotROC(test.samples$Churn, tree.predict) #0.6535


