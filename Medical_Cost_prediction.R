#Importing the dataset
library(readr)
insurance <- read_csv("C:/Users/prach/Downloads/Kaggle_Projects/Medical_cost_predicition/insurance.csv")

View(insurance)


# Load libraries
library(ggplot2)
library(ggthemes)
library(psych)
library(relaimpo)
library(dplyr)
install.packages("nortest")
library(nortest)

#Exploring the overall data
str(insurance)
summary(insurance)

#Checking for missing values
colSums(is.na(insurance))

#there are no missing values in the dataset


#Converting the variables with character data type as factors
data<- insurance%>%
  mutate_if(is.character,as.factor)

summary(data)

#Univariate Analysis
#age
ggplot(data,aes(age))+
  geom_histogram(col="black",fill="white",binwidth = 1)

#sex
data.frame(prop.table(table(data$sex)))%>%
  ggplot(aes(Var1,Freq))+
  geom_bar(stat="identity",fill="white",col="black")+
  geom_text(aes(y=Freq+.02,label=paste0(Freq*nrow(data)," / ",signif(Freq,3))))+
  labs(x="sex",y="count / Freq")

#bmi
ggplot(data,aes(bmi))+
  geom_histogram(aes(y=..density..),col="black",fill="white")+
  stat_function(fun=dnorm,color="red",args = list(mean=mean(data$bmi),sd=sd(data$bmi)),lwd=1.5)

#red line shows normal distribution. Doing the adtest

ad.test(data$bmi)

#pvalue is 0.003, which is less than 0.05. Thus, it is a normal distribution

#children

data.frame(prop.table(table(data$children)))%>%
  ggplot(aes(Var1,Freq))+
  geom_bar(stat="identity",fill="white",col="black")+
  geom_text(aes(y=Freq+.01,label=paste0(Freq*nrow(data)," / ",signif(Freq,3))))+
  labs(x="number of children",y="count / Freq")

#smoker
data.frame(prop.table(table(data$smoker)))%>%
  ggplot(aes(Var1,Freq))+
  geom_bar(stat="identity",fill="white",col="black")+
  geom_text(aes(y=Freq+.03,label=paste0(Freq*nrow(data)," / ",signif(Freq,3))))+
  labs(x="have smoke",y="count / Freq")

#region
data.frame(prop.table(table(data$region)))%>%
  ggplot(aes(Var1,Freq))+
  geom_bar(stat="identity",fill="white",col="black")+
  geom_text(aes(y=Freq+.01,label=paste0(Freq*nrow(data)," / ",signif(Freq,3))))+
  labs(x="region",y="count / Freq")

#charges
ggplot(data,aes(charges))+
  geom_histogram(col="black",fill="white")

#Bivariate Analysis

#Descriptive statistics by different groups
describeBy(data$charges, data$region)
plot1 <- ggplot(data = data,aes(region,charges)) + geom_boxplot(fill = c(2:5)) + theme_classic() + ggtitle("Boxplot of Medical Charges per Region")

describeBy(data$charges,data$smoker)

plot2 <- ggplot(data = data,aes(smoker,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Smoking Status")

describeBy(data$charges,data$sex)

plot3 <- ggplot(data = data,aes(sex,charges)) + geom_boxplot(fill = c(2:3)) +
  theme_classic() + ggtitle("Boxplot of Medical Charges by Gender")

describeBy(insurance$charges,insurance$children)

plot4 <- ggplot(data = data,aes(as.factor(children),charges)) + geom_boxplot(fill = c(2:7)) + theme_classic() +  xlab("children") + ggtitle("Boxplot of Medical Charges by Number of Children")



# Create new variable "Obese" derived from bmi
data$obese = ifelse(bmi>=30,"yes","no")

describeBy(data$charges, data$obese)

plot5 <- ggplot(data = data,aes(obese,charges)) + geom_boxplot(fill = c(2:3)) + theme_classic() + ggtitle("Boxplot of Medical Charges by Obesity")

#Checking correlation among the numerical variables
pairs.panels(data[c("age", "bmi", "children", "charges")])

# Build a model using the original set of variables
#Here the data is not divided into training and test, as it is too small, and we just want to see our model is performing.
#Also, this would help us determine the important predictors

ins_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = data)
summary(ins_model)

#The adj R square with the first model is 0.74. 
#The important variables are age, children, bmi, smoker"Yes", southeast and south west region

plot(age,charges,col=smoker)

#Implementing a model with these 2 variables
ins_model3<-lm(charges~age+smoker,data)
summary(ins_model3)

#The results show that both of these variables explain 72% variability in the charges. Thus, this is the simplest model which could be used if required

#Variable Importance
intercepts<-c(coef(ins_model3)["(Intercept)"],coef(ins_model3)["(Intercept)"]+coef(ins_model3)["smokeryes"])
lines.df<- data.frame(intercepts = intercepts,
                      
                      slopes = rep(coef(ins_model3)["age"], 2),
                      
                      smoker = levels(data$smoker))
qplot(x=age,y=charges,color=smoker,data=data)+geom_abline(aes(intercept=intercepts,slope=slopes,color=smoker),data=lines.df) + theme_few() + scale_y_continuous(breaks = seq(0,65000,5000))

ins_model_shapley<-calc.relimp(ins_model,type="lmg")
ins_model_shapley
ins_model_shapley$lmg
sum(ins_model_shapley$lmg)

barplot(sort(ins_model_shapley$lmg,decreasing = TRUE),col=c(2:10),main="Relative Importance of Predictors",xlab="Predictor Labels",ylab="Shapley Value Regression",font.lab=2)
