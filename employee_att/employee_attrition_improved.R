

setwd("E:/R_Datasets/R_projects_ML_cape_fear/employee_att")

df <- read.csv(file = 'employee_att.csv')
head(df)

#### Data Preprocessing ####### 

df$Attrition <- as.numeric(df$Attrition)

df$BusinessTravel <- as.numeric(df$BusinessTravel)

df$OverTime <- as.numeric(df$OverTime)

df$Over18 <- as.numeric(df$Over18)

df$MaritalStatus <- as.numeric(df$MaritalStatus)

df$JobRole <- as.numeric(df$JobRole)

df$Department <- as.numeric(df$Department)

df$EducationField <- as.numeric(df$EducationField)

df2 <- df

df <- df[ -c(12) ]

#### Convert those colums to numeric which are actually not numeric.

library( taRifx )
df <- japply( df, which(sapply(df, class)=="integer"), as.numeric )


#### Correlation Calculation ##### 

cor(df,method = c("pearson","kendall","spearman"))

summary(cor(df,method = c("pearson","kendall","spearman")))


###### [1]######

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(df, col=col, symm=TRUE)

######[2]########


r1 <- exp(cor(df))/2

round(r1,2)

r2 <- cor(df)

round(r2,2)

r3 <- log(cor(df))

round(r3,3)

library(ggplot2)

library(ggcorrplot)

g = (r1-r2-r3)/3

ggcorrplot(g)


#### Exploratory Data Analysis ######

ggplot(df,aes(x = ï..Age, y = DistanceFromHome))

library(plotly)

ggplot(df, aes(x=JobLevel, y=MonthlyIncome, color=Attrition)) +
  geom_point(size=3) +
  labs(x = "JobLevel",
       y = "MonthlyIncome",
       color = "Attrition") +
  theme_bw()


##### train test split#######


train_index <- sample(1:nrow(df), 0.80 * nrow(df))
test_index <- setdiff(1:nrow(df), train_index)

# Build X_train, y_train, X_test, y_test
X_train <- df[train_index, -15]
y_train <- df[train_index, "Attrition"]

X_test <- df[test_index, -15]
y_test <- df[test_index, "Attrition"]



x2_train <- X_train[17:22]
y2_train <- X_train[2]

X2_test <- X_test[17:22]
y2_test <- X_test[2]


df3 <- df2

keeps <- c("MaritalStatus","OverTime","DistanceFromHome","Attrition")
df3 = df[keeps]


#### Data Split For fitting into the tree #### 


pd <- sample(2,nrow(df3),replace = TRUE,prob = c(0.60,0.40))
train2 <- df3[pd==1,]
validate2 <- df3[pd==2,]


##### Decision Tree Model ####### 


library(partykit)

## model Building ## 

tree <- ctree(Attrition~MaritalStatus+OverTime+DistanceFromHome,data = train2, control = ctree_control(mincriterion = 0.999,minsplit = 50000))
tree
plot(tree) 

### Accuracy Calculation 

predict(tree,validate2,type = "prob")

tab <- table(predict(tree),train2$Attrition)

print(tab)

1-sum(diag(tab))/sum(tab)


#### Wald test ### 


library(aod)


df$Attrition <- factor(df$Attrition)
mylogit <- glm(Attrition~MaritalStatus+OverTime+DistanceFromHome, data = df, family = "binomial")

summary(mylogit)

## CIs using profiled log-likelihood

CIS <- confint(mylogit)

exp(coef(mylogit))

#  (Intercept) MaritalStatus      OverTime 
#0.006026267   2.010431809   4.029261606

### Binding both types of coefficients #### 


exp(cbind(OR = coef(mylogit), confint(mylogit)))

#                       OR       2.5 %     97.5 %
#(Intercept)   0.006026267 0.002929186 0.01200599
#MaritalStatus 2.010431809 1.629282339 2.49567154
#OverTime      4.029261606 3.004006272 5.41859912

### Wald Test #### 

wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 1:3)



with(mylogit, null.deviance - deviance)





train2$Attrition <- factor(train2$Attrition)
validate2$Attrition <- factor(validate2$Attrition)
logitMod <- glm(Attrition~MaritalStatus+OverTime, data = train2, family = "binomial")

predicted <- plogis(predict(logitMod, validate2))


library(InformationValue)
optCutOff <- optimalCutoff(validate2$Attrition, predicted)[1] 

summary(logitMod)


#Call:
 # glm(formula = Attrition ~ MaritalStatus + OverTime, family = "binomial", 
  #    data = train2)

#Deviance Residuals: 
 # Min       1Q   Median       3Q      Max  
#-1.0979  -0.5900  -0.4206  -0.2967   2.5081  

#Coefficients:
 # Estimate Std. Error z value Pr(>|z|)    
#(Intercept)    -5.2920     0.4059 -13.037  < 2e-16 ***
 # MaritalStatus   0.7205     0.1207   5.972 2.34e-09 ***
#  OverTime        1.4703     0.1687   8.714  < 2e-16 ***
 # ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#(Dispersion parameter for binomial family taken to be 1)

#Null deviance: 1059.42  on 1231  degrees of freedom
#Residual deviance:  952.31  on 1229  degrees of freedom
#AIC: 958.31

#Number of Fisher Scoring iterations: 5


misClassError(validate2$Attrition, predicted, threshold = optCutOff)

#### [1] 0.2973 #### 

plotROC(validate2$Attrition, predicted)


Concordance(validate2$Attrition, predicted)

sensitivity(validate2$Attrition, predicted, threshold = optCutOff)

### [1] 0.832

specificity(validate2$Attrition, predicted, threshold = optCutOff)

#####[1] 0.04347826

confusionMatrix(validate2$Attrition, predicted, threshold = optCutOff)

#    1  2
#0  63  3
#1 312 66





#### Improve accuracy ##### 


new_df = (exp(df)-df-log(df))/6

pd2 <- sample(2,nrow(new_df),replace = TRUE,prob = c(0.75,0.25))
train3 <- new_df[pd2==1,]
validate3 <- new_df[pd2==2,]

train3$Attrition <- factor(train3$Attrition)
validate3$Attrition <- factor(validate3$Attrition)
logitMod2 <- glm(Attrition~MaritalStatus+OverTime+DistanceFromHome , data = train3, family = "binomial")

predicted <- plogis(predict(logitMod2, validate3))


library(InformationValue)
optCutOff <- optimalCutoff(validate3$Attrition, predicted)[1] 

summary(logitMod)



misClassError(validate3$Attrition, predicted, threshold = optCutOff)

#### [1] 0.2973 #### 

plotROC(validate3$Attrition, predicted)


Concordance(validate2$Attrition, predicted)

sensitivity(validate2$Attrition, predicted, threshold = optCutOff)

### [1] 0.832

specificity(validate2$Attrition, predicted, threshold = optCutOff)

#####[1] 0.04347826

confusionMatrix(validate3$Attrition, predicted, threshold = optCutOff)









































