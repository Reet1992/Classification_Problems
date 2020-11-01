### User Behavior Dataset ### 

setwd('E:/R_Datasets/R_projects_ML_cape_fear')
df <- read.csv('user_behavior/advertising.csv')

X <- data.frame(df$Age,df$Area.Income,df$Daily.Internet.Usage)
y <- data.frame(df$Clicked.on.Ad)

df_new <- data.frame(df$Age,df$Area.Income,df$Daily.Internet.Usage,df$Clicked.on.Ad)

pd <- sample(2,nrow(df_new),replace = TRUE,prob = c(0.85,0.15))
train <- df_new[pd==1,]
validate <- df_new[pd==2,]

library(partykit)

logitmod <- glm(df.Clicked.on.Ad~df.Daily.Internet.Usage+df.Area.Income+df.Age,data = train,family=binomial(link="logit") )
logitmod
plot(logitmod) 

predicted <- plogis(predict(logitmod, validate)) 
print(predicted)

### Optimal Cutff ### 

library(InformationValue)

optCutOff <- optimalCutoff(validate$df.Clicked.on.Ad, predicted)[1] 
print(optCutOff)

### Accuracy #### 

actuals_preds <- data.frame(cbind(actuals=validate$df.Clicked.on.Ad, predicteds=predicted))
print(actuals_preds)

correlation_accuracy <- cor(actuals_preds)
correlation_accuracy

#### Missclassification error 

misClassError(validate$df.Clicked.on.Ad, predicted, threshold = optCutOff)*100 

summary(logitmod)

plotROC(validate$df.Clicked.on.Ad, predicted)

### Min_max_accuracy ##### 

min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))  

print(min_max_accuracy)

### Confusion Matrix ### 
conf <- confusionMatrix(validate$df.Clicked.on.Ad, predicted, threshold = optCutOff)

conf

library(ggplot2)

autoplot(conf, type = "mosaic")
  
conf$`1`
