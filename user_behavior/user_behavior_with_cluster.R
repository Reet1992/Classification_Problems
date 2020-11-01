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

accuracy <- ((conf$`0`[1]+conf$`1`[2])/(conf$`0`[1]+conf$`0`[2]+conf$`1`[1]+conf$`1`[2]))*100
accuracy

library(ggplot2)

autoplot(conf, type = "mosaic")

library(ggcorrplot)
ggcorrplot(conf/100)







library(ggplot2)
ggplot(df_new, 
       aes(x = df.Daily.Internet.Usage, y = df.Area.Income, size = df.Clicked.on.Ad)) +
  geom_point(alpha = .5,fill=df_new$df.Age)



fit1 <- kmeans(df_new, 4)
fit2 <- kmeans(df_new, 5)

### correlation between centers #### 

c1 <- fit1$centers
c2 <- fit2$centers

cor(c1)
cor(c2)

ggcorrplot(cor(c1))

library(superheat)
superheat(c1)
superheat(c2)

library(corrplot)
corrplot(cor(centers), method="number")

#########


library(cluster)


# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster)
clusplot(df_new, fit1$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(df_new, fit1$cluster)


#### Vlidating 2 cluster solutions

plot(cluster.stats(fit1$cluster,fit2$cluster),df_new$df.Clicked.on.Ad)



library(ggplot2)
ggplot(df_new, 
       aes(x = df.Daily.Internet.Usage, y = df.Area.Income, size = df.Clicked.on.Ad)) +
  geom_point(alpha = .5,fill=fit2$cluster)
