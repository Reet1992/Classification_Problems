
setwd("C:/Users/Ted/Desktop/endbegin/test1/germandata/")
gc <- read.csv("german_credit_data.csv")

df <- data.frame(gc)
df2 <- data.frame(gc)

library(tidyr) ###library for using drop_na column##

df3 <- df %>% drop_na()
df4 <- df2 %>% drop_na()

which(df3$Purpose == 'Car')

library(plyr)   #### replacing all the categorical value with numbers
df3$Purpose <- revalue(df3$Purpose, c("car"="1","education" = "2","furniture/equipment"="3","radio/TV"="4","business"="5","domestic appliances"="6","repairs" = "7","vacation/others"="8"))

df3$Age <- as.factor(df3$Age)
df3$Duration <- as.factor(df3$Duration)
df3$Housing <- as.factor(df3$Housing)

###calssification with decision Tree###

set.seed(1234)

pd <- sample(2,nrow(df3),replace = TRUE,prob = c(0.8,0.2))
train <- df3[pd==1,]
validate <- df3[pd==2,]

install.packages("partykit")
library("party")

tree <- ctree(Purpose~Duration+Credit.amount,data = train, controls = ctree_control(mincriterion = 0.99,minsplit = 5000))
tree
plot(tree)

predict(tree,validate,type = "prob")


##misclassification of the training data##

tab <- table(predict(tree),train$Purpose)
print(tab)  

1-sum(diag(tab))/sum(tab)




