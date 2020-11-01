
setwd('E:/R_Datasets/R_projects_ML_cape_fear')
df <- read.csv('personality/2018-personality-data.csv')

df_r <- read.csv('personality/2018_ratings.csv')


df$assigned.metric <- as.numeric(df$assigned.metric)
df$assigned.condition <- as.numeric((df$assigned.condition))

#### Exploratory Data Analysis ### 


library(ggplot2)
library(plotly)

### Plot 1 

sp <- ggplot(df, aes(x=emotional_stability, y=openness, color=agreeableness)) +geom_point(size=3)
sp
g1 <- sp + facet_grid(assigned.metric~ assigned.condition)
g1







