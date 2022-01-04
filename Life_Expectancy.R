#Importing Libraries

library(dplyr)
library(tidyverse)
library(ggplot2)
library(psych)
library(leaps)
library(Metrics)

# Analyzing the 2 tables 
# We merge the data into a single table

?merge
df <- merge(Life_Expectancy_per_Country, world_pop, by = "Country" )
view(df)

summary(df)

#There are N.A. values in the column Urban Population Percentage, so we drop it.
names(df)[names(df) == "Urban Pop%"] <- "urban"
life_exp <- subset(df, select = -c(urban))
View(life_exp)

#Exploring the final life_exp data set
dim(life_exp)
summary(life_exp)
class(life_exp)
lapply(life_exp, class)

# Converting characters to factors
life_exp$Country <- as.factor(life_exp$Country)
life_exp$`Fert. Rate` <- as.numeric(life_exp$`Fert. Rate`)
life_exp$`Med. Age` <- as.numeric(life_exp$`Med. Age`)

# Target variable is Life Expectancy

# plotting a histogram
hist(life_exp$`Life Expectancy (Both sexes)`,
     main = "LifeExpectance Distribution",
     xlab = "Life Expectancy(yrs)")

# density plot
plot(density(life_exp$`Life Expectancy (Both sexes)`),
     main = "Distribution of Life Expectancy",
     xlab = "Life Expectancy (yrs)")
abline(v=mean(life_exp$`Life Expectancy (Both sexes)`))

#Correlation
cor(data_num, data_num$`Life Expectancy (Both sexes)`)

#selecting the variables that are numeric

data_num <- life_exp %>% 
  select_if(is.numeric)

#Plotting the correlation 
ggcorr(data_num, 
       label = T, 
       label_size = 3,
       label_round = 2,
       hjust = 1,
       size = 3, 
       color = "black",
       layout.exp = 5,name = "Correlation")

#Pearson Method
pairs.panels(data_num, 
             method = "pearson",
             hist.col = "green",
             density = TRUE,
             ellipses = TRUE)

#Variable Selection

#Forward Selection
nullmodel_1 <- lm(`Life Expectancy (Both sexes)`~ 1, data = data_num)
fullmodel_1 <- lm(`Life Expectancy (Both sexes)`~ ., data = data_num)

forward_1 <- step(nullmodel_1, scope=list(lower=nullmodel_1, upper=fullmodel_1), 
                direction='forward')

summary(forward_1)

#Backward Elimination
backward_1 <- step(fullmodel_1,direction='backward')

summary(backward_1)


#Step - Wise Selection
stepwise_1 <- step(nullmodel_1, scope=list(lower=nullmodel_1, upper=fullmodel_1), 
                 direction='both')

summary(stepwise_1)

### Different Models

## Model A - Null Model
model_a <- lm(data_num$`Life Expectancy (Both sexes)` ~ 1, data = data_num)

predict_a <- predict(model_a,  newdata = data_num)

summary(model_a)
plot(model_a)
step(model_a)


## Model B - Full Model
model_b <- lm(data_num$`Life Expectancy (Both sexes)` ~ .,data = data_num)
predict_b <- predict(model_b,  newdata = data_num)
summary(model_b)
plot(model_b)
step(model_b)

## Model C - Using Selected Variables from Forward Selection
model_c <- lm(data_num$`Life Expectancy (Both sexes)` ~ data_num$`Males (Life Exp)` 
              + data_num$`Females (Life Exp)` + data_num$`Yearly Change` 
              + data_num$`Fert. Rate`+ data_num$`Med. Age`, data = data_num)
predict_c <- predict(model_c,  newdata = data_num)
summary(model_c)
plot(model_c)
step(model_c)

## Model D 
model_d <- lm(data_num$`Life Expectancy (Both sexes)` ~ data_num$`Yearly Change`)
predict_d <- predict(model_d,  newdata = data_num)
summary(model_d)
plot(model_d)
step(model_d)

## Model E
model_e <- lm(data_num$`Life Expectancy (Both sexes)` ~ data_num$`Fert. Rate`)
predict_e <- predict(model_e,  newdata = data_num)
summary(model_e)
plot(model_e)
step(model_e)

## Model F
model_f <- lm(data_num$`Life Expectancy (Both sexes)` ~ data_num$`Med. Age`)
predict_f <- predict(model_f,  newdata = data_num)
summary(model_f)
plot(model_f)
step(model_f)

## Model G
model_g <- lm(data_num$`Life Expectancy (Both sexes)` ~ data_num$`Med. Age` + 
                data_num$`Fert. Rate`)
predict_g <- predict(model_g,  newdata = data_num)
summary(model_g)
plot(model_g)
step(model_g)


summary(model_a)$adj.r.squared
summary(model_b)$adj.r.squared 
summary(model_c)$adj.r.squared 
summary(model_d)$adj.r.squared 
summary(model_e)$adj.r.squared 
summary(model_f)$adj.r.squared
summary(model_g)$adj.r.squared






