library(readr)
data.2015 <- read_csv("world-happiness/2015.csv")
head(data.2015)
tail(data.2015)

require(dplyr)
library(dplyr)

#As soon as we will have data for different years, I create a fuction for filtering and renaming the columns
filter.data <- function(data.set) {
  data.set <- data.set %>%
    rename(
      happ_score = `Happiness Score`,
      GDP = `Economy (GDP per Capita)`,
      family = Family,
      health = `Health (Life Expectancy)`,
      freedom = Freedom,
      trust = `Trust (Government Corruption)`,
    ) %>%
    select(c(Country, happ_score, GDP, family, health, freedom, trust))
  return(data.set)
}

#Updated data
#variables left: country, happ_score, GDP, family, health, freedom, trust
data.2015 <-filter.data(data.2015)

require(corrplot)
library(corrplot)
require(PerformanceAnalytics)
library("PerformanceAnalytics")
cor_data <- data.2015[, c(2, 3, 4, 5, 6)]
corr_matrix <- cor(cor_data)
#simple correlation matrix
round(corr_matrix, 2)
#visualized
corrplot(corr_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

require("mlbench")
library(mlbench)
require("caret")
require(caret)
library(caret)
library(randomForest)

model.data <- data.2015[c(2, 3, 4, 5,6, 7)]
#simple linear model
lm.model1 <- lm(happ_score ~., data=model.data)
summary(lm.model1)

#random forest approach
randForest.selection <- randomForest(happ_score ~., data=model.data)
importance(randForest.selection)

plot(data.2015$GDP, data.2015$happ_score,  xlab= "GDP per capita", ylab = "Happiness Score", main = "2015 GDP vs Happiness Score")
#we see linear relationship between parameters
plot(data.2015$family, data.2015$happ_score, xlab = "Family", ylab= "Happiness Score", main = "2015 Family vs Happiness Score")
#in this case we can spot some outliers, but overall relationship is also linear
plot(data.2015$health, data.2015$happ_score, xlab = "Health", ylab= "Happiness Score", main = "2015 Health vs Happiness Score")

plot(data.2015$freedom, data.2015$happ_score, xlab = "Freedom", ylab= "Happiness Score", main = "2015 Freedom vs Happiness Score")
#not strong correlation

data.2015.lmodelGDP <- lm(happ_score ~ GDP , data = data.2015)
summary(data.2015.lmodelGDP)
y <- data.2015$happ_score
x <- data.2015$GDP
ggplot(data = data.2015, aes(GDP, happ_score)) + geom_point() + geom_smooth(method = lm, formula = y ~ x)

data.2015.lmodel <- lm(happ_score ~ GDP + family + health, data = data.2015)
summary(data.2015.lmodel)
y <- data.2015$happ_score
x <- data.2015$GDP + data.2015$family + data.2015$health
ggplot(data = data.2015, aes(GDP + family + health, happ_score)) + geom_point() + geom_smooth(method = lm, formula = y ~ x)

model_fitted <- data.2015$happ_score - residuals(data.2015.lmodel)
model_fitted
plot(model_fitted, residuals(data.2015.lmodel))
abline(h=0)

#load more data
data.2016 <- read_csv("world-happiness/2016.csv")
data.2017 <- read_csv("world-happiness/2017.csv")
data.2018 <- read_csv("world-happiness/2018.csv")
data.2019 <- read_csv("world-happiness/2019.csv")
data.2016 <- filter.data(data.2016)
#standartize the data
data.2017 <- data.2017 %>% rename(
  happ_score = Happiness.Score,
  GDP = Economy..GDP.per.Capita.,
  family = Family, 
  health = Health..Life.Expectancy.,
  freedom = Freedom,
)
data.2018 <- data.2018 %>% rename(
  Country = `Country or region`,
  happ_score = Score,
  GDP = `GDP per capita`,
  #no family, I add NA
  health = `Healthy life expectancy`,
  freedom = `Freedom to make life choices`
)
data.2019 <- data.2019 %>% rename(
  Country = `Country or region`,
  happ_score = Score,
  GDP = `GDP per capita`,
  #no family, I add NA in a loop
  health = `Healthy life expectancy`,
  freedom = `Freedom to make life choices`
)

data.lst <- list(data.2016, data.2017, data.2018, data.2019)
top.lst <- list();
full.lst <- list();
#filter, clean up and select top countries 
i <- 1
for (data.set_i in 1:length(data.lst)) {
  if ("family" %in% colnames(data.lst[[data.set_i]])) {
    new.set <- data.lst[[data.set_i]] %>% select(c(Country, happ_score, GDP, family, health, freedom))
  } else {
    new.set <- data.lst[[data.set_i]] %>% select(c(Country, happ_score, GDP, health, freedom))
    new.set$family = NA
    
  }
  new.set$year = toString(2015 + i)
  top.lst[[i]] <- new.set %>% filter(new.set[2] > 7)
  full.lst[[i]] <- new.set
  i <- i + 1
}

top.2015 <- data.2015 %>% filter(data.2015[2] > 7) %>% select(c(Country, happ_score, GDP, family, health, freedom))
top.2015$year = "2015"

merged.set <- rbind(top.2015, top.lst[[1]], top.lst[[2]], top.lst[[3]], top.lst[[4]])
merged.set
require(ggplot2)
require(dplyr)
require(plotly)
library(ggplot2)
library(dplyr)
library(plotly)
p <- merged.set %>% ggplot(aes(x = year, y = happ_score, colour = Country, group = Country)) + geom_line() + ylab("Happiness Score") + xlab("Year")
p

austria.time.info <- merged.set %>% filter(merged.set[1] == "Austria")
austria.time.info

par(mfrow=c(2,2))
plot(austria.time.info$year, austria.time.info$happ_score, type="o", xlab= "Year", ylab = "Happiness Score")
plot(austria.time.info$year, austria.time.info$GDP, type="o", xlab= "Year", ylab = "GDP")
plot(austria.time.info$year, austria.time.info$health, type="o", xlab= "Year", ylab = "Health")
plot(austria.time.info$year, austria.time.info$freedom, type="o", xlab= "Year", ylab = "Freedom")

data.2015$year = "2015"

merged.set.2 <- rbind(data.2015 %>% select(-c(trust)), full.lst[[1]], full.lst[[2]], full.lst[[3]], full.lst[[4]])
merged.set.2
united.states.info <- merged.set.2 %>% filter(merged.set.2[1] == "United States")
united.states.info

par(mfrow=c(2,2))
plot(united.states.info$year, united.states.info$happ_score, type="o", xlab= "Year", ylab = "Happiness Score")
plot(united.states.info$year, united.states.info$GDP, type="o", xlab= "Year", ylab = "GDP")
plot(united.states.info$year, united.states.info$health, type="o", xlab= "Year", ylab = "Health")
plot(united.states.info$year, united.states.info$freedom, type="o", xlab= "Year", ylab = "Freedom")

finland.info <- merged.set.2 %>% filter(merged.set.2[1] == "Finland")
finland.info

par(mfrow=c(2,2))
plot(finland.info$year, finland.info$happ_score, type="o", xlab= "Year", ylab = "Happiness Score")
plot(finland.info$year, finland.info$GDP, type="o", xlab= "Year", ylab = "GDP")
plot(finland.info$year, finland.info$health, type="o", xlab= "Year", ylab = "Health")
plot(finland.info$year, finland.info$freedom, type="o", xlab= "Year", ylab = "Freedom")