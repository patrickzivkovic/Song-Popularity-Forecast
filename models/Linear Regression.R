# Trying to predict the popularity of a song using all the variables we created
# (general information about the song, readability and sentiment of the lyrics)

pacman::p_load(dplyr, data.table, tidyverse, magrittr, ggplot2, leaps, corrplot, car, caret)

data <- fread("data/merged_dataset.csv")

data$genre <- as.numeric(factor(data$genre))
#Prepare data
data %<>% select(-artist,-songname,-duration_ms)
#To check how only readability and sentiment variables perform
#data %<>% select(-artist,-songname,-duration_ms,-genre, -artist_popularity, -year, -duration_s)

#Correlation matrix is indicating strong correlations between 
#           - the two readability inidices
#           - artist popularity and song popularity
#           - strong positive/negative correlations between the intuitively similar/opposite sentiments
ggcorrplot::ggcorrplot(cor(data), method = "circle", type = "lower")

#Forward model 
forward_model <- regsubsets(data=data, popularity ~ .,
                            method = "forward", nvmax = length(data))
summary(forward_model)
par(mfrow=c(1,3))
plot(forward_model, scale = c('bic'))
plot(forward_model, scale = c('adjr2'))
plot(forward_model, scale = c('Cp'))
par(mfrow=c(1,1))

plot(forward_model, scale = c('adjr2'))


metrics <- summary(forward_model)
which.max(metrics$adjr2)
which.min(metrics$cp)
which.min(metrics$bic)

# id: model id
# object: regsubsets object
# data: data used to fit regsubsets
# outcome: outcome variable
get_model_formula <- function(id, object, outcome){
  # get models data
  models <- summary(object)$which[id,-1]
  # Get outcome variable
  #form <- as.formula(object$call[[2]])
  #outcome <- all.vars(form)[1]
  # Get model predictors
  predictors <- names(which(models == TRUE))
  predictors <- paste(predictors, collapse = "+")
  # Build model formula
  as.formula(paste0(outcome, "~", predictors))
}

#Example of extracting a formula which is later used for the model
get_model_formula(5, forward_model, "popularity")

#Get 10-fold-crossvalidation error
get_cv_error <- function(model.formula, data){
  set.seed(1)
  train.control <- trainControl(method = "cv", number = 10)
  cv <- train(model.formula, data = data, method = "lm",
              trControl = train.control)
  cv$results$MAE
}

# Check the average error rate for every single variable configuration of the calculated forward model
model.ids <- 1:(length(data)-1)
cv.errors <- map(model.ids, get_model_formula, forward_model, "popularity") %>%
  map(get_cv_error, data = data) %>%
  unlist()
cv.errors
mean(cv.errors)
sd(cv.errors)

# Remind ourselves in which boundary the most popularity score are populated
summary(data$popularity)

which.min(cv.errors) #The model which got chosen by the cross-validation
coef(forward_model,13)
