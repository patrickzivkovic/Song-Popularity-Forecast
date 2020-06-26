
if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, dplyr, data.table, stats, Hmisc, forcats, psych, h2o, caret, DMwR, crossval)

data = fread("Data-Challenge-Songlyrics/merged_dataset.csv", na.strings = ".")

set.seed(15000)
fact_cols = c("genre", "year")
setDT(data)[, (fact_cols):= lapply(.SD, factor), .SDcols=fact_cols]

colSums(apply(data, 2, is.na)) #check auf NAs - keine in unseren Daten

data_new = data %>% select(-artist, -songname, -duration_ms)

#--------------------------------------------------------------------------------------------------#

h2o.init(nthreads=-1, max_mem_size="4G", enable_assertions = FALSE)
data.h2o = as.h2o(data_new)
# qdap::qview(data.h2o)
# x = h2o.kmeans(training_frame = data.h2o, x = 1:20, k = 10)
# data.h2o$clusters = h2o.predict(x, newdata = data.h2o)



data_splits <- h2o.splitFrame(data =  data.h2o, ratios = c(0.6, 0.2), seed = 1234)
train <- data_splits[[1]]
valid <- data_splits[[2]]
test <- data_splits[[3]]



h2o.grid("randomForest", x = c(1,3:20), y = 2, 
         grid_id = "rf_grid", 
         training_frame = train, 
         validation_frame = valid,
         hyper_params = list(max_depth = seq(12,20,1), 
                             ntrees = seq(10,30,1)
         ))


h2o.grid("randomForest", x = c(1,3:20), y = 2, 
         grid_id = "rf_grid_3", 
         training_frame = train, 
         validation_frame = valid, 
         hyper_params = list(max_depth = seq(12,20,1), 
                             ntrees = seq(10,70,10)
                             ))

rf_1  = h2o.getModel(h2o.getGrid("rf_grid", sort_by = "mae")@model_ids[[1]])
rf_2 = h2o.getModel(h2o.getGrid(grid_id = "rf_grid", sort_by = "rmse", decreasing = F)@model_ids[[1]])
rf_3 = h2o.getModel(h2o.getGrid("rf_grid_3", sort_by = "mae")@model_ids[[1]])
h2o.varimp_plot(rf_1)
h2o.varimp_plot(rf_2)
h2o.varimp_plot(rf_3)


h2o.performance(rf_1, newdata = test)
h2o.performance(rf_2, newdata = test)
h2o.performance(rf_3, newdata = test)

h2o.saveModel(rf_1, path = "Data-Challenge-Songlyrics/07_Modelling/models/randomForest_1")
h2o.saveModel(rf_3, path = "Data-Challenge-Songlyrics/07_Modelling/models/randomForest_2")


# h2o.predict_contributions(rf_3, valid)



gbm_params2 <- list(learn_rate = seq(0.05, 0.15, .05),
                    max_depth = seq(5, 15, 1),
                    ntrees = seq(5,20,1),
                    sample_rate = seq(0.5, 1.0, .1),
                    col_sample_rate = seq(0.5, 1.0, .1),
                    min_rows = seq(5,10, 1))

search_criteria <- list(strategy = "RandomDiscrete", stopping_metric = "MAE", stopping_tolerance = 0.0001, stopping_rounds = 5)


h2o.grid("gbm", x = c(1,3:20), y = 2, grid_id = "gbm_grid", 
         training_frame = train, validation_frame = valid, hyper_params = gbm_params2, nfolds = 5,
         search_criteria = search_criteria)

gbm_1  = h2o.getModel(h2o.getGrid("gbm_grid", sort_by = "mae")@model_ids[[1]])
gbm_2 = h2o.getModel(h2o.getGrid(grid_id = "gbm_grid", sort_by = "rmse", decreasing = F)@model_ids[[1]])

# gbm_model = h2o.getModel(h2o.getGrid("gbm_grid_1", sort_by = "rmsle")@model_ids[[1]])

h2o.varimp_plot(gbm_1)
h2o.varimp_plot(gbm_2)

h2o.performance(gbm_1, newdata = test)
h2o.performance(gbm_2, newdata = test)

predictions = h2o.predict(gbm_1, newdata = test)
test$predicted = predictions
# h2o.shutdown()

#test_df2 = test_df %>% select(popularity, predicted)

#--------------------------------------------------------------------------------------------------------#
# Auto-ML kreiirt viele Modelle (hier: hauptsächlich GBM, paar GLM und deep learning) und vereint sie zu einem Stacked Ensemble
h2o.automl(x = c(1,3:20), y = 2,
                     training_frame = train, validation_frame = valid, stopping_metric = "MAE", project_name = "auto_ml")

h2o.get_automl(auto_ml)
auto_ml = h2o.getModel("StackedEnsemble_AllModels_AutoML_20200624_190302")
auto_ml2 = h2o.getModel("StackedEnsemble_BestOfFamily_AutoML_20200624_190302")
h2o.performance(auto_ml, newdata = test)
h2o.performance(auto_ml2, newdata = test)
h2o.saveModel(auto_ml, path = "Data-Challenge-Songlyrics/07_Modelling/models/auto_ml")
h2o.saveModel(auto_ml2, path = "Data-Challenge-Songlyrics/07_Modelling/models/auto_ml_best_of_family")

h2o.removeAll(retained_elements = c(test, valid, train, data.h2o))


name_rf1 = list.files("Data-Challenge-Songlyrics/07_Modelling/models/randomForest_1")
name_rf2 = list.files("Data-Challenge-Songlyrics/07_Modelling/models/randomForest_2")
name_gbm1 = list.files("Data-Challenge-Songlyrics/07_Modelling/models/GBM_1")
name_gbm2 = list.files("Data-Challenge-Songlyrics/07_Modelling/models/GBM_2")
name_ml_bof = list.files("Data-Challenge-Songlyrics/07_Modelling/models/auto_ml_best_of_family")
name_ml = list.files("Data-Challenge-Songlyrics/07_Modelling/models/auto_ml")

rf_1 = h2o.loadModel(paste0("Data-Challenge-Songlyrics/07_Modelling/models/randomForest_1/", name_rf1))
rf_2 = h2o.loadModel(paste0("Data-Challenge-Songlyrics/07_Modelling/models/randomForest_2/", name_rf2))
gbm_1 = h2o.loadModel(paste0("Data-Challenge-Songlyrics/07_Modelling/models/GBM_1/", name_gbm1))
gbm_2 = h2o.loadModel(paste0("Data-Challenge-Songlyrics/07_Modelling/models/GBM_2/", name_gbm2))
auto_ml_bof = h2o.loadModel(paste0("Data-Challenge-Songlyrics/07_Modelling/models/auto_ml_best_of_family/", name_ml_bof))
auto_ml = h2o.loadModel(paste0("Data-Challenge-Songlyrics/07_Modelling/models/auto_ml/", name_ml))

h2o.performance(auto_ml_bof, newdata = test)
h2o.performance(auto_ml, newdata = test)

h2o.shutdown()
