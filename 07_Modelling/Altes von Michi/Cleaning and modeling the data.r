#--------------------------------------------------------------------------------------------------------------------------#
                                              #Cleanen der Daten
#--------------------------------------------------------------------------------------------------------------------------#
                                  ## Einlesen Packages und Daten

if(!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, data.table, stats, Hmisc, forcats, psych, h2o, caret, DMwR, crossval)

data = fread("U:/Business Intelligence/Michi/data_40weeks_modellbau.csv", na.strings = ".")

set.seed(15000)
#--------------------------------------------------------------------------------------------------------------------------#
                              ## Definieren der Spalten, die noch richtig formatiert werden müssen nach Charakter, Faktor und Integer/numeric

char_cols = c("KTNR", "EMAIL", "PLZ")
fact_cols = c("GENDER", "WEEK_GOT_ACTIVE", "TOPF", "KREDITKUNDE", "VERLAUF_KSTATUS", "GOLDKUNDE", "KSPERRE",
              "CUSTOM4", "CLICK_STATUS_WEEK6", "CLICK_STATUS_WEEK40", "OPEN_STATUS_WEEK6", "OPEN_STATUS_WEEK40",
              "HAUPTSORT_BIGGEST_NUMS_LAST_40", "HAUPTSORT_MOST_BOUGHT_LAST_40", "STATE_AKT")
int_cols = c("BRUMS","RETOUREN")

#--------------------------------------------------------------------------------------------------------------------------#
                              ##Spalten in richtiges Format überführen

helper_fun = function(x) { #Da Numerische Werte mit "," als Trennzeichen -> durch "." ersetzen und dann erst umwandeln
  x = sub(",", ".", x, fixed=TRUE)
  x = as.numeric(x)
  return(x)
}


setDT(data)[, (fact_cols):= lapply(.SD, factor), .SDcols=fact_cols]
setDT(data)[, (char_cols):= lapply(.SD, as.character), .SDcols=char_cols]
setDT(data)[, (int_cols):= lapply(.SD, helper_fun), .SDcols=int_cols]

#--------------------------------------------------------------------------------------------------------------------------#
                            ## Faktoren Hauptsort... cleanen ("" wird zu 00 kein Kauf)
                        

data$HAUPTSORT_BIGGEST_NUMS_LAST_40 = fct_recode(data$HAUPTSORT_BIGGEST_NUMS_LAST_40, "00 kein Kauf" = "")
data$HAUPTSORT_BIGGEST_NUMS_LAST_40 = fct_collapse(data$HAUPTSORT_BIGGEST_NUMS_LAST_40, "00 kein Kauf" = c("00 kein Kauf", "-99 nicht zuordenbar"))
data$HAUPTSORT_MOST_BOUGHT_LAST_40 = fct_recode(data$HAUPTSORT_MOST_BOUGHT_LAST_40, "00 kein Kauf" = "")

#--------------------------------------------------------------------------------------------------------------------------#
                          ##Für Modellierung den aktuellen Status in "abgemeldet" und rest umwandeln

data$STATE_AKT = fct_explicit_na(data$STATE_AKT)
data$STATE_AKT = fct_collapse(data$STATE_AKT, "abgemeldet" = c("4", "6"), "akt/inakt" = c("1","7", "2", "3", "5", "(Missing)"))

#--------------------------------------------------------------------------------------------------------------------------#
                        ## Imputieren von Missing Data immer nach ähnlichsten der gleichen Gruppe (hier: State_Akt)

data$ALTER = unname(aregImpute(~ALTER + I(STATE_AKT), data = data, match = 'weighted', nk = 10, tlinear=F, n.impute = 1, x = T)$x[,1])
data$GENDER = unname(aregImpute(~GENDER + I(STATE_AKT), data = data, match = 'weighted', nk = 10, tlinear=F, n.impute = 1, x = T)$x[,1])
data$KUNDE_AKTIV_SEIT = unname(aregImpute(~KUNDE_AKTIV_SEIT + I(STATE_AKT), data = data, match = 'weighted', nk = 20, tlinear=F, n.impute = 1, x = T)$x[,1])
data$KREDITKUNDE = unname(aregImpute(~KREDITKUNDE + I(STATE_AKT), data = data, match = 'weighted', nk = 10, tlinear=F, n.impute = 1, x = T)$x[,1])
data$GOLDKUNDE = unname(aregImpute(~GOLDKUNDE + I(STATE_AKT), data = data, match = 'weighted', nk = 10, tlinear=F, n.impute = 1, x = T)$x[,1])
data$KSPERRE = unname(aregImpute(~KSPERRE + I(STATE_AKT), data = data, match = 'weighted', nk = 10, tlinear=F, n.impute = 1, x = T)$x[,1])

#--------------------------------------------------------------------------------------------------------------------------#
                        ## Personen, die noch nie geklickt/geöffnet/gekauft haben auf Erstellungsdatum setzen

data$LAST_CLICKED = ifelse(is.na(data$LAST_CLICKED), data$ACCOUNT_EXISTS_SINCE, data$LAST_CLICKED)
data$LAST_OPENED = ifelse(is.na(data$LAST_OPENED), data$ACCOUNT_EXISTS_SINCE, data$LAST_OPENED)
data$LAST_BUY_OR_RET = ifelse(is.na(data$LAST_BUY_OR_RET), data$ACCOUNT_EXISTS_SINCE, data$LAST_BUY_OR_RET)

#--------------------------------------------------------------------------------------------------------------------------#
                        ## Missing Values bei Kaufdaten auf 0 setzen (kein Kauf, kein Gutschein eingelöst, usw.)

data$NLBUYS_LAST_40_WEEKS = ifelse(is.na(data$NLBUYS_LAST_40_WEEKS), 0, data$NLBUYS_LAST_40_WEEKS)
data$N_PURCHASES_WEEK3 = ifelse(is.na(data$N_PURCHASES_WEEK3), 0, data$N_PURCHASES_WEEK3)
data$N_PURCHASES_WEEK12 = ifelse(is.na(data$N_PURCHASES_WEEK12), 0, data$N_PURCHASES_WEEK12)
data$N_PURCHASES_WEEK40 = ifelse(is.na(data$N_PURCHASES_WEEK40), 0, data$N_PURCHASES_WEEK40)
data$N_PURCHASES_TOTAL = ifelse(is.na(data$N_PURCHASES_TOTAL), 0, data$N_PURCHASES_TOTAL)
data$N_DISCOUNTS = ifelse(is.na(data$N_DISCOUNTS), 0, data$N_DISCOUNTS)
data$N_VOUCHERS = ifelse(is.na(data$N_VOUCHERS), 0, data$N_VOUCHERS)
data$BRUMS = ifelse(is.na(data$BRUMS) | data$BRUMS < 0, 0, data$BRUMS)
data$RETOUREN = ifelse(is.na(data$RETOUREN), 0, data$RETOUREN)

#--------------------------------------------------------------------------------------------------------------------------#
                        ## Berechnet Raten, wie oft Personen mit Gutschein gekauft haben und wie oft Personen vom NL zum Kauf gekommen sind

data$buys_to_discount_ratio = data %>% select(N_PURCHASES_TOTAL, N_DISCOUNTS, N_VOUCHERS) %>% mutate(buys_to_discount_ratio = ifelse(N_PURCHASES_TOTAL != 0, (N_DISCOUNTS + N_VOUCHERS)/N_PURCHASES_TOTAL, 0)) %>% select(buys_to_discount_ratio)
data$buys_to_nlbuys_ratio = data %>% select(N_PURCHASES_TOTAL, NLBUYS_LAST_40_WEEKS) %>% mutate(buys_to_nlbuys_ratio = ifelse(N_PURCHASES_TOTAL != 0, (NLBUYS_LAST_40_WEEKS)/N_PURCHASES_TOTAL, 0)) %>% select(buys_to_nlbuys_ratio)

#--------------------------------------------------------------------------------------------------------------------------#
                        ## Lädt Tabellen, wo jeder PLZ eine Dichte zugewiesen wird und jeder Nummer bei Custom4 was diese bedeutet


load("lookup_tables/lookup_PLZ_dichte.RData")
load("lookup_tables/lookup_custom4.RData")

lookup_PLZ_zu_dichte = function(x) { #Funktion zum zuordnen der Dichte
  value = unname(lookup_PLZ_dichte[paste0("", x, "")])
  return(value)
}

#--------------------------------------------------------------------------------------------------------------------------#
                        ## Zuweißen des Besiedelungsindex, kürzen der Faktornamen und NAs imputieren


data$wohnort_dichte = sapply(data$PLZ, lookup_PLZ_zu_dichte)
data$wohnort_dichte = as.factor(data$wohnort_dichte)
data$wohnort_dichte = fct_collapse(data$wohnort_dichte, dicht = "Dicht besiedelt (Grossstadt, Urbanes Zentrum)",
                                   duenn = "Duenn besiedelt (laendliche Region)",
                                   mitteldicht = "Mitteldicht besiedelt (Kleinstadt, Vorstadt)")
data$wohnort_dichte = unname(aregImpute(~wohnort_dichte + I(STATE_AKT), data = data, match = 'weighted', nk = 10, tlinear=F, n.impute = 1, x = T)$x[,1])


#--------------------------------------------------------------------------------------------------------------------------#
                        ## Extrahieren der Mailadressen von denen die Kunden kommen und alle die bei weniger als jeder 100. Person vorkommen 
                        ## zu "Other" zusammenfassen 


mailprov_helper = function(dataset, mailcol) {
  if(!require(pacman)) install.packages("pacman")
  pacman::p_load(reshape2, stringi)
  dataset[[mailcol]] = stri_replace_last_fixed(dataset[[mailcol]], ".", "?")
  x = unlist(colsplit((colsplit(dataset[[mailcol]], "@", c("name", "MP"))[2])$MP, "[?]", c("MP", "Land"))[1])
  x = as.factor(x)
  x = fct_lump(x, prop = c(.01))
  gc()
  return(x)
}

data$mailprov = mailprov_helper(data, "EMAIL")

data = data %>% select(-PLZ, -EMAIL)

#--------------------------------------------------------------------------------------------------------------------------#
                          ## Zuweisen der Namen zu den Nummern von CUSTOM4 und wieder Faktoren < 1x/100 Personen zu Other zusammenfassen


lookup_custom_helper = function(x) {
  value = unname(lookup_custom4[paste0("", x, "")])
  return(value)
}

clean_CUSTOM4_ = function(dataset, custom_col_name) {
  #Aufbereiten Custom 4 - Aufbau: immer Bezeichnung, woher Kunde kommt und dann CreationDate+KW 
  if(!require(pacman)) install.packages("pacman")
  pacman::p_load(reshape2, forcats)
  
  ## erstellen eines Look-Up-Tables
  
  x = sapply(dataset[[custom_col_name]], lookup_custom_helper)
  x = as.factor(x)
  x = fct_explicit_na(x)
  x = fct_lump(x, prop = c(.01))
  return(x)
}

data$custom4_ = clean_CUSTOM4_(data, "CUSTOM4")

#--------------------------------------------------------------------------------------------------------------------------#
                ## Wenn man einen Überblick über die Faktorenunterschiede haben möchte einfach nach describeBy x eingeben und Enter drücken
# 
# x = describeBy(x = select(data,-STATE_AKT), group = data$STATE_AKT, mat = T)

#--------------------------------------------------------------------------------------------------------------------------#
          ## Wenn man möchte kann man hier die Datei Zwischenspeichern

#write_csv(data, path = ""U:/Business Intelligence/Michi/data_40weeks_modellbau_cleaned.csv"")

#--------------------------------------------------------------------------------------------------------------------------#
                ## Entfernen der zum modellieren nicht verwendeten Spalte "KTNR"

data = data %>% select(-KTNR, -CUSTOM4)

#___________________________________________________________________________________________________________________________#
                                      #      VORBEREITEN DER DATEN MIT SMOTE
h2o.init(nthreads=-1, max_mem_size="4G")
data.h2o = as.h2o(data)
x = h2o.kmeans(training_frame = data.h2o, x = 1:39, k = 10, init = "PlusPlus", seed = "4829013303891719739")
data$clusters = as.data.table(h2o.predict(x, newdata = data.h2o))



weight_distribution = function(clustercol, statuscol) {
  cluster = as.factor(clustercol)
  status = as.factor(statuscol)
  dt = data.table(cluster, status)
  names(dt) = c("cluster", "status")
  dt2 = dt[status == "abgemeldet",]
  unique = unique(clustercol)
  weights = as.vector(NULL)
  counts = as.vector(NULL)
  length = length(clustercol)
  for(i in 1:length(unique)){
    counts[i] = ifelse(sum(dt2$cluster == unique[i]) != 0, sum(dt2$cluster == unique[i]), 0)
  }
  names(counts) = unique
  counts = counts[order(as.numeric(names(counts)))]
  for(i in 1:length(counts)) {
  weights[i] = ifelse(unname(counts[i])/length < 0.0001, 100, ifelse(unname(counts[i])/length < 0.0005, 80, ifelse(unname(counts[i])/length < 0.001, 60, ifelse(unname(counts[i])/length < 0.005, 40,1))))
  }
  weightcol = ifelse(dt$cluster == as.numeric(names(counts[1])) & dt$status == "abgemeldet", weights[1], 
                ifelse(dt$cluster == as.numeric(names(counts[2])) & dt$status == "abgemeldet", weights[2],
                  ifelse(dt$cluster == as.numeric(names(counts[3])) & dt$status == "abgemeldet", weights[3],
                    ifelse(dt$cluster == as.numeric(names(counts[4])) & dt$status == "abgemeldet", weights[4], 
                        ifelse(dt$cluster == as.numeric(names(counts[5])) & dt$status == "abgemeldet", weights[5], 
                          ifelse(dt$cluster == as.numeric(names(counts[6])) & dt$status == "abgemeldet", weights[6], 
                            ifelse(dt$cluster == as.numeric(names(counts[7])) & dt$status == "abgemeldet", weights[7], 
                              ifelse(dt$cluster == as.numeric(names(counts[8])) & dt$status == "abgemeldet", weights[8], 
                                ifelse(dt$cluster == as.numeric(names(counts[9])) & dt$status == "abgemeldet", weights[9], 
                                  ifelse(dt$cluster == as.numeric(names(counts[10])) & dt$status == "abgemeldet", weights[10], 
                                    ifelse(dt$cluster == as.numeric(names(counts[11])) & dt$status == "abgemeldet", weights[11], 
                                      ifelse(dt$cluster == as.numeric(names(counts[12])) & dt$status == "abgemeldet", weights[12], 
                                         ifelse(dt$cluster == as.numeric(names(counts[13])) & dt$status == "abgemeldet", weights[13], 
                                           ifelse(dt$cluster == as.numeric(names(counts[14])) & dt$status == "abgemeldet", weights[14], 
                                             ifelse(dt$cluster == as.numeric(names(counts[15])) & dt$status == "abgemeldet", weights[15], 
                                               ifelse(dt$cluster == as.numeric(names(counts[16])) & dt$status == "abgemeldet", weights[16],
                                                      ifelse(dt$cluster == as.numeric(names(counts[17])) & dt$status == "abgemeldet", weights[17],
                                                             ifelse(dt$cluster == as.numeric(names(counts[18])) & dt$status == "abgemeldet", weights[18],
                                                                    ifelse(dt$cluster == as.numeric(names(counts[19])) & dt$status == "abgemeldet", weights[19],
                                                                           ifelse(dt$cluster == as.numeric(names(counts[20])) & dt$status == "abgemeldet", weights[20],1))))))))))))))))))))
  return(weightcol)
}

data$weights = weight_distribution(data$clusters, data$STATE_AKT) 

index = createDataPartition(data$weights, p=.6, list = F)
trainSplit = data[index,] %>% select(-clusters)
testSplit = data[-index,]
index2 = createDataPartition(testSplit$STATE_AKT, p=.5, list = F)
test_red = testSplit[index2,] %>% select(-contains("weights"), -clusters)
validate_red = testSplit[-index2,] %>% select(-contains("weights"), -clusters)


smoted_train_red = DMwR::SMOTE(STATE_AKT~.,data = trainSplit, perc.over = 200, k = 2, perc.under = 1000)
smoted_valid = DMwR::SMOTE(STATE_AKT~.,data = validate_red, perc.over = 200, k = 3, perc.under = 1000)
smoted_test = DMwR::SMOTE(STATE_AKT~.,data = test_red, perc.over = 200, k = 3, perc.under = 1000)


#___________________________________________________________________________________________________________________________#
                                      #      MODELLIERUNG DER DATEN

h2o.init(nthreads=-1, max_mem_size="4G")
h2o.removeAll() ## clean slate - just in case the cluster was already running

train = as.h2o(smoted_train_red)
train_nat = as.h2o(trainSplit)
test = as.h2o(test_red)
valid_nat = as.h2o(validate_red)
smoted_test = as.h2o(smoted_test)
valid = as.h2o(smoted_valid)


#___________________________________________________________________________________________________________________________#
                                      #      TESTEN DER PERFEKTEN HYPERPARAMETER/Modellgrößen usw.
                          #rf_grid_1 -> Gewichtsverhältnis 1 zu 3
                          #rf_grid_2 -> Gewichtsverhältnis 1 zu 4
                          #rf_grid_3 -> Gewichtsverhältnis 1 zu 5

#------------------------------------------------ RANDOM FOREST ------------------------------#

h2o.grid("randomForest", x = c(1:33,35:39), y = 34, grid_id = "rf_grid_1", 
                   training_frame = train, validation_frame = valid, hyper_params = list(max_depth = seq(15,30), ntrees = seq(100,300,20)),
                   weights_column = "weights")
rf_gridperf_1  = h2o.getGrid(grid_id = "rf_grid_1", sort_by = "auc", decreasing = T)
rf_gridperf_1_2 = h2o.getGrid(grid_id = "rf_grid_1", sort_by = "logloss")
best_rf1_auc = h2o.getModel(rf_gridperf_1@model_ids[[1]])
best_rf1_ll = h2o.getModel(rf_gridperf_1_2@model_ids[[1]])
h2o.saveModel(best_rf1_auc, path = "U:/Business Intelligence/Michi/models/best_rf1_auc")
h2o.saveModel(best_rf1_ll, path = "U:/Business Intelligence/Michi/models/best_rf1_ll")
h2o.removeAll(retained_elements = c(train, valid, test))

h2o.grid("randomForest", x = c(1:33,35:39), y = 34, grid_id = "rf_grid_nat", 
         training_frame = train, validation_frame = valid, hyper_params = list(max_depth = seq(15,30), ntrees = seq(100,300,20)))
rf_gridperf_2  = h2o.getGrid(grid_id = "rf_grid_nat", sort_by = "auc", decreasing = T)
rf_gridperf_2_2 = h2o.getGrid(grid_id = "rf_grid_nat", sort_by = "logloss")
best_rf2_auc = h2o.getModel(rf_gridperf_2@model_ids[[1]])
best_rf2_ll = h2o.getModel(rf_gridperf_2_2@model_ids[[1]])
h2o.saveModel(best_rf2_auc, path = "U:/Business Intelligence/Michi/models/best_rfnat_auc")
h2o.saveModel(best_rf2_ll, path = "U:/Business Intelligence/Michi/models/best_rfnat_ll")
h2o.removeAll(retained_elements = c(train, valid, test))


# rf_model_3 = h2o.loadModel("U:/Business Intelligence/Michi/best_rf1_auc/rf_grid_1_model_53")
# rf_model_4 = h2o.loadModel("U:/Business Intelligence/Michi/models/best_rf4_auc/rf_grid_4_model_")
# h2o.performance(rf_model_1, newdata = test)
# h2o.performance(rf_model_2, newdata = test)
# h2o.performance(rf_model_3, newdata = test)


# h2o.grid("randomForest", x = c(1:33,35:39), y = 34, grid_id = "rf_grid_3", 
#          training_frame = train, validation_frame = valid, hyper_params = list(max_depth = seq(15,30), ntrees = seq(100,300,20)),
#          weights_column = "weights3")
# rf_gridperf_3  = h2o.getGrid(grid_id = "rf_grid_3", sort_by = "auc", decreasing = T)
# rf_gridperf_3_2 = h2o.getGrid(grid_id = "rf_grid_3", sort_by = "logloss")
# best_rf3_auc = h2o.getModel(rf_gridperf_3@model_ids[[1]])
# best_rf3_ll = h2o.getModel(rf_gridperf_3_2@model_ids[[1]])
# h2o.saveModel(best_rf3_auc, path = "U:/Business Intelligence/Michi/models/best_rf3_auc")
# h2o.saveModel(best_rf3_ll, path = "U:/Business Intelligence/Michi/models/best_rf3_ll")
# h2o.removeAll(retained_elements = c(train, valid, test))
# 
# 
# h2o.grid("randomForest", x = c(1:33,35:39), y = 34, grid_id = "rf_grid_4", 
#          training_frame = train, validation_frame = valid, hyper_params = list(max_depth = seq(15,30), ntrees = seq(100,300,20)),
#          weights_column = "weights4")
# rf_gridperf_4  = h2o.getGrid(grid_id = "rf_grid_4", sort_by = "auc", decreasing = T)
# rf_gridperf_4_2 = h2o.getGrid(grid_id = "rf_grid_4", sort_by = "logloss")
# best_rf4_auc = h2o.getModel(rf_gridperf_4@model_ids[[1]])
# best_rf4_ll = h2o.getModel(rf_gridperf_4_2@model_ids[[1]])
# h2o.saveModel(best_rf4_auc, path = "U:/Business Intelligence/Michi/models/best_rf4_auc")
# h2o.saveModel(best_rf4_ll, path = "U:/Business Intelligence/Michi/models/best_rf4_ll")
# h2o.removeAll(retained_elements = c(train, valid, test))

# 




#------------------------------------------------ GRADIENT BOOSTING MACHINE ------------------------------#

gbm_params2 <- list(learn_rate = seq(0.1, 0.05),
                    max_depth = seq(5, 15, 2),
                    ntrees = seq(50,150,15),
                    sample_rate = seq(0.5, 1.0),
                    col_sample_rate = seq(0.5, 1.0),
                    min_rows = seq(5,10))

search_criteria <- list(strategy = "RandomDiscrete", stopping_metric = "AUTO", stopping_tolerance = 0.0001, stopping_rounds = 10, max_models = 80)


h2o.grid("gbm", x = c(1:33,35:39), y = 34, grid_id = "gbm_grid_1", 
         training_frame = train, validation_frame = valid, hyper_params = gbm_params2, nfolds = 3,
         weights_column = "weights", search_criteria = search_criteria)
gbm_gridperf_1  = h2o.getGrid(grid_id = "gbm_grid_1", sort_by = "auc", decreasing = T)
gbm_gridperf_1_2 = h2o.getGrid(grid_id = "gbm_grid_1", sort_by = "logloss")
best_gbm1_auc = h2o.getModel(gbm_gridperf_1@model_ids[[1]])
best_gbm1_ll = h2o.getModel(gbm_gridperf_1_2@model_ids[[1]])
h2o.saveModel(best_gbm1_auc, path = "U:/Business Intelligence/Michi/models/best_gbm1_auc")
h2o.saveModel(best_gbm1_ll, path = "U:/Business Intelligence/Michi/models/best_gbm1_ll")
h2o.removeAll(retained_elements = c(train, valid, test, gbm_params2, search_criteria))


h2o.grid("gbm", x = c(1:33,35:39), y = 34, grid_id = "gbm_grid_nat", 
         training_frame = train, validation_frame = valid, hyper_params = gbm_params2, nfolds = 3,
         search_criteria = search_criteria) 
gbm_gridperf_2  = h2o.getGrid(grid_id = "gbm_grid_nat", sort_by = "auc", decreasing = T)
gbm_gridperf_2_2 = h2o.getGrid(grid_id = "gbm_grid_nat", sort_by = "logloss")
best_gbm2_auc = h2o.getModel(gbm_gridperf_2@model_ids[[1]])
best_gbm2_ll = h2o.getModel(gbm_gridperf_2_2@model_ids[[1]])
h2o.saveModel(best_gbm2_auc, path = "U:/Business Intelligence/Michi/models/best_gbmnat_auc")
h2o.saveModel(best_gbm2_ll, path = "U:/Business Intelligence/Michi/models/best_gbmnat_ll")
h2o.removeAll(retained_elements = c(train, valid, test, gbm_params2, search_criteria))

# h2o.grid("gbm", x = c(1:33,35:39), y = 34, grid_id = "gbm_grid_3", 
#          training_frame = train, validation_frame = valid, hyper_params = gbm_params2, nfolds = 3,
#          weights_column = "weights3", search_criteria = search_criteria)
# gbm_gridperf_3  = h2o.getGrid(grid_id = "gbm_grid_3", sort_by = "auc", decreasing = T)
# gbm_gridperf_3_2 = h2o.getGrid(grid_id = "gbm_grid_3", sort_by = "logloss")
# best_gbm3_auc = h2o.getModel(gbm_gridperf_3@model_ids[[1]])
# best_gbm3_ll = h2o.getModel(gbm_gridperf_3_2@model_ids[[1]])
# h2o.performance(best_gbm3_auc, test)
# h2o.performance(best_gbm3_auc, smoted_test)
# h2o.performance(best_gbm3_auc, natural_valid)
# h2o.saveModel(best_gbm3_auc, path = "U:/Business Intelligence/Michi/models/best_gbm3_auc")
# h2o.saveModel(best_gbm3_ll, path = "U:/Business Intelligence/Michi/models/best_gbm3_ll")
# h2o.removeAll(retained_elements = c(train, valid, test, gbm_params2, search_criteria))
# 
# 
# h2o.grid("gbm", x = c(1:33,35:39), y = 34, grid_id = "gbm_grid_4", 
#          training_frame = train, validation_frame = valid, hyper_params = gbm_params2,nfolds = 3,
#          weights_column = "weights4", search_criteria = search_criteria)
# gbm_gridperf_4  = h2o.getGrid(grid_id = "gbm_grid_4", sort_by = "auc", decreasing = T)
# gbm_gridperf_4_2 = h2o.getGrid(grid_id = "gbm_grid_4", sort_by = "logloss")
# best_gbm4_auc = h2o.getModel(gbm_gridperf_4@model_ids[[1]])
# best_gbm3_ll = h2o.getModel(gbm_gridperf_4_2@model_ids[[1]])
# h2o.saveModel(best_gbm4_auc, path = "U:/Business Intelligence/Michi/models/best_gbm4_auc")
# h2o.saveModel(best_gbm4_ll, path = "U:/Business Intelligence/Michi/models/best_gbm4_ll")
# h2o.removeAll(retained_elements = c(train, valid, test, gbm_params2, search_criteria))


#------------------------------------------------ DEEP LEARNING ------------------------------------------------------------#

dl_model = h2o.deeplearning(model_id = "dl_model", x = c(1:33,35:39), y = 34, training_frame = train, validation_frame = valid,
         epochs = 10000, momentum_start = .5, adaptive_rate = F, input_dropout_ratio = .4, stopping_rounds = 10, variable_importances = T, weights_column = "weights",
         export_checkpoints_dir = "U:/Business Intelligence/Michi/models/dl_model_new")



#------------------------------------------------ NAIVE BAYES ---------------------------------------------------------------#

nb_model = h2o.naiveBayes(model_id = "naiveBayes", x = c(1:33,35:39), y = 34, nfolds = 3, training_frame = train, validation_frame = valid, laplace = 3, 
               export_checkpoints_dir = "U:/Business Intelligence/Michi/models/naiveBayes")
x = as.data.table(h2o.predict(nb_model, test))
#------------------------------------------------ Generalized Linear Model --------------------------------------------------#

h2o.glm(model_id = "glm_binom", x = c(1:33,35:39), y = 34, training_frame = train, validation_frame = valid, nfolds = 3,
         family = "binomial", weights_column = "weights", remove_collinear_columns = T, prior = .05, lambda_search = T,
        export_checkpoints_dir = "U:/Business Intelligence/Michi/models/glm_model")


#_______________________________________________________________________________________________________________________________________#

#                                                 Baue das Modell 2-ter Stufe

#_______________________________________________________________________________________________________________________________________#

automl = h2o.automl(x = c(1:33,35:39), y = 34, training_frame = train, weights_column = "weights", export_checkpoints_dir = "U:/Business Intelligence/Michi/models/AutoMl/")
automl_model = h2o.getAutoML("automl_smoted_train_red_sid_92fb_31")
automl_model_final = h2o.getModel("StackedEnsemble_AllModels_AutoML_20190820_103403")
x = data.table(actual_col_test, as.data.table(h2o.predict(automl_model_final, test)))

actual_col_train = smoted_train_red$STATE_AKT  
actual_col_test = test_red$STATE_AKT
actual_col_valid = validate_red$STATE_AKT

ensemble = h2o.stackedEnsemble(x = c(1:33,35:39), y = 34, 
                               training_frame = train, 
                               model_id = "ensemble_learner_glm", 
                               blending_frame = valid_nat,
                               metalearner_algorithm = "glm",
                               base_models = list(rf_nat, rf_weight_auc, rf_weight_ll, gbm_nat_auc, gbm_nat_ll, gbm_weight_ll, gbm_weight_auc, dl_model, glm_model_weight, naiveBayes_model)
                               )
ensemble2 = h2o.stackedEnsemble(x = c(1:33,35:39), y = 34, 
                               training_frame = train, 
                               model_id = "ensemble_learner_gbm", 
                               blending_frame = valid_nat,
                               metalearner_algorithm = "gbm",
                               base_models = list(rf_nat, rf_weight_auc, rf_weight_ll, gbm_nat_auc, gbm_nat_ll, gbm_weight_ll, gbm_weight_auc, dl_model, glm_model_weight, naiveBayes_model)
)
                               
ensemble3 = h2o.stackedEnsemble(x = c(1:33,35:39), y = 34, 
                                training_frame = train, 
                                model_id = "ensemble_learner_drf", 
                                blending_frame = valid_nat,
                                metalearner_algorithm = "drf",
                                base_models = list(rf_nat, rf_weight_auc, rf_weight_ll, gbm_nat_auc, gbm_nat_ll, gbm_weight_ll, gbm_weight_auc, dl_model, glm_model_weight, naiveBayes_model)
)

ensemble4 = h2o.stackedEnsemble(x = c(1:33,35:39), y = 34, 
                                training_frame = train, 
                                model_id = "ensemble_learner_dl", 
                                blending_frame = valid_nat,
                                metalearner_algorithm = "deeplearning",
                                metalearner_params = list(hidden = c(15,15), epochs = 1000, input_dropout_ratio = .2, hidden_dropout_ratios = c(.2,.2), activation = "TanhWithDropout"),
                                base_models = list(rf_nat, rf_weight_auc, rf_weight_ll, gbm_nat_auc, gbm_nat_ll, gbm_weight_ll, gbm_weight_auc, dl_model, glm_model_weight, naiveBayes_model)
)
x = data.table(actual_col_valid, as.data.table(h2o.predict(ensemble3, valid_nat)))


rf_nat = h2o.loadModel("U:/Business Intelligence/Michi/models/best_rfnat_auc/rf_grid_nat_model_146")
rf_weight_auc = h2o.loadModel("U:/Business Intelligence/Michi/models/best_rf1_auc/rf_grid_1_model_95")
rf_weight_ll = h2o.loadModel("U:/Business Intelligence/Michi/models/best_rf1_ll/rf_grid_1_model_175")
gbm_nat_auc = h2o.loadModel("U:/Business Intelligence/Michi/models/best_gbmnat_auc/gbm_grid_nat_model_27")
gbm_nat_ll = h2o.loadModel("U:/Business Intelligence/Michi/models/best_gbmnat_ll/gbm_grid_nat_model_58")
gbm_weight_ll = h2o.loadModel("U:/Business Intelligence/Michi/models/best_gbm1_ll/gbm_grid_1_model_19")
gbm_weight_auc = h2o.loadModel("U:/Business Intelligence/Michi/models/best_gbm1_auc/gbm_grid_1_model_43")
dl_model = h2o.loadModel("U:/Business Intelligence/Michi/models/dl_model_new/dl_model") 
glm_model_weight = h2o.loadModel("U:/Business Intelligence/Michi/models/glm_model/glm_binom")
naiveBayes_model = h2o.loadModel("U:/Business Intelligence/Michi/models/naiveBayes/naiveBayes")

merged_train = data.table(
          as.data.table(h2o.predict(rf_nat, train)[c("abgemeldet", "akt/inakt")]),
          as.data.table(h2o.predict(rf_weight_auc, train)[c("abgemeldet", "akt/inakt")]),
          as.data.table(h2o.predict(rf_weight_ll, train)[c("abgemeldet", "akt/inakt")]),
          as.data.table(h2o.predict(gbm_nat_auc, train)[c("abgemeldet", "akt/inakt")]),
          as.data.table(h2o.predict(gbm_nat_ll, train)[c("abgemeldet", "akt/inakt")]),
          as.data.table(h2o.predict(gbm_weight_ll, train)[c("abgemeldet", "akt/inakt")]),
          as.data.table(h2o.predict(gbm_weight_auc, train)[c("abgemeldet", "akt/inakt")]),
          as.data.table(h2o.predict(dl_model, train)[c("abgemeldet", "akt/inakt")]),
          as.data.table(h2o.predict(glm_model_weight, train)[c("abgemeldet", "akt/inakt")]),
          as.data.table(h2o.predict(naiveBayes_model, train)[c("abgemeldet", "akt/inakt")]),
          actual_col_train)
names(merged_train) = c("rf_nat_abg", "rf_nat_akt.inakt", "rf_weight_auc_abg", "rf_weight_auc_akt.inakt",
                  "rf_weight_ll_abg", "rf_weight_ll_akt.inakt", "gbm_nat_auc_abg", "gbm_nat_auc_akt.inakt",
                  "gbm_nat_ll_abg", "gbm_nat_ll_akt.inakt", "gbm_weight_ll_abg", "gbm_weight_ll_akt.inakt",
                  "gbm_weight_auc_abg", "gbm_weight_auc_akt.inakt", "dl_model_abg", "dl_model_akt.inakt",
                  "glm_model_weight_abg", "glm_model_weight_akt.inakt", "naiveBayes_model_abg", "naiveBayes_model_akt.inakt", "actual")

merged_train.h2o = as.h2o(merged_train)



merged_valid = data.table(
  as.data.table(h2o.predict(rf_nat, valid_nat)[c("abgemeldet", "akt/inakt")]),
  as.data.table(h2o.predict(rf_weight_auc, valid_nat)[c("abgemeldet", "akt/inakt")]),
  as.data.table(h2o.predict(rf_weight_ll, valid_nat)[c("abgemeldet", "akt/inakt")]),
  as.data.table(h2o.predict(gbm_nat_auc, valid_nat)[c("abgemeldet", "akt/inakt")]),
  as.data.table(h2o.predict(gbm_nat_ll, valid_nat)[c("abgemeldet", "akt/inakt")]),
  as.data.table(h2o.predict(gbm_weight_ll, valid_nat)[c("abgemeldet", "akt/inakt")]),
  as.data.table(h2o.predict(gbm_weight_auc, valid_nat)[c("abgemeldet", "akt/inakt")]),
  as.data.table(h2o.predict(dl_model, valid_nat)[c("abgemeldet", "akt/inakt")]),
  as.data.table(h2o.predict(glm_model_weight, valid_nat)[c("abgemeldet", "akt/inakt")]),
  as.data.table(h2o.predict(naiveBayes_model, valid_nat)[c("abgemeldet", "akt/inakt")]),
  actual_col_valid)

names(merged_valid) = c("rf_nat_abg", "rf_nat_akt.inakt", "rf_weight_auc_abg", "rf_weight_auc_akt.inakt",
                        "rf_weight_ll_abg", "rf_weight_ll_akt.inakt", "gbm_nat_auc_abg", "gbm_nat_auc_akt.inakt",
                        "gbm_nat_ll_abg", "gbm_nat_ll_akt.inakt", "gbm_weight_ll_abg", "gbm_weight_ll_akt.inakt",
                        "gbm_weight_auc_abg", "gbm_weight_auc_akt.inakt", "dl_model_abg", "dl_model_akt.inakt",
                        "glm_model_weight_abg", "glm_model_weight_akt.inakt", "naiveBayes_model_abg", "naiveBayes_model_akt.inakt", "actual")
merged_valid.h2o = as.h2o(merged_valid)

                               


train_on_train_data <- data.table(
  pred_rf_nat = as.factor(as.data.table(h2o.predict(rf_nat, train))$predict),
  pred_rf_weight_auc = as.factor(as.data.table(h2o.predict(rf_weight_auc, train))$predict),
  pred_rf_weight_ll = as.factor(as.data.table(h2o.predict(rf_weight_ll, train))$predict),
  pred_gbm_nat_auc = as.factor(as.data.table(h2o.predict(gbm_nat_auc, train))$predict),
  pred_gbm_nat_ll = as.factor(as.data.table(h2o.predict(gbm_nat_ll, train))$predict),
  pred_gbm_weight_ll = as.factor(as.data.table(h2o.predict(gbm_weight_ll, train))$predict),
  pred_gbm_weight_auc = as.factor(as.data.table(h2o.predict(gbm_weight_auc, train))$predict),
  pred_dl_model = as.factor(as.data.table(h2o.predict(dl_model, train))$predict),
  pred_glm_model = as.factor(as.data.table(h2o.predict(glm_model_weight, train))$predict),
  pred_naiveBayes_model = as.factor(as.data.table(h2o.predict(naiveBayes_model, train))$predict),
  actual = actual_col_train
) 

train_final.h2o = as.h2o(train_on_train_data)

train_on_valid_data <- data.table(
  pred_rf_nat = as.factor(as.data.table(h2o.predict(rf_nat, valid_nat))$predict),
  pred_rf_weight_auc = as.factor(as.data.table(h2o.predict(rf_weight_auc, valid_nat))$predict),
  pred_rf_weight_ll = as.factor(as.data.table(h2o.predict(rf_weight_ll, valid_nat))$predict),
  pred_gbm_nat_auc = as.factor(as.data.table(h2o.predict(gbm_nat_auc, valid_nat))$predict),
  pred_gbm_nat_ll = as.factor(as.data.table(h2o.predict(gbm_nat_ll, valid_nat))$predict),
  pred_gbm_weight_ll = as.factor(as.data.table(h2o.predict(gbm_weight_ll, valid_nat))$predict),
  pred_gbm_weight_auc = as.factor(as.data.table(h2o.predict(gbm_weight_auc, valid_nat))$predict),
  pred_dl_model = as.factor(as.data.table(h2o.predict(dl_model, valid_nat))$predict),
  pred_glm_model = as.factor(as.data.table(h2o.predict(glm_model_weight, valid_nat))$predict),
  pred_naiveBayes_model = as.factor(as.data.table(h2o.predict(naiveBayes_model, valid_nat))$predict),
  actual = actual_col_valid
) 

valid_final.h2o = as.h2o(train_on_valid_data)

train_on_test_data <- data.table(
  pred_rf_nat = as.factor(as.data.table(h2o.predict(rf_nat, test))$predict),
  pred_rf_weight_auc = as.factor(as.data.table(h2o.predict(rf_weight_auc, test))$predict),
  pred_rf_weight_ll = as.factor(as.data.table(h2o.predict(rf_weight_ll, test))$predict),
  pred_gbm_nat_auc = as.factor(as.data.table(h2o.predict(gbm_nat_auc, test))$predict),
  pred_gbm_nat_ll = as.factor(as.data.table(h2o.predict(gbm_nat_ll, test))$predict),
  pred_gbm_weight_ll = as.factor(as.data.table(h2o.predict(gbm_weight_ll, test))$predict),
  pred_gbm_weight_auc = as.factor(as.data.table(h2o.predict(gbm_weight_auc, test))$predict),
  pred_dl_model = as.factor(as.data.table(h2o.predict(dl_model, test))$predict),
  pred_glm_model = as.factor(as.data.table(h2o.predict(glm_model_weight, test))$predict),
  pred_naiveBayes_model = as.factor(as.data.table(h2o.predict(naiveBayes_model, test))$predict),
  actual = actual_col_test
)  

test_final.h2o = as.h2o(train_on_test_data)


model_with_preds <- h2o.gbm(
  model_id = 'stacked_with_preds',
  training_frame = train_final.h2o,
  validation_frame = valid_final.h2o,
  score_each_iteration = T, 
  x = 1:10,
  y = 11,
  export_checkpoints_dir = "U:/Business Intelligence/Michi/models/stacked_dl_with_probs"
)    

x = data.table(actual_col_test, as.data.table(h2o.predict(model_with_preds, test_final.h2o)))


model_weights4 <- h2o.gbm(
  model_id = 'gbm_stacked_weights4',
  
  training_frame = train_weights4,
  
  x = 1:6,
  y = 7
)

model_weights5 <- h2o.gbm(
  model_id = 'gbm_stacked_weights5',
  
  training_frame = train_weights5,
  
  x = 1:6,
  y = 7
)

model_weights10 <- h2o.gbm(
  model_id = 'gbm_stacked_weights10',
  
  training_frame = train_weights6,
  
  x = 1:6,
  y = 7
)





#h2o.saveModel(model_super, paste(getwd(),'/tier2_modelle', sep = ''), force = T)


#----------------------------------------------------------------------------------------------------------------------------------------------------------------------#

# Evaluiere dieses auf den Test Daten

#model_super <- h2o.loadModel(paste(getwd(), '/tier2_modelle/Z_tier2_model', sep = ''))

preds_weights3 <- data.table(
  pred_rf = as.vector(h2o.predict(rf_model_1,test)$predict),
  pred_gbm = as.vector(h2o.predict(gbm_model_1, test)$predict),
  pred_glm = as.vector(h2o.predict(glm_model, test)$predict),
  pred_km = as.vector(h2o.predict(knn_model, test)$predict),
  pred_naive = as.vector(h2o.predict(naive_model, test)$predict),
  pred_deep = as.vector(h2o.predict(deep_model, test)$predict),
  actual = test_red$STATE_AKT
) %>% as.h2o()

preds_weights4 <- data.table(
  pred_rf = as.vector(h2o.predict(rf_model_2,test)$predict),
  pred_gbm = as.vector(h2o.predict(gbm_model_2, test)$predict),
  pred_glm = as.vector(h2o.predict(glm_model, test)$predict),
  pred_km = as.vector(h2o.predict(knn_model, test)$predict),
  pred_naive = as.vector(h2o.predict(naive_model, test)$predict),
  pred_deep = as.vector(h2o.predict(deep_model, test)$predict),
  actual = test_red$STATE_AKT
) %>% as.h2o()

preds_weights5 <- data.table(
  pred_rf = as.vector(h2o.predict(rf_model_3,test)$predict),
  pred_gbm = as.vector(h2o.predict(gbm_model_3, test)$predict),
  pred_glm = as.vector(h2o.predict(glm_model, test)$predict),
  pred_km = as.vector(h2o.predict(knn_model, test)$predict),
  pred_naive = as.vector(h2o.predict(naive_model, test)$predict),
  pred_deep = as.vector(h2o.predict(deep_model, test)$predict),
  actual = test_red$STATE_AKT
) %>% as.h2o()

preds_weights10 <- data.table(
  pred_rf = as.vector(h2o.predict(rf_model_4,test)$predict),
  pred_gbm = as.vector(h2o.predict(gbm_model_4, test)$predict),
  pred_glm = as.vector(h2o.predict(glm_model, test)$predict),
  pred_km = as.vector(h2o.predict(knn_model, test)$predict),
  pred_naive = as.vector(h2o.predict(naive_model, test)$predict),
  pred_deep = as.vector(h2o.predict(deep_model, test)$predict),
  actual = test_red$STATE_AKT
) %>% as.h2o()

h2o.performance(model_weights3, preds_weights3)
h2o.performance(model_weights4, preds_weights4)
h2o.performance(model_weights5, preds_weights5)
h2o.performance(model_weights10, preds_weights10)

prediction <- as.data.table(h2o.predict(model_super, preds_tierI))

crossval::confusionMatrix(test_daten$grp_akt,prediction$predict, negative = 'ohne') %>% crossval::diagnosticErrors()













