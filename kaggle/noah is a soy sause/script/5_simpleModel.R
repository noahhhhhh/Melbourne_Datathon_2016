# setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_2016/kaggle/noah is a soy sause/")
# rm(list = ls()); gc();
require(data.table)
require(tm)
require(SnowballC)
require(stringr)
require(parallel)
require(Matrix)
require(ggplot2)
require(caret)
require(xgboost)
require(Metrics)
require(caTools)
require(FeatureHashing)
source("utility.R")
load("dt_jobs_brief.RData")
# load("sm_title_all_useTest2.RData")
# load("sm_all_useTest2.RData")
# load("sm_all_useTest2_withLoc.RData")
# load("sm_all_useTest2.RData")
dt.jobs <- dt.jobs.brief
#######################################################################################
## train, valid, test #################################################################
#######################################################################################
set.seed(888)
ind.train <- createDataPartition(dt.jobs[hat >= 0]$hat, p = .8, list = F)
ind.train <- as.numeric(ind.train)
sm.train.all <- sm.all.useTest2[dt.jobs$hat >= 0, ][ind.train, ]
y.train.all <- dt.jobs$hat[dt.jobs$hat >= 0][ind.train]
sm.valid.all <- sm.all.useTest2[dt.jobs$hat >= 0, ][-ind.train, ]
y.valid.all <- dt.jobs$hat[dt.jobs$hat >= 0][-ind.train]
sm.test.all <- sm.all.useTest2[dt.jobs$hat < 0, ]

dim(sm.train.all); dim(sm.valid.all); dim(sm.test.all);
length(y.train.all); length(y.valid.all)
nrow(dt.jobs[dt.jobs$hat >= 0][ind.train]);nrow(dt.jobs[dt.jobs$hat >= 0][-ind.train]);nrow(dt.jobs[dt.jobs$hat < 0]);

#######################################################################################
## xgb ################################################################################
#######################################################################################
## dmx
gc();
dmx.train <- xgb.DMatrix(data = sm.train.all, label = y.train.all)
dmx.valid <- xgb.DMatrix(data = sm.valid.all, label = y.valid.all)
dmx.test <- xgb.DMatrix(data = sm.test.all, label = rep(0, nrow(sm.test.all)))
## watchlist
watchlist <- list(valid = dmx.valid, train = dmx.train)
## params
params <- list(booster = "gbtree"
               , nthread = 36
               # , min_child_weight = 10
               , objective = "binary:logistic"
               , eval_metric = "auc"
               , max_depth = 12 # 20
               , subsample = 1 # .9
               # , gamma = .1
               , colsample_bylevel = .3 #.4
               , eta = .2
)

## train
set.seed(888)
md.xgb <- xgb.train(params = params
                    , data = dmx.train
                    , nrounds = 100000
                    , early.stop.round = 50
                    , watchlist = watchlist
                    , print.every.n = 50
                    , verbose = T)

pred.valid <- predict(md.xgb, dmx.valid)
score.valid <- 2 * as.numeric(colAUC(pred.valid, as.numeric(y.valid.all))) - 1
print(paste("gini valid:", score.valid))
# gini: .965, raw params, with only salary and title
# gini: 0.983508726941559, raw params, with salary and all
# gini: 0.98372702404728, raw params, with salary and location and all
# gini: 0.929..., raw params, with salary and location and count and all
# gini: 0.98351530762602, raw params, with salary and location (removed city) and all (combined title and abstract)
# gini: 0.983487490729311, raw params, with salary and location and all (combined title and abstract)
# gini: 0.982878664753845, raw params, with salary and location and is.dup and all
# gini: 0.986772942217905, 16 md, with salary and location and 1 and 2 gram all
# gini: .9860..., 10 md, with salary and location and modified 1 and 2 gram all
# gini: 0.986608653918384, 16 md, with salary and location and modified 1 and 2 gram all 
# gini: 0.987138991506605, 24 md, with salary and location and modified 1 and 2 gram all
# * gini: 0.987007468277948, 20 md, with salary and location and modified 1 and 2 gram all, 882 rounds
# gini: 0.992145288495381, 20 md, with salary and location and jobs.users.impressions and modified 1 and 2 gram all, 664 rounds
# gini: 0.987007468277948, 20 md, with salary and location and modified 1 and 2 gram all
# gini: 987021969857646, 20 md, with salary and location and modified 1 and 2 and 3 gram all, 1013 rounds
# gini: 0.986494277277413, 20 md, with salary and location and 1 gram all, 588 rounds
# gini: 0.986949975914574 20 md, with salary and location and meta seg and modified 1 and 2 gram all
# gini: 0.987008860416387 20 md, gamma .2, with salary and location and modified 1 and 2 gram all, 893 rounds
# gini: 0.927022570846031 20 md, with salary and location and 3 gram, 4318 rounds
# gini: 0.9864... 20 md, with salary and location and 1 and 2 gram, removed nzv, ~900 rounds
# gini: 0.986752664190306, 20 md, with salary and location and 1 and 2 and 3 gram, removed nzv, 831 rounds
# gini: 0.986858468179648, 12 md, 1 ss, .3 csbl, and 1 and 2 and 3 gram, 1822 rounds

## train on full set
sm.train.all <- sm.all.useTest2[dt.jobs$hat >= 0, ]
y.train.all <- dt.jobs$hat[dt.jobs$hat >= 0]
dmx.train <- xgb.DMatrix(data = sm.train.all, label = y.train.all)
dmx.test <- xgb.DMatrix(data = sm.test.all, label = rep(0, nrow(sm.test.all)))
params <- list(booster = "gbtree"
               , nthread = 36
               , objective = "binary:logistic"
               , eval_metric = "auc"
               , max_depth = 20
               , subsample = .9
               , colsample_bylevel = .4
               , eta = .2
)
watchlist <- list(train = dmx.train)
## train on full set with diff seed
reps <- 10
vec.pred.test <- rep(0, nrow(sm.test.all))
for(rep in 1:reps){
    set.seed(rep * 888)
    md.xgb <- xgb.train(params = params
                        , data = dmx.train
                        , nrounds = 882
                        # , early.stop.round = 50
                        , watchlist = watchlist
                        , print.every.n = 50
                        , verbose = T)
    vec.pred.test <- vec.pred.test + predict(md.xgb, dmx.test) / reps
}

# pred.test.1gram <- vec.pred.test / reps
# save(pred.test.1gram, file = "temp/pred_test_1gram.RData")
# pred.test.2gram <- vec.pred.test / reps
# save(pred.test.2gram, file = "temp/pred_test_2gram.RData")
#######################################################################################
## sbmit ##############################################################################
#######################################################################################
# pred.test <- predict(md.xgb, dmx.test)
pred.test <- vec.pred.test
submit <- data.table(job_id = dt.jobs[dt.jobs$hat < 0]$job_id, hat = pred.test)
write.csv(submit, file = "submission/12_10_xgb_on_all_train_md_20_salary_location_all_1_and_2_gram.csv", row.names = F, quote = F)
# gini: 0.98337, raw params, with salary and all
# gini: 0.98658, 16 md, with salary and location and 1 and 2 gram all
# gini: 0.98660, 24 md, with salary and location and modified 1 and 2 gram all
# gini: 0.98706, 20 md, with salary and location and modified 1 and 2 gram all, 882 rounds
# * gini: 0.98839, on all train, 20 md, with salary and location and modified 1 and 2 gram all, 882 rounds
# gini: 0.98581, 20 md, with salary and location and jobs.users.impressions and modified 1 and 2 gram all, 664 rounds
# gini: 0.98669, on 80 train, 20 md, with salary and location and modified 1 and 2 and 3 gram all, 1013 rounds
