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
               , nthread = 8
               , objective = "binary:logistic"
               , eval_metric = "auc"
               , max_depth = 16
               , subsample = .9
               , colsample_bylevel = .4
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

#######################################################################################
## sbmit ##############################################################################
#######################################################################################
pred.test <- predict(md.xgb, dmx.test)
submit <- data.table(job_id = dt.jobs[dt.jobs$hat < 0]$job_id, hat = pred.test)
write.csv(submit, file = "submission/1_single_xgb_raw_params_salary_all_1_gram.csv", row.names = F, quote = F)
# gini: 0.98337, raw params, with salary and all




