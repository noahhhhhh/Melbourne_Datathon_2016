# setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_2016/kaggle/noah is a soy sause/")
rm(list = ls()); gc();
require(data.table)
require(tm)
require(SnowballC)
require(stringr)
require(parallel)
require(Matrix)
require(ggplot2)
source("utility.R")
load("dt_jobs.RData")
load("dt_keywords_hat_test.RData")
load("dt_keywords_hat_all.RData")
#######################################################################################
## rm sparse version - test terms #####################################################
#######################################################################################
options(mc.cores=1)
dtm.title.test <- DocumentTermMatrix(Corpus(DataframeSource(as.data.frame(list.title.hat.test[["dt"]]))), control = list(minWordLength = 2, minDocFreq = 3))
col.title.test <- dtm.title.test$dimnames$Terms

options(mc.cores=1)
dtm.abstract.test <- DocumentTermMatrix(Corpus(DataframeSource(as.data.frame(list.abstract.hat.test[["dt"]]))), control = list(minWordLength = 2, minDocFreq = 3))
col.abstract.test <- dtm.abstract.test$dimnames$Terms

options(mc.cores=1)
dtm.raw_job_type.test <- DocumentTermMatrix(Corpus(DataframeSource(as.data.frame(list.raw_job_type.hat.test[["dt"]]))), control = list(minWordLength = 2, minDocFreq = 3))
col.raw_job_type.test <- dtm.raw_job_type.test$dimnames$Terms

#######################################################################################
## rm sparse version - all terms ######################################################
#######################################################################################
# dtm.title.all <- DocumentTermMatrix(Corpus(DataframeSource(as.data.frame(list.title.hat.all[["dt"]]))), control = list(minWordLength = 2, minDocFreq = 3))
options(mc.cores=1)
dtm.title.all <- DocumentTermMatrix(Corpus(DataframeSource(list.title.hat.all[["dt"]])), control = list(minWordLength = 2, minDocFreq = 3))
col.title.all <- dtm.title.all$dimnames$Terms
length(col.title.all)
save(dtm.title.all, file = "dtm_title_all.RData")

options(mc.cores=1)
dtm.abstract.all <- DocumentTermMatrix(Corpus(DataframeSource(list.abstract.hat.all[["dt"]])), control = list(minWordLength = 2, minDocFreq = 3))
col.abstract.all <- dtm.abstract.all$dimnames$Terms
length(col.abstract.all)
save(dtm.abstract.all, file = "dtm_abstract_all.RData")

options(mc.cores=1)
dtm.raw_job_type.all <- DocumentTermMatrix(Corpus(DataframeSource(list.raw_job_type.hat.all[["dt"]])), control = list(minWordLength = 2, minDocFreq = 3))
col.raw_job_type.all <- dtm.raw_job_type.all$dimnames$Terms
length(col.raw_job_type.all)
save(dtm.raw_job_type.all, file = "dtm_raw_job_type_all.RData")

#######################################################################################
## rm sparse version - only use test terms ############################################
#######################################################################################
length(setdiff(col.title.all, col.title.test))
length(col.title.all)
length(col.title.test)

length(setdiff(col.abstract.all, col.abstract.test))
length(col.abstract.all)
length(col.abstract.test)

length(setdiff(col.raw_job_type.all, col.raw_job_type.test))
length(col.raw_job_type.all)
length(col.raw_job_type.test)

dtm.title.all.useTest <- dtm.title.all[, col.title.test]
inspect(dtm.title.all[1:5, col.title.test[1:5]])

dtm.abstract.all.useTest <- dtm.abstract.all[, col.abstract.test]
inspect(dtm.abstract.all[1:5, col.abstract.test[1:5]])

dtm.raw_job_type.all.useTest <- dtm.raw_job_type.all[, col.raw_job_type.test]
inspect(dtm.raw_job_type.all.useTest[1:5, col.raw_job_type.test[1:5]])

save(dtm.title.all, dtm.abstract.all, dtm.raw_job_type.all, file = "dtm_all.RData")
save(dtm.title.test, dtm.abstract.test, dtm.raw_job_type.test, file = "dtm_test.RData")

#######################################################################################
## sparseMatrix and Cbind more cols ###################################################
#######################################################################################
sm.title.all.useTest <- sparseMatrix(i = dtm.title.all.useTest$i
                                     , j = dtm.title.all.useTest$j
                                     , x = dtm.title.all.useTest$v
                                     , dims = c(dtm.title.all.useTest$nrow, dtm.title.all.useTest$ncol))

sm.abstract.all.useTest <- sparseMatrix(i = dtm.abstract.all.useTest$i
                                        , j = dtm.abstract.all.useTest$j
                                        , x = dtm.abstract.all.useTest$v
                                        , dims = c(dtm.abstract.all.useTest$nrow, dtm.abstract.all.useTest$ncol))

sm.raw_job_type.all.useTest <- sparseMatrix(i = dtm.raw_job_type.all.useTest$i
                                            , j = dtm.raw_job_type.all.useTest$j
                                            , x = dtm.raw_job_type.all.useTest$v
                                            , dims = c(dtm.raw_job_type.all.useTest$nrow, dtm.raw_job_type.all.useTest$ncol))

## add salary
log.salary_min <- log(dt.jobs$salary_min)
log.salary_min <- ifelse(is.na(log.salary_min), mean(log.salary_min, na.rm = T), log.salary_min)
log.salary_min.scale <- range01(log.salary_min)

log.salary_max <- log(dt.jobs$salary_max)
log.salary_max <- ifelse(is.na(log.salary_max), mean(log.salary_max, na.rm = T), log.salary_max)
log.salary_max.scale <- range01(log.salary_max)

log.salary_mean <- ifelse(is.na(log((dt.jobs$salary_max + dt.jobs$salary_min) / 2))
                          , mean(log((dt.jobs$salary_max + dt.jobs$salary_min) / 2), na.rm = T)
                          , log((dt.jobs$salary_max + dt.jobs$salary_min) / 2)
)
log.salary_mean.scale <- range01(log.salary_mean)

log.salary_diff <- abs(ifelse(is.na((dt.jobs$salary_max - dt.jobs$salary_min))
                              , mean((dt.jobs$salary_max - dt.jobs$salary_min), na.rm = T)
                              , (dt.jobs$salary_max - dt.jobs$salary_min)
))
log.salary_diff.scale <- range01(log.salary_diff)


sm.all.useTest2 <- cBind(matrix(ifelse(dt.jobs$salary_type == "y", 1, 0), nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_type_y"))
                         , matrix(ifelse(dt.jobs$salary_type == "h", 1, 0), nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_type_h"))
                         , matrix(ifelse(dt.jobs$salary_type == "-1", -1, 0), nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_type__1"))
                         , matrix(log.salary_min, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_min"))
                         , matrix(log.salary_max, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_max"))
                         , matrix(log.salary_mean, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_mean"))
                         , matrix(log.salary_diff, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_diff"))
                         , sm.title.all.useTest
                         , sm.abstract.all.useTest
                         , sm.raw_job_type.all.useTest)

save(sm.title.all.useTest2, file = "sm_title_all_useTest2.RData")
save(sm.abstract.all.useTest, file = "sm_abstract_all_useTest.RData")
save(sm.raw_job_type.all.useTest, file = "sm_raw_job_type_all_useTest.RData")
save(sm.all.useTest2, file = "sm_all_useTest2.RData")














