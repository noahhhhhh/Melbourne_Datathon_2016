# setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_2016/kaggle/noah is a soy sause/")
rm(list = ls()); gc();
require(data.table)
require(tm)
require(SnowballC)
require(stringr)
require(parallel)
require(Matrix)
require(ggplot2)
require(slam)
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
load("dtm_all.RData")
load("dtm_test.RData")
load("dtm_bi_all.RData")
load("dtm_bi_test.RData")

MyUseTest <- function(dtm.title.all, dtm.abstract.all, dtm.raw_job_type.all
                      , dtm.title.test, dtm.abstract.test, dtm.raw_job_type.test){
    col.title.all <- dtm.title.all$dimnames$Terms
    col.abstract.all <- dtm.abstract.all$dimnames$Terms
    col.raw_job_type.all <- dtm.raw_job_type.all$dimnames$Terms
    col.title.test <- dtm.title.test$dimnames$Terms
    col.abstract.test <- dtm.abstract.test$dimnames$Terms
    col.raw_job_type.test <- dtm.raw_job_type.test$dimnames$Terms
    
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
    
    return(list(dtm.title.all.useTest, dtm.abstract.all.useTest, dtm.raw_job_type.all.useTest))
}
# col.title.test <- dtm.title.bi.test$dimnames$Terms
# col.abstract.test <- dtm.abstract.bi.test$dimnames$Terms
# col.raw_job_type.test <- dtm.raw_job_type.bi.test$dimnames$Terms

ls.dtm.bi.useTest <- MyUseTest(dtm.title.bi.all, dtm.abstract.bi.all, dtm.raw_job_type.bi.all
                               , dtm.title.bi.test, dtm.abstract.bi.test, dtm.raw_job_type.bi.test)
dtm.title.bi.useTest <- ls.dtm.bi.useTest[[1]]
dtm.abstract.bi.useTest <- ls.dtm.bi.useTest[[2]]
dtm.raw_job_type.bi.useTest <- ls.dtm.bi.useTest[[3]]

ls.dtm.all.useTest <- MyUseTest(dtm.title.all, dtm.abstract.all, dtm.raw_job_type.all
                                , dtm.title.test, dtm.abstract.test, dtm.raw_job_type.test)

dtm.title.all.useTest <- ls.dtm.all.useTest[[1]]
dtm.abstract.all.useTest <- ls.dtm.all.useTest[[2]]
dtm.raw_job_type.all.useTest <- ls.dtm.all.useTest[[3]]

# #######################################################################################
# ## combine title and abstract  ########################################################
# #######################################################################################
# length(col.title.test)
# # 15234
# length(col.abstract.test)
# # 59259
# length(intersect(col.title.test, col.abstract.test))
# # 9848
# 
# ## cols
# col.title.abstract.test <- intersect(col.title.test, col.abstract.test)
# col.title.test.only <- setdiff(col.title.test, col.title.abstract.test)
# col.abstract.test.only <- setdiff(col.abstract.test, col.title.abstract.test)
# 
# ## get the common cols matrix
# dtm.title.all.useTest.common <- dtm.title.all.useTest[, col.title.abstract.test]
# dtm.abstract.all.useTest.common <- dtm.abstract.all.useTest[, col.title.abstract.test]
# dtm.title.abstract.all.useTest <- dtm.title.all.useTest.common + dtm.abstract.all.useTest.common
# rm(list = c("dtm.title.all.useTest.common", "dtm.abstract.all.useTest.common"))
# 
# # get the only cols matrix
# dtm.title.all.useTest.only <- dtm.title.all.useTest[, col.title.test.only]
# dtm.abstract.all.useTest.only <- dtm.abstract.all.useTest[, col.abstract.test.only]
# dtm.title.abstract.useTest <- cbind(dtm.title.all.useTest.only, dtm.title.abstract.all.useTest, dtm.abstract.all.useTest.only)
#######################################################################################
## sparseMatrix and Cbind more cols ###################################################
#######################################################################################
MySm <- function(dtm.title.all.useTest, dtm.abstract.all.useTest, dtm.raw_job_type.all.useTest){
    sm.title.all.useTest <- sparseMatrix(i = dtm.title.all.useTest$i
                                         , j = dtm.title.all.useTest$j
                                         , x = dtm.title.all.useTest$v
                                         , dims = c(dtm.title.all.useTest$nrow, dtm.title.all.useTest$ncol))
    
    sm.abstract.all.useTest <- sparseMatrix(i = dtm.abstract.all.useTest$i
                                            , j = dtm.abstract.all.useTest$j
                                            , x = dtm.abstract.all.useTest$v
                                            , dims = c(dtm.abstract.all.useTest$nrow, dtm.abstract.all.useTest$ncol))
    #   # combined title and abstract
    #   sm.titel.abstract.all.useTest <- sparseMatrix(i = dtm.title.abstract.useTest$i
    #                                        , j = dtm.title.abstract.useTest$j
    #                                        , x = dtm.title.abstract.useTest$v
    #                                        , dims = c(dtm.title.abstract.useTest$nrow, dtm.title.abstract.useTest$ncol))
    #   
    sm.raw_job_type.all.useTest <- sparseMatrix(i = dtm.raw_job_type.all.useTest$i
                                                , j = dtm.raw_job_type.all.useTest$j
                                                , x = dtm.raw_job_type.all.useTest$v
                                                , dims = c(dtm.raw_job_type.all.useTest$nrow, dtm.raw_job_type.all.useTest$ncol))
    return(list(sm.title.all.useTest, sm.abstract.all.useTest, sm.raw_job_type.all.useTest))
}

ls.bi.useTest <- MySm(dtm.title.bi.useTest, dtm.abstract.bi.useTest, dtm.raw_job_type.bi.useTest)
sm.title.bi.useTest <- ls.bi.useTest[[1]]
sm.abstract.bi.useTest <- ls.bi.useTest[[2]]
sm.raw_job_type.bi.useTest <- ls.bi.useTest[[3]]

ls.all.useTest <- MySm(dtm.title.all.useTest, dtm.abstract.all.useTest, dtm.raw_job_type.all.useTest)
sm.title.all.useTest <- ls.all.useTest[[1]]
sm.abstract.all.useTest <- ls.all.useTest[[2]]
sm.raw_job_type.all.useTest <- ls.all.useTest[[3]]


## add salary
load("dt_jobs.RData")
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

## add location
load("dt_location.RData")
load("smmlocation.RData")

lat <- dt.location$lat
lng <- dt.location$lng
rot45_x <- dt.location$rot45_x
rot45_y <- dt.location$rot45_y
rot30_x <- dt.location$rot30_x
rot30_y <- dt.location$rot30_y
rot60_x <- dt.location$rot60_x
rot60_y <- dt.location$rot60_y
radial_r <- dt.location$radial_r

sm.all.useTest2 <- cBind(matrix(ifelse(dt.jobs$salary_type == "y", 1, 0), nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_type_y"))
                         , matrix(ifelse(dt.jobs$salary_type == "h", 1, 0), nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_type_h"))
                         , matrix(ifelse(dt.jobs$salary_type == "-1", -1, 0), nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_type__1"))
                         , matrix(log.salary_min.scale, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_min"))
                         , matrix(log.salary_max.scale, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_max"))
                         , matrix(log.salary_mean.scale, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_mean"))
                         , matrix(log.salary_diff.scale, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "salary_diff"))
                         , matrix(lat, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "lat"))
                         , matrix(lng, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "lng"))
                         , matrix(rot45_x, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "rot45_x"))
                         , matrix(rot45_y, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "rot45_y"))
                         , matrix(rot30_x, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "rot30_x"))
                         , matrix(rot30_y, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "rot30_y"))
                         , matrix(rot60_x, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "rot60_x"))
                         , matrix(rot60_y, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "rot60_y"))
                         , matrix(radial_r, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "radial_r"))
                         , smm.location[[1]] # city names too big
                         , smm.location[[2]]
                         , smm.location[[3]]
                         , smm.location[[4]]
                         , sm.title.all.useTest
                         , sm.abstract.all.useTest
                         , sm.title.bi.useTest # bi
                         , sm.abstract.bi.useTest # bi
                         # , sm.titel.abstract.all.useTest # combined by title and abstract
                         # , sm.title.bi.useTest # combined by title and abstract
                         , sm.raw_job_type.all.useTest
                         , sm.raw_job_type.bi.useTest # bi
)

dim(sm.all.useTest2)
# [1] 1439436   80874
# [1] 1439436 1566679

# rm(list = setdiff(ls(), "sm.all.useTest2"))

# save(sm.all.useTest2, file = "sm_all_useTest2.RData")

# ## add count
# load("dtm_all.RData")
# head(dtm.title.all$dimnames$Terms)
# popluar.terms <- c("airlin", "bar", "beverag", "chef", "cook", "guest", "servic"
#                    , "gaming", "housekeeping", "kitchen", "sandwich", "hand"
#                    , "reservation", "tour", "guide", "travel"
#                    , "agent", "waiting")
# popluar.terms <- wordStem(popluar.terms)
# popular.terms.title.cnt <- range01(row_sums(dtm.title.all[, popluar.terms]))
# popular.terms.abstract.cnt <- range01(row_sums(dtm.abstract.all[, popluar.terms]))
# 
# load("sm_all_useTest2_withLoc.RData")
# sm.all.useTest2.withLoc.withCnt <- cBind(matrix(popular.terms.title.cnt, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "popular.terms.title.cnt"))
#                                          , matrix(popular.terms.abstract.cnt, nrow(dt.jobs), 1, dimnames = list(rep(NA, nrow(dt.jobs)), "popular.terms.abstract.cnt"))
#                                          , sm.all.useTest2.withLoc)
# dim(sm.all.useTest2.withLoc.withCnt)
# # [1] 1439436   80876
# save(sm.all.useTest2.withLoc.withCnt, file = "sm_all_useTest2.RData")



