# setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_2016/kaggle/noah is a soy sause/")
rm(list = ls()); gc();
require(data.table)
require(tm)
require(SnowballC)
require(stringr)
require(ggplot2)
source("utility.R")
load("dt_jobs.RData")
#######################################################################################
## hat = 1 ############################################################################
#######################################################################################
# dt.hat.1 <- dt.jobs[hat == 1]
title.hat.1 <- tolower(dt.jobs[hat == 1]$title)
abstract.hat.1 <- tolower(dt.jobs[hat == 1]$abstract)
raw_job_type.hat.1 <- tolower(dt.jobs[hat == 1]$raw_job_type)

###########
## title ##
###########
list.title.hat.1 <- txtPreprocess(title.hat.1)
dt.title.hat.1 <- list.title.hat.1[["dt"]]
dtm.title.hat.1 <- list.title.hat.1["dtm"]
dt.wf.title.hat.1 <- list.title.hat.1[["dt.wf"]]

# plot
p <- ggplot(subset(dt.wf.title.hat.1, freq > 2000), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

##############
## abstract ##
##############
list.abstract.hat.1 <- txtPreprocess(abstract.hat.1, rm_sparse = .99999)
dt.abstract.hat.1 <- list.abstract.hat.1[["dt"]]
dtm.abstract.hat.1 <- list.abstract.hat.1["dtm"]
dt.wf.abstract.hat.1 <- list.abstract.hat.1[["dt.wf"]]

# plot
p <- ggplot(subset(dt.wf.abstract.hat.1, freq > 2000), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

##################
## raw_job_type ##
##################
list.raw_job_type.hat.1 <- txtPreprocess(raw_job_type.hat.1)
dt.raw_job_type.hat.1 <- list.raw_job_type.hat.1[["dt"]]
dtm.raw_job_type.hat.1 <- list.raw_job_type.hat.1["dtm"]
dt.wf.raw_job_type.hat.1 <- list.raw_job_type.hat.1[["dt.wf"]]

# plot
p <- ggplot(subset(dt.wf.raw_job_type.hat.1, freq > 100), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

## save
save(list.title.hat.1, list.abstract.hat.1, list.raw_job_type.hat.1, file = "../../../data/RData/dt_keywords_hat_1.RData")

#######################################################################################
## hat != -1 ############################################################################
#######################################################################################
# dt.hat.all <- dt.jobs
title.hat.all <- tolower(dt.jobs$title)
abstract.hat.all <- tolower(dt.jobs$abstract)
raw_job_type.hat.all <- tolower(dt.jobs$raw_job_type)

###########
## title ##
###########
list.title.hat.all <- txtPreprocess(title.hat.all, rm_sparse = .999)
dt.title.hat.all <- list.title.hat.all[["dt"]]
dtm.title.hat.all <- list.title.hat.all["dtm"]
dt.wf.title.hat.all <- list.title.hat.all[["dt.wf"]]

# plot
p <- ggplot(subset(dt.wf.title.hat.all, freq > 2000), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

##############
## abstract ##
##############
list.abstract.hat.all <- txtPreprocess(abstract.hat.all, rm_sparse = .993)
dt.abstract.hat.all <- list.abstract.hat.all[["dt"]]
dtm.abstract.hat.all <- list.abstract.hat.all["dtm"]
dt.wf.abstract.hat.all <- list.abstract.hat.all[["dt.wf"]]

# plot
p <- ggplot(subset(dt.wf.abstract.hat.all, freq > 2000), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

##################
## raw_job_type ##
##################
list.raw_job_type.hat.all <- txtPreprocess(raw_job_type.hat.all, rm_sparse = .999)
dt.raw_job_type.hat.all <- list.raw_job_type.hat.all[["dt"]]
dtm.raw_job_type.hat.all <- list.raw_job_type.hat.all["dtm"]
dt.wf.raw_job_type.hat.all <- list.raw_job_type.hat.all[["dt.wf"]]

# plot
p <- ggplot(subset(dt.wf.raw_job_type.hat.all, freq > 100), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

## save
# save(list.raw_job_type.hat.all, file = "dt_row_job_type_hat_all.RData")
save(list.title.hat.all, list.abstract.hat.all, list.raw_job_type.hat.all, file = "dt_keywords_hat_all.RData")

#######################################################################################
## hat == -1 ##########################################################################
#######################################################################################
# dt.hat.test <- dt.jobs[hat == -1]
title.hat.test <- tolower(dt.jobs[hat == -1]$title)
abstract.hat.test <- tolower(dt.jobs[hat == -1]$abstract)
raw_job_type.hat.test <- tolower(dt.jobs[hat == -1]$raw_job_type)

###########
## title ##
###########
list.title.hat.test <- txtPreprocess(title.hat.test, rm_sparse = .9999)
dt.title.hat.test <- list.title.hat.test[["dt"]]
dtm.title.hat.test <- list.title.hat.test["dtm"]
dt.wf.title.hat.test <- list.title.hat.test[["dt.wf"]]

# plot
p <- ggplot(subset(dt.wf.title.hat.test, freq > 2000), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

##############
## abstract ##
##############
list.abstract.hat.test <- txtPreprocess(abstract.hat.test, rm_sparse = .9999)
dt.abstract.hat.test <- list.abstract.hat.test[["dt"]]
dtm.abstract.hat.test <- list.abstract.hat.test["dtm"]
dt.wf.abstract.hat.test <- list.abstract.hat.test[["dt.wf"]]

# plot
p <- ggplot(subset(dt.wf.abstract.hat.test, freq > 2000), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

##################
## raw_job_type ##
##################
list.raw_job_type.hat.test <- txtPreprocess(raw_job_type.hat.test, rm_sparse = .9999)
dt.raw_job_type.hat.test <- list.raw_job_type.hat.test[["dt"]]
dtm.raw_job_type.hat.test <- list.raw_job_type.hat.test["dtm"]
dt.wf.raw_job_type.hat.test <- list.raw_job_type.hat.test[["dt.wf"]]

# plot
p <- ggplot(subset(dt.wf.raw_job_type.hat.test, freq > 100), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

## save
save(list.title.hat.test, list.abstract.hat.test, list.raw_job_type.hat.test, file = "dt_keywords_hat_test.RData")





























