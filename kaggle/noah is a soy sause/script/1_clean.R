setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_2016/kaggle/noah is a soy sause/")
rm(list = ls()); gc();
require(data.table)
require(dplyr)
#######################################################################################
## 1.0 read ###########################################################################
#######################################################################################
dt.jobs <- fread("../../../data/MelbourneDatathon2016/all/jobs_all.csv")
dt.impressions <- fread("../../../data/MelbourneDatathon2016/all/job_impressions_all.csv")
dt.searches <- fread("../../../data/MelbourneDatathon2016/all/job_searches_all.csv")
dt.clicks <- fread("../../../data/MelbourneDatathon2016/all/job_clicks_all.csv")

dt.jobClass <- fread("../../../data/MelbourneDatathon2016/job_classification.txt")
#######################################################################################
## 2.0 explore ########################################################################
#######################################################################################
dt.jobs
table(dt.jobs$hat)
table(dt.jobs$Segment)
table(dt.jobs$subclasses)
unique(dt.jobs$raw_job_type)
table(dt.jobs$salary_type)

dt.jobs[hat != -1] %>%
    group_by(hat, Segment, subclasses) %>%
    summarise(count = n())

# plot(dt.jobs$subclasses, dt.jobs$hat)
# plot(dt.jobs$Segment, dt.jobs$hat)


#######################################################################################
## 3.0 clean ##########################################################################
#######################################################################################
## Segment
dt.jobs[, Segment := ifelse(dt.jobs$Segment == "-1.0", "-1"
                            , ifelse(dt.jobs$Segment == "Unknown", "-10"
                                     , dt.jobs$Segment))]
dt.jobs[, Segment := as.numeric(dt.jobs$Segment)]
table(dt.jobs$Segment)

## salary_type
sum(dt.jobs$salary_type == "")
dt.jobs[, salary_type := ifelse(dt.jobs$salary_type == "", "-1", dt.jobs$salary_type)]
table(dt.jobs$salary_type)

#######################################################################################
## 4.0 save ###########################################################################
#######################################################################################
save(dt.jobs, file = "../../../data/RData/dt_jobs.RData")
