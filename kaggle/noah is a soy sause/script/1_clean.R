setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_Pitch_2016 /kaggle/noah is a soy sause/")
rm(list = ls()); gc();
require(data.table)
require(dplyr)
#######################################################################################
## 1.0 read ###########################################################################
#######################################################################################
dt.jobs <- fread("../../../data/sneak/jobs_sneak.csv")
dt.impressions <- fread("../../../data/sneak/job_impressions_sneak.csv")
dt.searches <- fread("../../../data/sneak/job_searches_sneak.csv")
dt.clicks <- fread("../../../data/sneak/job_clicks_sneak.csv")

#######################################################################################
## explore ############################################################################
#######################################################################################
table(dt.jobs$hat)
table(dt.jobs$Segment)
table(dt.jobs$subclasses)

dt.jobs[hat != -1] %>%
    group_by(hat, Segment, subclasses) %>%
    summarise(count = n())

plot(dt.jobs$subclasses, dt.jobs$hat)
plot(dt.jobs$Segment, dt.jobs$hat)
