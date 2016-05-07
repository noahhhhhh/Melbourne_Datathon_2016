require(data.table)
require(dplyr)
dt.searches <- fread("../../../data/MelbourneDatathon2016/all/job_searches_all_V2.csv")
dt.impressions <- fread("../../../data/MelbourneDatathon2016/all/job_impressions_all_V2.csv")
dt.clicks <- fread("../../../data/MelbourneDatathon2016/all/job_clicks_all_V2.csv")
load("../../../data/RData/dt_jobs.RData")

dt.impressions
dt.impressions[user_id == 337672 & search_id == 432154][order()]
dt.impressions[, ind := 1]
## job id for hat and non hat
job.id.hat <- unique(dt.jobs[hat == 1]$job_id)
job.id.nonhat <- unique(dt.jobs[hat == 0]$job_id)

## users who saw hat and non hat
user.id.hat <- dt.impressions[job_id %in% job.id.hat]$user_id
user.id.nonhat <- dt.impressions[job_id %in% job.id.nonhat]$user_id

user.id.hat.unique <- unique(user.id.hat)
user.id.nonhat.unique <- unique(user.id.nonhat)

temp.hat <- dt.impressions[job_id %in% job.id.hat, list(ind_sum = sum(ind)), by = "user_id"]
temp.nonhat <- dt.impressions[job_id %in% job.id.nonhat, list(ind_sum = sum(ind)), by = "user_id"]

quantile.hat <- quantile(temp.hat$ind_sum)
user.id.hat.0_25 <- temp.hat[ind_sum >= quantile.hat[1] & ind_sum < quantile.hat[2]]$user_id
user.id.hat.25_50 <- temp.hat[ind_sum >= quantile.hat[2] & ind_sum < quantile.hat[3]]$user_id
user.id.hat.50_75 <- temp.hat[ind_sum >= quantile.hat[3] & ind_sum < quantile.hat[4]]$user_id
user.id.hat.75_100 <- temp.hat[ind_sum >= quantile.hat[4] & ind_sum <= quantile.hat[5]]$user_id

user.id.nonhat.0_25 <- temp.nonhat[ind_sum >= quantile.hat[1] & ind_sum < quantile.hat[2]]$user_id
user.id.nonhat.25_50 <- temp.nonhat[ind_sum >= quantile.hat[2] & ind_sum < quantile.hat[3]]$user_id
user.id.nonhat.50_75 <- temp.nonhat[ind_sum >= quantile.hat[3] & ind_sum < quantile.hat[4]]$user_id
user.id.nonhat.75_100 <- temp.nonhat[ind_sum >= quantile.hat[4] & ind_sum <= quantile.hat[5]]$user_id

## merge job with impression
dt.jobs.temp <- dt.jobs[, c("job_id", "hat"), with = F]
dt.jobs.temp <- dt.jobs.temp[, id := 1:nrow(dt.jobs.temp)]
dt.jobs.users <- merge(dt.jobs.temp
      , dt.impressions[, c("job_id", "user_id"), with = F]
      , by = "job_id"
      , all.x = T
      , all.y = F
      , sort = F)

dt.jobs.users[, is.user.id.hat := user_id %in% user.id.hat.unique]
dt.jobs.users[, is.user.id.hat.0_25 := user_id %in% user.id.hat.0_25]
dt.jobs.users[, is.user.id.hat.25_50 := user_id %in% user.id.hat.25_50]
dt.jobs.users[, is.user.id.hat.50_75 := user_id %in% user.id.hat.50_75]
dt.jobs.users[, is.user.id.hat.75_100 := user_id %in% user.id.hat.75_100]
dt.jobs.users[, is.user.id.nonhat := user_id %in% user.id.nonhat.unique]
dt.jobs.users[, is.user.id.nonhat.0_25 := user_id %in% user.id.nonhat.0_25]
dt.jobs.users[, is.user.id.nonhat.25_50 := user_id %in% user.id.nonhat.25_50]
dt.jobs.users[, is.user.id.nonhat.50_75 := user_id %in% user.id.nonhat.50_75]
dt.jobs.users[, is.user.id.nonhat.75_100 := user_id %in% user.id.nonhat.75_100]
dt.jobs.users.features <- dt.jobs.users[, list(cnt.user.id.hat = sum(is.user.id.hat)
                     , cnt.user.id.hat.0_25 = sum(is.user.id.hat.0_25)
                     , cnt.user.id.hat.25_50 = sum(is.user.id.hat.25_50)
                     , cnt.user.id.hat.50_75 = sum(is.user.id.hat.50_75)
                     , cnt.user.id.hat.75_100 = sum(is.user.id.hat.75_100)
                     , cnt.user.id.nonhat = sum(is.user.id.nonhat)
                     , cnt.user.id.nonhat.0_25 = sum(is.user.id.nonhat.0_25)
                     , cnt.user.id.nonhat.25_50 = sum(is.user.id.nonhat.25_50)
                     , cnt.user.id.nonhat.50_75 = sum(is.user.id.nonhat.50_75)
                     , cnt.user.id.nonhat.75_100 = sum(is.user.id.nonhat.75_100))
              , by = c("job_id", "id")]

save(dt.jobs.users.features, file = "temp_data/dt_jobs_users_features.RData")
# dt.jobs.users[, user_id := ifelse(is.na(dt.jobs.users$user_id), 0, dt.jobs.users$user_id)]
# dt.jobs.users.feature <- data.table(id = 0
# 
#                                     , mean_user.id.hat = 0
#                                     , mean_user.id.hat.0_25 = 0
#                                     , mean_user.id.hat.25_50 = 0
#                                     , mean_user.id.hat.50_75 = 0
#                                     , mean_user.id.hat.75_100 = 0
# 
#                                     , mean_user.id.nonhat = 0
#                                     , mean_user.id.nonhat.0_25 = 0
#                                     , mean_user.id.nonhat.25_50 = 0
#                                     , mean_user.id.nonhat.50_75 = 0
#                                     , mean_user.id.nonhat.75_100 = 0)
# 
# # dt.jobs.users.merged <- merge(dt.jobs.users,ddply(dt.jobs.users,.(job_id),summarise, user_ls = paste0(user_id, collapse = " ")), by = "job_id")
# # 
# # 
# # dt.jobs.users.merged.unique <- unique(dt.jobs.users.merged[, c("job_id", "id", "user_ls"), with = F])
# # ls.users <- strsplit(dt.jobs.users.merged.unique$user_ls, " ")
# # 
# # mean_user.id.hat <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.hat.unique)}))
# # mean_user.id.hat.0_25 <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.hat.0_25)}))
# # mean_user.id.hat.25_50 <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.hat.25_50)}))
# # mean_user.id.hat.50_75 <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.hat.50_75)}))
# # mean_user.id.hat.75_100 <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.hat.75_100)}))
# # 
# # mean_user.id.nonhat <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.nonhat.unique)}))
# # mean_user.id.nonhat.0_25 <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.nonhat.0_25)}))
# # mean_user.id.nonhat.25_50 <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.nonhat.25_50)}))
# # mean_user.id.nonhat.50_75 <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.nonhat.50_75)}))
# # mean_user.id.nonhat.75_100 <- unlist(lapply(ls.users, function(x){sum(x %in% user.id.nonhat.75_100)}))
# 
# for(i in 1:nrow(dt.jobs.temp)){
#     mean_user.id.hat <- sum(dt.jobs.users[id == i]$user_id %in% user.id.hat.unique)
#     mean_user.id.hat.0_25 <- sum(dt.jobs.users[id == i]$user_id %in% user.id.hat.0_25)
#     mean_user.id.hat.25_50 <- sum(dt.jobs.users[id == i]$user_id %in% user.id.hat.25_50)
#     mean_user.id.hat.50_75 <- sum(dt.jobs.users[id == i]$user_id %in% user.id.hat.50_75)
#     mean_user.id.hat.75_100 <- sum(dt.jobs.users[id == i]$user_id %in% user.id.hat.75_100)
# 
#     mean_user.id.nonhat <- sum(dt.jobs.users[id == i]$user_id %in% user.id.nonhat.unique)
#     mean_user.id.nonhat.0_25 <- sum(dt.jobs.users[id == i]$user_id %in% user.id.nonhat.0_25)
#     mean_user.id.nonhat.25_50 <- sum(dt.jobs.users[id == i]$user_id %in% user.id.nonhat.25_50)
#     mean_user.id.nonhat.50_75 <- sum(dt.jobs.users[id == i]$user_id %in% user.id.nonhat.50_75)
#     mean_user.id.nonhat.75_100 <- sum(dt.jobs.users[id == i]$user_id %in% user.id.nonhat.75_100)
# 
#     dt.jobs.users.feature <- rbind(dt.jobs.users.feature, data.table(id = i
# 
#                                                                      , mean_user.id.hat = mean_user.id.hat
#                                                                      , mean_user.id.hat.0_25 = mean_user.id.hat.0_25
#                                                                      , mean_user.id.hat.25_50 = mean_user.id.hat.25_50
#                                                                      , mean_user.id.hat.50_75 = mean_user.id.hat.50_75
#                                                                      , mean_user.id.hat.75_100 = mean_user.id.hat.75_100
# 
#                                                                      , mean_user.id.nonhat = mean_user.id.nonhat
#                                                                      , mean_user.id.nonhat.0_25 = mean_user.id.nonhat.0_25
#                                                                      , mean_user.id.nonhat.25_50 = mean_user.id.nonhat.25_50
#                                                                      , mean_user.id.nonhat.50_75 = mean_user.id.nonhat.50_75
#                                                                      , mean_user.id.nonhat.75_100 = mean_user.id.nonhat.75_100
#                                                                      )
#                                    )
#     print(paste(round(i / nrow(dt.jobs.temp), 2), i, "in", nrow(dt.jobs.temp)))
# }