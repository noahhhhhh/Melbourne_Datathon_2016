
load(dt.jobs)
require(caTools)
## title.abstract
title.abstract.train <- paste(dt.jobs$title[dt.jobs$hat >= 0], dt.jobs$abstract[dt.jobs$hat >= 0])
title.abstract.test <- paste(dt.jobs$title[dt.jobs$hat < 0], dt.jobs$abstract[dt.jobs$hat < 0])

## known pred
known.pred <- unique(dt.jobs[hat >= 0 & title_abstract %in% intersect(title.abstract.train, title.abstract.test), c("title_abstract", "hat"), with = F])

## distribution of duplicated hat
table(known.pred[duplicated(known.pred[, "title_abstract", with = F])]$hat)
# 0   1 
# 273 299

## unknown
unknown.pred <- dt.jobs[hat < 0 & title_abstract %in% intersect(title.abstract.train, title.abstract.test), c("title_abstract", "hat"), with = F]


## rollup the known.pred
known.pred.agg <- known.pred[, list(hat = sum(hat)), by = "title_abstract"]
table(known.pred.agg$hat)

## remove duplicated rows
is.dup <- duplicated(known.pred$title_abstract, fromLast = F) | duplicated(known.pred$title_abstract, fromLast = T)
nrow(known.pred[!is.dup])
# 84998
nrow(known.pred)
# 86142

known.pred.removed <- known.pred[!is.dup]

## merge with dt.test
dt.test <- dt.jobs[hat < 0]

dt.test.merge <- merge(dt.test[, c("job_id", "title_abstract"), with = F]
      # , known.pred.agg
      , known.pred.removed
      , by = "title_abstract"
      , all.x = T
      , sort = F)

table(dt.test.merge$hat)
sum(is.na(dt.test.merge$hat))

## load the 1st submissin
submit <- fread("submission/1_single_xgb_raw_params_salary_all_1_gram.csv")
submit.merge <- data.table(submit, hat.merge = dt.test.merge$hat)
range(submit$hat)
hat.updated <- ifelse(is.na(submit.merge$hat.merge), submit.merge$hat
                      , 1 - submit.merge$hat.merge)
submit.merge[, hat.updated := hat.updated]

cor(submit.merge$hat, submit.merge$hat.updated)
# 0.9425013
submit.merge[, hat := NULL]
submit.merge[, hat.merge := NULL]
submit.merge[, hat := hat.updated]
submit.merge[, hat.updated := NULL]

## submit
write.csv(submit.merge, file = "submission/4_single_xgb_raw_params_salary_all_1_gram_combining_known_result_hashed_to_reverse_0_1.csv", row.names = F, quote = F)
# 0.97342 hashed to min and max
# 0.97342 hashed to 0 and 1



