setwd("/Volumes/Data Science/Google Drive/data_science_competition/melbourne_datathon_2nd/Melbourne_Datathon_2016/kaggle/noah is a soy sause/")
rm(list = ls()); gc();
require(data.table)
require(tm)
require(SnowballC)
require(stringr)
require(ggplot2)
load("../../../data/RData/dt_jobs.RData")
#######################################################################################
## hat = 1 ############################################################################
#######################################################################################
dt.hat.1 <- dt.jobs[hat == 1]
title.hat.1 <- tolower(dt.hat.1$title)
abstract.hat.1 <- tolower(dt.hat.1$abstract)
raw_job_type.hat.1 <- tolower(dt.hat.1$raw_job_type)

#######################################################################################
## title ##############################################################################
#######################################################################################
## remove punctuations and digits
title.hat.1 <- gsub("[[:punct:][:digit:]]", " ", title.hat.1)
## remove more than 2 space
title.hat.1 <- gsub("\\s+", " ", title.hat.1)
## remove non alpha
title.hat.1 <- gsub("[^a-zA-Z]", " ", title.hat.1)
## remove more than 2 space and strim spaces
title.hat.1 <- str_trim(gsub("\\s+", " ", title.hat.1))
## stemming
title.hat.1 <- wordStem(title.hat.1)
## remove stop words
corp.title.hat.1 <- tm_map(Corpus(VectorSource(title.hat.1)), removeWords, stopwords("english")) 

## dtm
dtm.title.hat.1 <- DocumentTermMatrix(corp.title.hat.1)

## data table
dt.title.hat.1 <- data.table(text = unlist(sapply(corp.title.hat.1, `[`, "content")))

## freq
freq.title.hat.1 <- colSums(as.matrix(dtm.title.hat.1)) 
length(freq.title.hat.1); length(title.hat.1)
# [1] 12235
# [1] 122886
# order by frequency
ord.title.hat.1 <- order(-freq.title.hat.1)
# explore
freq.title.hat.1[head(ord.title.hat.1)]
# chef    cook barista   staff kitchen    cafe 
# 23695   10945    7960    7733    7577    7184 
head(table(freq.title.hat.1), 200)
# 1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   17   18   19   20 
# 4630 1688  900  623  461  321  278  233  206  164  145  131  108  102   90   74   91   65   60   46 
dt.wf.title.hat.1 <- data.table(word = names(freq.title.hat.1), freq = freq.title.hat.1)

# plot
p <- ggplot(subset(dt.wf.title.hat.1, freq > 50), aes(word, freq))
p <- p + geom_bar(stat = "identity")   
p <- p + theme(axis.text.x=element_text(angle = 45, hjust = 1))
p

# assoc
findAssocs(dtms, "contrast", corlimit = .9)



