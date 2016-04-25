#######################################################################################
## txtPreprocess ######################################################################
#######################################################################################
txtPreprocess <- function(x, rm_sparse = 1){
    require(slam)
    cat("stemming ...\n")
    x <- wordStem(x)
    cat("removing punctuations and digits ...\n")
    x <- gsub("[[:punct:][:digit:]]", " ", x)
    cat("removing more than 2 space ...\n")
    x <- gsub("\\s+", " ", x)
    cat("removing non alpha ...\n")
    x <- gsub("[^a-zA-Z]", " ", x)
    cat("removing more than 2 space and strim spaces ...\n")
    x <- str_trim(gsub("\\s+", " ", x))
    cat("removing stop words ...\n")
    x <- removeWords(x, stopwords(kind = "en"))
    # corp.x <- tm_map(Corpus(VectorSource(x)), contentremoveWords, stopwords("english")) 
    cat("stemming ...\n")
    # x <- stemDocument(x)
    corp.x <- tm_map(Corpus(VectorSource(x)), content_transformer(stemDocument), lazy = T)
    
    cat("Creating data-term-matrix (dtm) ...\n")
    corp.x <- tm_map(corp.x, #Corpus(VectorSource(x)),
                     content_transformer(function(x) iconv(x, to = 'UTF-8', sub = 'byte')),
                     mc.cores = 1)
    dtm.x <- DocumentTermMatrix(corp.x, control = list(minWordLength = 2, minDocFreq = 3))
    
    if(rm_sparse != 1){
        cat("removing sparse ...\n")
        dtm.x <- removeSparseTerms(dtm.x, rm_sparse)
    }
    
    cat("Creating data table (dt) ...\n")
    dt.x <- data.table(text = unlist(sapply(corp.x, `[`, "content")))
    
    ## freq
    freq.x <- col_sums(as.matrix(dtm.x), na.rm = T)
    length(freq.x); length(x)
    
    # order by frequency
    ord.x <- order(-freq.x)
    cat("head word frequency:]n")
    print(head(freq.x[ord.x]))
    
    cat("Creating word-frequency data table (dt.wf) ...\n")
    dt.wf.x <- data.table(word = names(freq.x), freq = freq.x)[order(-freq.x)]
    dt.wf.x
    
    return(list(dt = dt.x, dtm = dtm.x, dt.wf = dt.wf.x))
}



# require(slam)
# cat("stemming ...\n")
# x <- wordStem(abstract.hat.test)
# cat("removing punctuations and digits ...\n")
# x <- gsub("[[:punct:][:digit:]]", " ", x)
# cat("removing more than 2 space ...\n")
# x <- gsub("\\s+", " ", x)
# cat("removing non alpha ...\n")
# x <- gsub("[^a-zA-Z]", " ", x)
# cat("removing more than 2 space and strim spaces ...\n")
# x <- str_trim(gsub("\\s+", " ", x))
# cat("removing stop words ...\n")
# x <- removeWords(x, stopwords(kind = "en"))
# # corp.x <- tm_map(Corpus(VectorSource(x)), contentremoveWords, stopwords("english")) 
# cat("stemming ...\n")
# # x <- stemDocument(x)
# corp.x <- tm_map(Corpus(VectorSource(x)), content_transformer(stemDocument), lazy = T)
# 
# cat("Creating data-term-matrix (dtm) ...\n")
# corp.x <- tm_map(corp.x, #Corpus(VectorSource(x)),
#                  content_transformer(function(x) iconv(x, to = 'UTF-8', sub = 'byte')),
#                  mc.cores = 1)
# dtm.x <- DocumentTermMatrix(corp.x, control = list(minWordLength = 2, minDocFreq = 3))
# 
# # dt.x <- as.data.table(data.frame(text = unlist(sapply(corp.x, `[`, "content")), stringsAsFactors = F))
# 
# # dtm.x.x <- removeSparseTerms(dtm.x, .999)
# # cat("Creating data table (dt) ...\n")
# # dt.x <- data.table(text = unlist(sapply(corp.x, `[`, "content")))
# # 
# # ## freq
# # freq.x <- col_sums(as.matrix(dtm.x.x), na.rm = T)
# # length(freq.x); length(x)
# # 
# # # order by frequency
# # ord.x <- order(-freq.x)
# # cat("head word frequency:]n")
# # print(head(freq.x[ord.x]))
# # 
# # cat("Creating word-frequency data table (dt.wf) ...\n")
# # dt.wf.x <- data.table(word = names(freq.x), freq = freq.x)[order(-freq.x)]
# # dt.wf.x
# 
# # list.raw_job_type.hat.all <- list(dt = dt.x, dtm = dtm.x, dt.wf = dt.wf.x)
# dtm.abstract.test <- dtm.x # orignial 56.MB
# object.size(dtm.abstract.test)
# save(dtm.abstract.test, file = "dtm_abstract_test.RData")
# 
# 
# length(setdiff(dtm.abstract.all$dimnames$Terms, dtm.abstract.hat.test$dimnames$Terms))
# length(dtm.abstract.hat.test$dimnames$Terms)
# length(dtm.abstract.all$dimnames$Terms)
