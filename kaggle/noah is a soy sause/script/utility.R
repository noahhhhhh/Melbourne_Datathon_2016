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

#######################################################################################
## range01 ############################################################################
#######################################################################################
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#######################################################################################
## merge.with.order ###################################################################
#######################################################################################
merge.with.order <- function(x,y, ..., sort = T, keep_order)
{
    # this function works just like merge, only that it adds the option to return the merged data.frame ordered by x (1) or by y (2)
    add.id.column.to.data <- function(DATA)
    {
        data.frame(DATA, id... = seq_len(nrow(DATA)))
    }
    # add.id.column.to.data(data.frame(x = rnorm(5), x2 = rnorm(5)))
    order.by.id...and.remove.it <- function(DATA)
    {
        # gets in a data.frame with the "id..." column.  Orders by it and returns it
        if(!any(colnames(DATA)=="id...")) stop("The function order.by.id...and.remove.it only works with data.frame objects which includes the 'id...' order column")
        
        ss_r <- order(DATA$id...)
        ss_c <- colnames(DATA) != "id..."
        DATA[ss_r, ss_c]
    }
    
    # tmp <- function(x) x==1; 1	# why we must check what to do if it is missing or not...
    # tmp()
    
    if(!missing(keep_order))
    {
        if(keep_order == 1) return(order.by.id...and.remove.it(merge(x=add.id.column.to.data(x),y=y,..., sort = FALSE)))
        if(keep_order == 2) return(order.by.id...and.remove.it(merge(x=x,y=add.id.column.to.data(y),..., sort = FALSE)))
        # if you didn't get "return" by now - issue a warning.
        warning("The function merge.with.order only accepts NULL/1/2 values for the keep_order variable")
    } else {return(merge(x=x,y=y,..., sort = sort))}
}

#######################################################################################
## myNgram ############################################################################
#######################################################################################
myNgram <- function(x, ngram = 2){
    require(slam)
    # require(RWeka)
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
    corp.x <- tm_map(Corpus(VectorSource(x)), stemDocument, lazy = T)
    
    cat("Creating data-term-matrix (dtm) ...\n")
    corp.x <- tm_map(corp.x, #Corpus(VectorSource(x)),
                     content_transformer(function(x) iconv(x, to = 'UTF-8', sub = 'byte')),
                     mc.cores = 1)
    
    Tokenizer <- function(x) 
        unlist(lapply(ngrams(words(x), ngram), paste, collapse = " "), use.names = FALSE)
    dtm.x <- DocumentTermMatrix(corp.x, control = list(minWordLength = 2
                                                       , minDocFreq = 3
                                                       , tokenize = Tokenizer
                                                       , stemming = T
    ))
    
    return(dtm.x)
}



