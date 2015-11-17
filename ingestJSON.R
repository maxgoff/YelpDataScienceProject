## Elite Yelp Users Project
## Max K. Goff
## November 2015

# This file contains the functions used to ingest the Yelp Academic data set.
# The code presumes you have downloaded and unzipped the data set
# into a subdirectory yelp.  The code also presumes you have created
# a subdirectory ydata.  Intermediate versions of R objects are stored
# in the ydata directory.

# This code was not designed to work as a stand alone package.
# Some of the functions in this file were experimental and not used.
# The creation, testing, and modification of data was done in an
# interative manner using functions like those in this file, with
# R objects stored as intermediate placeholders along the way.

readNflatten= function(){
# This function reads the JSON files, flattens and stores
# for additional processing.
  
# set the working directory specific to your system.
  
setwd('/Users/maxgoff/Dropbox/Coursera/Capstone/drill') 
rawFiles = c("user", "business", "review", "tip", "checkin")
for(i in 1:length(rawFiles)){
  json_file = paste("yelp/yelp_academic_dataset_", rawFiles[i], ".json", sep="")
  dat = jsonlite::fromJSON(sprintf("[%s]", paste(readLines(json_file), collapse=",")))
  dat_file = paste("ydata/", rawFiles[i], ".data", sep="")
  save(dat, file=dat_file)
  print(paste("json file:", json_file, " dat file:", dat_file))
 
  }

}

flen = function(mlist){
  ret = length(mlist[[1]])
  return(ret)
}

getFriendsCount = function(){
r = flen(dat[1,]$friends)
for(n in 2:nrow(dat)){
  r = append(r,flen( dat[n,]$friends))
  }

}

getEliteCount = function(){
  e = flen(dat[1,]$elite)
  for(n in 2:nrow(dat)){
    e = append(e, length(dat[n,]$elite[[1]]))
  }
  return(e)
}

misc = function() {
save(vf,file= 'ydata/yuserFlattened.data')

setwd('/Users/maxgoff/Dropbox/Coursera/Capstone/drill')
library(dplyr)
yvf = load('ydata/yuserFlattened.data')
#set.seed(3883)
vfSample=sample_n(vf, 40000, replace=FALSE)
vfSample=vfSample[,c(3,7,8,12,13,14,15)]
vec=vfSample$fans==0
vfSample=vfSample[!vec,]
}

setupReviewSampling = function() {
  setwd('/Users/maxgoff/Dropbox/Coursera/Capstone/drill')
  z = load('ydata/review.data')
  x = load('ydata/yuserFlattened.data')
  library(stringi)
  library(SnowballC)
 # set.seed(3883)
  baseSampleSize = as.integer( nrow(yreview) * 0.04)
  baseSample = sample(yreview$text, baseSampleSize, replace=FALSE)
  baseSampleChar = as.character(baseSample)
  baseAveLength = ave(nchar(baseSampleChar))[1]
  data <- paste(baseSample, collapse=" ")
  out <- stri_extract_all_words(stri_trans_tolower(SnowballC::wordStem(data[[1]], "english"))) #in old package versions it was named 'stri_extract_words'
  names(out) <- paste0("doc", 1:length(out))
  lev <- sort(unique(unlist(out)))
  dat <- do.call(cbind, lapply(out, function(x, lev) {
    tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
  }, lev = lev))
  rownames(dat) <- sort(lev)
  
  library(tm)
  library(wordcloud)
  dat <- dat[!rownames(dat) %in% tm::stopwords("english"), ]
  dat2 = sort(dat, decreasing=TRUE)
  dat3 = baseAveLength
  #save(dat3, file='ydata/dat3base.data')
  #save(dat2, file = 'ydata/dat2base.data')
  top100 = head(dat2, 100)
  print(paste("Base Char Count Ave:", dat3))
  
  wordcloud(names(top100), top100)
  return(top100)
}

setupElitesReviews = function(){
  rm(list=ls())
  setwd('/Users/maxgoff/Dropbox/Coursera/Capstone/drill')
  z = load('ydata/yuserFlattened.data')
  x = load('ydata/review.data')
  ev = vf$eliteCount > 0
  elites = vf[ev,]$user_id
  edata = as.character(" ")
  eSampleSize = as.integer(length(elites) * 0.04)
  eSample = sample(elites, eSampleSize, replace=FALSE)
  yreview$user_id = as.character(yreview$user_id)
  yreview$text = as.character(yreview$text)
  rm(elites)
  rm(vf)
  aeN = 0
  aeD = 0
 
   for(i in 1:length(eSample)){
    vec = yreview$user_id == eSample[i]
    aeN = aeN + sum(nchar(yreview[vec,]$text))
    aeD = aeD + sum(vec)
    txt = yreview[vec,]$text
    jdata = paste(txt, collapse=" ")
    edata = paste(edata, jdata)
    j = nchar(edata)
 
    k=as.integer(i%%5000)
    if( k == 0) print(paste("pass:", i, " of ", length(eSample)," length:", j, sep=""))
  }
  
  out <- stri_extract_all_words(stri_trans_tolower(SnowballC::wordStem(edata[[1]], "english"))) #in old package versions it was named 'stri_extract_words'
  names(out) <- paste0("doc", 1:length(out))
  lev <- sort(unique(unlist(out)))
  dat <- do.call(cbind, lapply(out, function(x, lev) {
    tabulate(factor(x, levels = lev, ordered = TRUE), nbins = length(lev))
  }, lev = lev))
  rownames(dat) <- sort(lev)
  
  library(tm)
  library(wordcloud)
  dat <- dat[!rownames(dat) %in% tm::stopwords("english"), ]
  dat2 = sort(dat, decreasing=TRUE)
  dat3 = aeN / aeD
 # save(dat3, file='ydata/dat3elite.data')
#  save(dat2, file = 'ydata/dat2elite.data')
  top100 = head(dat2, 100)
  wordcloud(names(top100), top100)
  print(paste("Elite Char Count Ave: ", dat3))
  
  return(top100)
}
GetDCorpus <-function(textVector) {
  library(tm)
  #install.packages("tm.plugin.dc")
  library(tm.plugin.dc)
  doc.corpus <- as.DistributedCorpus(VCorpus(VectorSource(textVector)))
  doc.corpus <- tm_map(doc.corpus, content_transformer(tolower))
  doc.corpus <- tm_map(doc.corpus, content_transformer(removeNumbers))
  doc.corpus <- tm_map(doc.corpus, content_transformer(removePunctuation))
  # <- tm_map(doc.corpus, removeWords, stopwords("english")) # won't accept this for some reason...
  return(doc.corpus)
}

bigRun = function(){
  rm(list=ls())
  setwd('/Users/maxgoff/Dropbox/Coursera/Capstone/drill')

  library(dplyr)
  library(tm)
  library(tm.plugin.dc)
  library(stringi)
  library(SnowballC)
  library(wordcloud)
  
  seeds = c(3883, 2442, 1221, 11, 38, 44, 17, 69, 2222, 90, 80, 443, 10, 20,
            30, 40, 55, 95, 104, 3333, 9999, 7896, 1031, 803)
  for( runseed in 1:length(seeds)){
    set.seed(seeds[runseed])
   
  par(mfrow=c(1,2))
  top100Base = setupReviewSampling()
  filename = paste("ydata/results/top100Base", seeds[runseed], ".data", sep="")
  save(top100Base,file = filename)
 
  top100Elite = setupElitesReviews()
  filename = paste("ydata/results/top100Elite", seeds[runseed], ".data", sep="")
  save(top100Elite,file = filename)

  }
  
  aveDocLength = function( txt){
    charCount = 0
    for(i in 1:length(txt)){
      charCount = charCount + nchar(txt[i])
    }
    return( charCount / length(txt))
    
  }

  getDocLength = function(){
   yrev = data.table(yreview)
    userz = vf$user_id
  #  baseSampleSize = as.integer( nrow(yreview) * 0.04)
  #  baseSample = sample_n(yreview, baseSampleSize, replace=FALSE)
  #  baseSample$text = as.character(baseSample$text)
  #  mytextCount = as.integer()
    myCount = as.integer()
   for( user in 1:length(userz)){
   
  
      vec = yrev$user_id == userz[user]
     # print(ave(nchar(yrev[vec,]$text)))
      myCount[user] = ave(nchar(yrev[vec,]$text))
      if(( user %% 5000) == 0) print(paste("Pass:", user))
   }
    
    return(myCount)
  }  
  
  getFK = function(){
    rm(list=ls())
    setwd('/Users/maxgoff/Dropbox/Coursera/Capstone/drill')
    library(dplyr)
    library(tm)
    library(data.table)
    library(koRpus)
    library(compiler)
    library(qdap)
   
    enableJIT(1)
    set.kRp.env(TT.cmd="/Users/maxgoff/Dropbox/Coursera/Capstone/drill/TreeTagger/cmd/tree-tagger-english", lang="en")
    load('ydata/reviewDataTable.data')
    load('ydata/userDataTable.data')
    #yr = data.table(yreview)
    #ur = data.table(vf)
    userz = as.character(vf$user_id)
    #  baseSampleSize = as.integer( nrow(yreview) * 0.04)
    #  baseSample = sample_n(yreview, baseSampleSize, replace=FALSE)
    #  baseSample$text = as.character(baseSample$text)
    #  mytextCount = as.integer()
  #  tf = tempfile()
  #  MyFK = as.numeric()
  #  MyST = as.numeric()
    start.time = Sys.time()
    test_results = as.numeric()
    foreach( user = 1:1000) %dopar% {
 #   for( user in 1:1000)  {
      
      #for( user in 1:3){
      vec = yr$user_id == userz[user]
      txt = as.character(yr[vec,]$text)
      fk = automated_readability_index(sent_detect(replace_abbreviation(txt)))
       test_results[user] = fk[,2]$Automated_Readability_Index
   #   if(!( user %% 500) == 0) print(paste("Pass:", user, " R:",fk[,2]$Automated_Readability_Index))
    }
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    
  }  
  
  funk = function(){  
    
    for( user in 1:3){
    vec = yr$user_id == userz[user]
    txt = as.character(yr[vec,]$text)
    write(txt,tf)
    # print(ave(nchar(yrev[vec,]$text)))
    rds = readability(tf, quiet=TRUE)
    MyFK[user] = as.numeric(rds@Flesch.Kincaid$grade)
    MyST[user] = as.numeric(rds@SMOG$grade)
    if(( user %% 500) != 0) print(paste("Pass:", user))
      }
    }
  
eliteUserCount = function() {
  countU = as.integer()
  for(i in 1:length(tip_userz)){
    
    iz = tp$user_id == tip_userz[i]
    countU[i] = sum(iz)
    if((i %% 1000 )==0)print(paste("Processing:", i))
  }
  return(countU)
}

addTipCount = function(){
  for(i in 1:length(tipU$tipCount)){
    v = ur$user_id == tipU[i,]$user_id
      ii = tipU[i,]$tipCount
      ur[v,18]  = as.integer(ii)
    
    if(( i %% 1000) == 0)print(paste("pass ", i))
  }
}
}

setwd('/Users/maxgoff/Dropbox/Coursera/Capstone/drill')
load('ydata/yuserTipCount275K.data')
load('ydata/ytip275K.data')
for(i in 275000:nrow(vf)){
 
  cnt = ytip$user_id == vf[i,]$user_id
  vf[i,]$tipCount = sum(cnt)
  if((i %% 1000)==0)
    print(paste("pass:", i, "  ", sum(cnt)))
}
