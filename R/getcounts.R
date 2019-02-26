getcounts<-function(col=ss){
  if(!require("dplyr")) BiocManager::install("dplyr",update = F,ask = F)
  if(!require("stringr")) BiocManager::install("stringr",update = F,ask = F)
  library(dplyr)
  library(stringr)
  fall <- dir(pattern = "*tab")
  fall1<-fall[1]
  str <- str_sub(fall1,start = 4, end = 10)
  num <- as.numeric(str)
  df.read <- read.table(fall1)
  if (col=='ss'){
    df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V4)
  }else{
    df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V2)
  }
  str_c<-str_c('SRR',str)
  colnames(df.use)<-c('V1',str_c)
  data.out<-df.use[1:4,]
  for (fnow in fall) {
    str <- str_sub(fnow, start = 4, end = 10)
    num <- as.numeric(str)
    df.read <- read.table(fnow)
    if (col=='ss'){
      df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V4)
    }else{
      df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V2)
    }
    str_c<-str_c('SRR',str)
    colnames(df.use)<-c('V1',str_c)
    data.out <- full_join(data.out, df.use,by="V1")
  }
  data.out1<-data.out[-(1:4),-2]
  colnames(data.out1)[2]<-str_c('SRR',str_sub(dir(pattern = "*tab")[1], start = 4, end = 10))
  rawcounts=data.frame(data.out1[,-1],row.names = data.out1[,1])
  write.csv(rawcounts, file = 'rawcounts.csv')
  return(rawcounts)
}