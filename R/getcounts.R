getcounts<-function(prefixlength=10,colss="Nss"){
  if(!require("dplyr")) BiocManager::install("dplyr",update = F,ask = F)
  if(!require("stringr")) BiocManager::install("stringr",update = F,ask = F)
  library(dplyr)
  library(stringr)
  fall <- dir(pattern = "*tab")
  fall1<-fall[1]
  str <- str_sub(fall1,start = 1, end = prefixlength)
  df.read <- read.table(fall1)
  if (colss=='dUTP'){
    df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V4)
  }
  if (colss=='NdUTP'){
    df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V3)
  }
  if (colss=='Nss'){
    df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V2)
  }
  colnames(df.use)<-c('V1',str)
  data.out<-df.use[1:4,]
  for (fnow in fall) {
    str <- str_sub(fnow, start = 1, end = prefixlength)
    df.read <- read.table(fnow)
    if (colss=='dUTP'){
      df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V4)
    }
    if (colss=='NdUTP'){
      df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V3)
    }
    if (colss=='Nss'){
      df.use <- data.frame(v1 = df.read$V1, v4 = df.read$V2)
    }
    colnames(df.use)<-c('V1',str)
    data.out <- full_join(data.out, df.use,by="V1")
  }
  data.out1<-data.out[-(1:4),-2]
  colnames(data.out1)[2]<-str_sub(dir(pattern = "*tab")[1], start = Start, end = End)
  rawcounts=data.frame(data.out1[,-1],row.names = data.out1[,1])
  write.csv(rawcounts, file = 'rawcounts.csv')
  return(rawcounts)
}

