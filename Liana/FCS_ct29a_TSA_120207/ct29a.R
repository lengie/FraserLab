DAC <- read.table("DMSO_antCanal.txt",skip=1)
DACr <- read.table("DMSO_antCrist.txt",skip=1)
DAM <- read.table("DMSO_antMac.txt",skip=1)
DDR <- read.table("DMSO_dorsalRoof.txt",skip=1)
DPC <- read.table("DMSO_postCanalpostCrist.txt",skip=1)
TAC <- read.table("TSA_antCanalantCrist.txt",skip=1)
TAM <- read.table("TSA_antMac.txt",skip=1)
TDR <- read.table("TSA_dorsalRoof.txt",skip=1)
TPC <- read.table("TSA_postCrist.txt",skip=1)

num <- data.frame(Name=character(),Mean=double(),Median=double(),stringsAsFactors=FALSE)
i <- 0

getData <- function (df){
  df <- plyr::rename(df,c("V1"="AvgSig","V2"="n","V3"="T","V4"="ttrip","V5"="tdiff",
                          "V6"="Exp","V7"="Page","V8"="cpp"))
  i <- i+1
  m <- mean(df$n)
  med <- median(df$n)
  avg <- mean(df$AvgSig)
  medsig <- median(df$AvgSig)
  print(paste("Mean signal is",avg,"& median is",medsig,
              " while mean n is",m,"and median is",med))
  dfname <- comment(df)
  print(num)
  num[i,] <- c(dfname,m,med)
  return(num)
}

categories <- list(DAC=DAC,DACr=DACr,DAM=DAM,DDR=DDR,DPC=DPC,TAC=TAC,TAM=TAM,
                   TDR=TDR,TPC=TPC)
num <- lapply(categories,getData)

qplot(num$Mean,num$Median)

