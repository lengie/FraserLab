#each has avg signal, n, T, t_trip, t_diff, filename (includes page number), cpp. No duration. 
#Maybe separate into another folder and have a loop open and read all of the files?
DAC <- read.table("DMSO_antCanal.txt",skip=1)
DACr <- read.table("DMSO_antCrist.txt",skip=1)
DAM <- read.table("DMSO_antMac.txt",skip=1)
DDR <- read.table("DMSO_dorsalRoof.txt",skip=1)
DPC <- read.table("DMSO_postCanal.txt",skip=1)
DPCr <- read.table("DMSO_postCrist.txt",skip=1)
TAC <- read.table("TSA_antCanal.txt",skip=1)
TACr <- read.table("TSA_antCrist.txt",skip=1)
TAM <- read.table("TSA_antMac.txt",skip=1)
TDR <- read.table("TSA_dorsalRoof.txt",skip=1)
TPC <- read.table("TSA_postCrist.txt",skip=1)

DACL <- read.table("DMSO_antCanal_Le.txt",skip=1)
DACrL <- read.table("DMSO_antCrist_Le.txt",skip=1)
DAML <- read.table("DMSO_antMac_Le.txt",skip=1)
DDRL <- read.table("DMSO_dorsalRoof_Le.txt",skip=1)
DPCL <- read.table("DMSO_postCanal_Le.txt",skip=1)
DPCrL <- read.table("DMSO_postCrist_Le.txt",skip=1)
TACL <- read.table("TSA_antCanal_Le.txt",skip=1)
TACrL <- read.table("TSA_antCrist_Le.txt",skip=1)
TAML <- read.table("TSA_antMac_Le.txt",skip=1)
TDRL <- read.table("TSA_dorsalRoof_Le.txt",skip=1)
TPCL <- read.table("TSA_postCrist_Le.txt",skip=1)


num <- data.frame(Name=character(),Mean=double(),Median=double(),stringsAsFactors=FALSE)
i <- 0

getData <- function (df){
  #df <- plyr::rename(df,c("V1"="AvgSig","V2"="n","V3"="T","V4"="ttrip","V5"="tdiff",
  #                        "V6"="Exp","V7"="Page","V8"="cpp"))
  m <- mean(df$V2)
  med <- median(df$V2)
  avg <- mean(df$V1)
  medsig <- median(df$V1)
  #dfname <- deparse(substitute(df))
  print(paste("Mean signal of",dfname,"is",avg,"& median is",medsig,
              " while mean n is",m,"and median is",med))
  #num[i,] <- c(m,med)
  return(c(m,med))
}

categories <- list(DAC=DAC,DACr=DACr,DAM=DAM,DDR=DDR,DPC=DPC,DPCr=DPCr,TAC=TAC,TACr=TACr,TAM=TAM,
                   TDR=TDR,TPC=TPC,DACL=DACL,DACrL=DACrL,DAML=DAML,DDRL=DDRL,
                   DPCL=DPCL,DPCrL=DPCrL,TACL=TACL,TACrL=TACrL,TAML=TAML,TDRL=TDRL,TPCL=TPCL)
#num <- lapply(categories,getData)
#I want a single dataframe with this info
#I am getting a list of dataframes, one per data set

#num <- sapply(list(DAC=DAC,DACr=DACr),getData)
#Could add to num with a loop instead of lapply, but it'd take longer
#Does that matter at this point?
#How much time should I spend on elegance and efficiency rather than brute-forcing it
catnames <- names(categories)

for(j in categories) {
  i <- i+1
  #dfname <- deparse(substitute(j))
  num[i,1] <- catnames[i]
  num[i,2:3] <- getData(j) 
}

num$factors <- c("antCanal","antCrist","antMac","dorsalRoof","postCanal","postCrist",
             "antCanal","antCrist","antMac","dorsalRoof","postCrist",
            "antCanal_Le","antCrist_Le","antMac_Le","dorsalRoof_Le","postCanal_Le","postCrist_Le",
            "antCanal_Le","antCrist_Le","antMac_Le","dorsalRoof_Le","postCrist_Le")

num$exp <- rep(c("DMSO","TSA","DMSO","TSA"),c(6,5,6,5))


ggplot(data=num,aes(x=Mean,y=Median))+geom_point()+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  geom_text(aes(label=Name),hjust=0, vjust=0)
#might be good to color code the corresponding points

#Median only plot? DMSO vs TSA? Averages
ggplot(data=num,aes(x=Mean,y=Median))+geom_boxplot()+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  geom_text(aes(label=Name),hjust=0, vjust=0)
#might be good to color code the corresponding points