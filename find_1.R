##output survey_form data file in R data frame(df2) and csv file

library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(xlsx)
library(gridExtra)
library(gmodels)
library(VennDiagram)
library(tidyr)

setwd("C:/Users/YK Huang/Documents/UIC-2/big data/tryout_biopy/find_abs_r")

##import##
raw <- read.csv(file="a5_voc.csv", header=T, sep=",")
#raw <- read.csv(file="C:/Users/YK Huang/a5.csv", header=T, sep=",")

##abs key
abskey <- read.xlsx("ab_group_key.xlsx", 1,header = T) # read first sheet

a5 <- raw[,c("X","AB","DP")]
colnames(a5) <- c("no","ab","dp")

  a5$dp <- str_sub(a5$dp,1,4)
    a5 <- a5[as.numeric(a5$dp)>1999,]  #clear year
#apply(a5["ab"], 1, str_locate, pattern="FINDINGS")[1,]==1
#apply(a5["ab"], 1, str_locate, pattern="RESULTS")[1,]==1
b1 <- rep(NA,nrow(a5))
b2 <- c()
 for (i in 1:nrow(a5)){
   b3 <- as.character(a5[i,2])
   nchar(b3)
    if (is.na(str_locate(b3,"FINDINGS")[1])==F){ w.start <- str_locate(b3,"FINDINGS")[1]}
      else if(is.na(str_locate(b3,"RESULTS")[1])==F){ w.start <- str_locate(b3,"RESULTS")[1]}
      else if(is.na(str_locate(b3,"MEASUREMENTS AND MAIN RESULTS")[1])==F){ w.start <- str_locate(b3,"MEASUREMENTS AND MAIN RESULTS")[1]}
      else {w.start <- 1}
   b2[i] <- str_sub(b3,w.start,) #all
    if(w.start!=1){
      b1[i] <- str_sub(b3,w.start,)  #only format in finding/results
      }
   }

  a6 <- subset(cbind(a5,b1),is.na(b1)==F) ##only format in finding/results
    a6$b1 <-tolower(a6$b1) 

    ##sort key
    for (k in 1:ncol(abskey)){
      ks1 <- colnames(abskey)[k]
      ks2 <- abskey[,k][!is.na(abskey[,k])]
        ks5 <- paste0(" ",ks2," ")
        ks6 <- paste0(" ",ks2,".")
        ks7 <- paste0(" ",ks2,",")
        ks8 <- paste0(" ",ks2,";")
          ks3 <- paste(c(ks5,ks6,ks7,ks8),collapse = '|')
          assign(paste0("ocs.",ks1),ks3)
    }
      ##
      #ocs.renal <- paste(c("renal","kidney"),collapse = '|')
      #ocs.obse <- paste(c("obesity","BMI","waist"),collapse = '|')
      #ocs.bloodpressure <- paste(c("sbp","dbp","blood pressure","hytertension"),collapse = '|')
      #ocs.hearing <- paste(c("hearing"),collapse = '|')
      #ocs.diabetes <- paste(c("diabetes"),collapse = '|')
      #ocs.depress <- paste(c("depress","mental","depresstion"),collapse = '|')
      
    c.ocs <- c(apropos("^ocs.")) #DO NOT ADD new.ocs. below!
    
      a6c <- matrix(nrow=nrow(a6),ncol=length(c.ocs))
      for (c in 1:length(c.ocs)){
        a6c[,c] <- str_detect(a6$b1,get(c.ocs[c]))
      }
    
        colnames(a6c) <- str_sub(c.ocs,5,)
      
      a7 <- cbind(a6,a6c)
      a7$dp <- as.numeric(a7$dp)
      
        m7 <- matrix(nrow=length(c.ocs),ncol = length(table(a7$dp)),NA)
        m8 <- matrix(nrow=length(c.ocs),ncol = length(table(a7$dp)),NA)
      for (y in 1:length(table(a7$dp))){
        a8 <- a7[a7$dp==levels(as.factor(a7$dp))[y],]
        a8 <- a8[ -c(1:4) ] #remove org(a7) col1,2,3,4
          for (m in 1:length(c.ocs)){
            #n
            m7[m,y] <- length(a8[,m]) - table(a8[,m])["FALSE"]
            # %
            m8[m,y] <- 1-(table(a8[,m])["FALSE"]/length(a8[,m]))
          }
      }
        
      colnames(m7) <- levels(as.factor(a7$dp))
      colnames(m8) <- levels(as.factor(a7$dp))

          #apropos->find object w/ names
          rownames(m7) <- str_sub(c.ocs,5,)
          rownames(m8) <- str_sub(c.ocs,5,)
          #wide to long          
            m7long <- cbind( gather( as.data.frame(m7)),
                           rep( rownames(m7),dim(m7)[2]))
            colnames(m7long) <- c("year","n","g")
            
            m8long <- cbind( gather( as.data.frame(m8)),
                             rep( rownames(m8),dim(m8)[2]))
            colnames(m8long) <- c("year","n","g")
        #plot
          p <- ggplot(m8long,aes(y=n,x=as.numeric(year),color=g))+geom_point(size=3)
          p <- p+geom_line(linetype = "solid",show.legend = F)
          p <- p+labs(y = "% in related references",x="year")
          p
        
          p1 <- p + expand_limits(x = 2020)+geom_text(data = m8long[m8long$year=="2018",],aes(label=g),hjust=-0.5,show.legend=F)
          
          ggsave("out.png",p1,width = 12)