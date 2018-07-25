library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(gridExtra)
library(tidyr)

#read raw
  #df_scopus_raw <- read.csv("C:/Users/yhuan25/Documents/YKUIC/PP/proposal_work/1a_bibilometric/2_raw_scopus_export/LB1_no2.csv",header=T)

## calculate FF,reture a long table
FF <- function(input.df,expo,hecon,mesh){
  
  df2 <- input.df
  
  #make lower character
  df2$Index.Keywords <- str_to_lower(df2$Index.Keywords)
  
  #2. make a plot over year. from 1994 to 2017
  m.year <- matrix(nrow=1,ncol=2017-1994+1)
  colnames(m.year) <- c(1994:2017)
  rownames(m.year) <- c(hecon) #temp
  
  #get which article qualified (Qarticle), and make a subset for it
  #df3 <- df2[which(str_detect(a$Index.Keywords,MeSHstr_dia)==T),]
  #calculate in which year, how many Qarticle are in it, and put the number in m.year
  for (i in 1994:2017){
    m.year[1,i-1993] <-  length(which(df2[which(str_detect(df2$Index.Keywords,mesh)==T),]$Year==i))
  }
  
  #make to a long table for ggplot
  mylong <- cbind( gather( as.data.frame(m.year)),
                   rep( rownames(m.year),dim(m.year)[2]),
                   rep( expo,dim(m.year)[2])
  )
  colnames(mylong) <- c("year","n","healthoutcome","exposure")
  return(mylong)
}      
##


##step 1. read  file
#1 no2 / 2 pm2.5 / 3 noise
#A CVD / B DIA / C RES / D pre? / E mental
  df_1A <- read.csv("C:/Users/yhuan25/Documents/YKUIC/PP/proposal_work/1a_bibilometric/2_raw_scopus_export/LB1A_no2_cvd.csv",header=T)
  df_2A <- read.csv("C:/Users/yhuan25/Documents/YKUIC/PP/proposal_work/1a_bibilometric/2_raw_scopus_export/LB2A_pm25_cvd.csv",header=T)
  df_3A <- read.csv("C:/Users/yhuan25/Documents/YKUIC/PP/proposal_work/1a_bibilometric/2_raw_scopus_export/LB3A_noise_cvd.csv",header=T)

  df_1B <- read.csv("C:/Users/yhuan25/Documents/YKUIC/PP/proposal_work/1a_bibilometric/2_raw_scopus_export/LB1B_no2_dia.csv",header=T)
  df_2B <- read.csv("C:/Users/yhuan25/Documents/YKUIC/PP/proposal_work/1a_bibilometric/2_raw_scopus_export/LB2B_pm25_dia.csv",header=T)
  df_3B <- read.csv("C:/Users/yhuan25/Documents/YKUIC/PP/proposal_work/1a_bibilometric/2_raw_scopus_export/LB3B_noise_dia.csv",header=T)
  
##testing different
  # df_1A <- df_1A[which(str_detect(df_1A$Index.Keywords,c("nitrogen dioxide"))==T),]
  #  df_1B <- df_1B[which(str_detect(df_1B$Index.Keywords,c("nitrogen dioxide"))==T),]
  #  df_2A <- df_2A[which(str_detect(df_2A$Index.Keywords,c("particulate matter"))==T),]
  #  df_2B <- df_2B[which(str_detect(df_2B$Index.Keywords,c("particulate matter"))==T),]
  #  df_3A <- df_3A[which(str_detect(df_3A$Index.Keywords,c("noise"))==T),]
  #  df_3B <- df_3B[which(str_detect(df_3B$Index.Keywords,c("noise"))==T),]
  
##step 1-1. made intersect file: with two or more exposure with same health
  
  F_dual <- function(A,B){
                  temp_df <- rbind(A,B)
                  temp_df <- temp_df[temp_df$EID%in%intersect(A$EID,B$EID),]
                  temp_df <- temp_df[!duplicated(temp_df$EID), ]
  }
  
  
  df_duale_12A <- F_dual(df_1A,df_2A)
  df_duale_12B <- F_dual(df_1B,df_2B)
  
  df_duale_13A <- F_dual(df_1A,df_3A)
  df_duale_13B <- F_dual(df_1B,df_3B)
  
  df_duale_23A <- F_dual(df_2A,df_3A)
  df_duale_23B <- F_dual(df_2B,df_3B)
  
  df_trie_123A <- F_dual(df_duale_12A,df_duale_13A) #n=2
  df_trie_123B <- F_dual(df_duale_12B,df_duale_13B) #n=0
  
##step 1-2. made intersect file: with one exposure with cardiometebolic (A+B)  
  
  df_1_CnM <- F_dual(df_1A,df_1B)
  df_2_CnM <- F_dual(df_2A,df_2B)
  df_3_CnM <- F_dual(df_3A,df_3B)
  
##step 1-3. made a function for union
  
  F_union_df <- function(A,B){
    temp_df <- rbind(A,B)
    temp_df <- temp_df[temp_df$EID%in%union(A$EID,B$EID),]
    temp_df <- temp_df[!duplicated(temp_df$EID), ]
  }
  
  df_1_CM <- F_union_df(df_1A,df_1B)
  df_2_CM <- F_union_df(df_2A,df_2B)
  df_3_CM <- F_union_df(df_3A,df_3B)
  
    df_d12_CM <- F_dual(df_1_CM,df_2_CM)
    df_d13_CM <- F_dual(df_1_CM,df_3_CM)
    df_d23_CM <- F_dual(df_2_CM,df_3_CM)
  
  df_duale_any_A <- F_union_df(df_duale_12A,df_duale_13A) %>% F_union_df(.,df_duale_23A)
  df_duale_np_A <- F_union_df(df_duale_13A,df_duale_23A)
  
  df_duale_any_B <- F_union_df(df_duale_12B,df_duale_13B) %>% F_union_df(.,df_duale_23B)
  df_duale_np_B <- F_union_df(df_duale_13B,df_duale_23B)
  
  df_duale_any_CM <- F_union_df(df_duale_any_A,df_duale_any_B)
  df_duale_np_CM <- F_union_df(df_duale_np_A,df_duale_np_B)
    
#step 2. made MeSH(EMTREE) term
  MeSHstr_cvd <- paste(c("stroke",
                         "brain ischemia",
                         "cardiovascular disease",
                         "congestive heart failure",
                         "hypertension",
                         "acute heart infarction",
                         "cerebrovascular accident"
                        ), collapse = '|')  ##use all lower character!
  
  
  MeSHstr_dia <- paste(c("non insulin dependent diabetes mellitus",
                         "diabetes mellitus",
                         "type 2 diabetes mellitus",
                         "insulin resistance",
                         "glucose blood level",
                         "glucose homeostasis"
                        ), collapse = '|')  ##use all lower character!

##step 2.1 cardiomatablioc mesh
      MeSHstr_CM <- paste(c(  MeSHstr_cvd, MeSHstr_dia  ), collapse = '|')
  
##step 2.2 study design hypothesis-testing design vs non hypothesis design
      EMTREE_hypo <-  paste(c("case control study",
                              "prospective studies",
                              "case-control studies",
                              "cohort studies",
                              "longitudinal study",
                              "retrospective study",
                              "clinical study"
                            ), collapse = '|') 
      
      EMTREE_nohy <-  paste(c("cross-sectional",
                             "ecological"
                            ), collapse = '|') 
      
  #get how many
      table(str_detect(df_3A$Index.Keywords,EMTREE_hypo))
      table(str_detect(df_3B$Index.Keywords,EMTREE_nohy))

  ##longtable
  ##FF(inputdf,exposure,healthoutcome,mesh)
  ml.1A <- FF(df_1A,"no2","cvd",MeSHstr_cvd)
  ml.2A <- FF(df_2A,"pm2.5","cvd",MeSHstr_cvd)
  ml.3A <- FF(df_3A,"noise","cvd",MeSHstr_cvd)
  
  ml.1B <- FF(df_1B,"no2","diabetes",MeSHstr_dia)
  ml.2B <- FF(df_2B,"pm2.5","diabetes",MeSHstr_dia)
  ml.3B <- FF(df_3B,"noise","diabetes",MeSHstr_dia)
  
    ml.d12A <- FF(df_duale_12A,"no2+pm2.5","cvd",MeSHstr_cvd)
    ml.d23A <- FF(df_duale_23A,"pm2.5+noise","cvd",MeSHstr_cvd)
    ml.d13A <- FF(df_duale_13A,"no2+noise","cvd",MeSHstr_cvd)
    
    ml.d12B <- FF(df_duale_12A,"no2+pm2.5","dia",MeSHstr_dia)
    ml.d23B <- FF(df_duale_23A,"pm2.5+noise","dia",MeSHstr_dia)
    ml.d13B <- FF(df_duale_13A,"no2+noise","dia",MeSHstr_dia)
    
      ml.t123A <- FF(df_trie_123A,"no2+pm2.5+noise","cvd",MeSHstr_cvd)
      ml.t123B <- FF(df_trie_123B,"no2+pm2.5+noise","cvd",MeSHstr_dia)
    
    #df C+M union
    ml.1_CM <- FF(df_1_CM,"no2","cardiometabolic",MeSHstr_CM)
    ml.2_CM <- FF(df_2_CM,"pm2.5","cardiometabolic",MeSHstr_CM)
    ml.3_CM <- FF(df_3_CM,"noise","cardiometabolic",MeSHstr_CM)
    
    #df C&M intersect
    ml.1_CnM <- FF(df_1_CnM,"no2","cvd+dia",MeSHstr_CM)
    ml.2_CnM <- FF(df_2_CnM,"pm2.5","cvd+dia",MeSHstr_CM)
    ml.3_CnM <- FF(df_3_CnM,"noise","cvd+dia",MeSHstr_CM)
    
    ml.d12_CM <- FF(df_d12_CM,"no2+pm2.5","cardiometabolic",MeSHstr_CM)
    ml.d23_CM <- FF(df_d23_CM,"pm2.5+noise","cardiometabolic",MeSHstr_CM)
    ml.d13_CM <- FF(df_d13_CM,"no2+noise","cardiometabolic",MeSHstr_CM)
    
    #df any dual combind
    ml.duale_any_A <- FF(df_duale_any_A,"joint exposure","cvd",MeSHstr_cvd)
    ml.duale_np_A <- FF(df_duale_np_A,"TRAP+noise","cvd",MeSHstr_cvd)
    ml.duale_any_B <- FF(df_duale_any_B,"joint exposure","diabetes",MeSHstr_cvd)
    ml.duale_np_B <- FF(df_duale_np_B,"TRAP+noise","diabetes",MeSHstr_cvd)
    
    ml.duale_any_CM <- FF(df_duale_any_CM,"joint exposure","cardiometabolic",MeSHstr_CM)
    ml.duale_np_CM <- FF(df_duale_np_CM,"TRAP+noise","cardiometabolic",MeSHstr_CM)
      
    
  #put long together
  mmasterlong <- rbind(ml.1A,ml.2A,ml.3A
                       ,ml.1B,ml.2B,ml.3B
                       ,ml.d12A,ml.d23A,ml.d13A
                       ,ml.d12B,ml.d23B,ml.d13B
                       ,ml.1_CM,ml.2_CM,ml.3_CM
                       ,ml.1_CnM,ml.2_CnM,ml.3_CnM
                       ,ml.t123A,ml.t123B
                       ,ml.d12_CM,ml.d13_CM,ml.d23_CM
                       ,ml.duale_any_A,ml.duale_any_B,ml.duale_any_CM
                       ,ml.duale_np_A,ml.duale_np_B,ml.duale_np_CM
                       )
  
    mmasterlong$h_e <- paste(mmasterlong$healthoutcome,mmasterlong$exposure,sep="_")
      mmasterlong[mmasterlong==0] <- NA #make 0 to NA
  
  ##make plot df
    #mlong_draw <- subset(mmasterlong,exposure%in% c("no2+pm2.5","no2+noise","pm2.5+noise"))
    mlong_draw <- subset(mmasterlong,healthoutcome%in%c("cvd") & exposure %in% c("no2","pm2.5","noise") )
      
    #plot
  p <- ggplot(mlong_draw,aes(y=n,x=as.numeric(year),color=h_e))+geom_point(size=3)
  p <- p+geom_line(linetype = "solid",show.legend = F)
  p <- p+geom_point(aes(shape=exposure),size=3) #make shape for different exposure
  p <- p+labs(y = "n references",x="year")
  p
  
  p1 <- p + expand_limits(x = 1990:2020,y=0)+geom_text(data = mlong_draw[mlong_draw$year=="2018",],aes(label=exposure),hjust=-0.5,show.legend=F)
  p1 <- p1+geom_vline(xintercept =1994,color="red")
  #p1 <- p1+geom_vline(xintercept =2010,color="firebrick",linetype="dashed")
  p1
  
  ggsave("4_r_output/out.png",p1,width = 12)
  
  
  
  
  
##addiont to test hypo v nohy
  #by 3 expo + 2 outcome
  hypo <- matrix(ncol=3,nrow=6)
 
  hypo[1,1] <- table(str_detect(df_1A$Index.Keywords,EMTREE_hypo))[2]
  hypo[1,2] <- table(str_detect(df_1A$Index.Keywords,EMTREE_nohy))[2]
  hypo[1,3] <- dim(df_1A)[1]
  
  hypo[2,1] <- table(str_detect(df_2A$Index.Keywords,EMTREE_hypo))[2]
  hypo[2,2] <- table(str_detect(df_2A$Index.Keywords,EMTREE_nohy))[2]
  hypo[2,3] <- dim(df_2A)[1]
  
  hypo[3,1] <- table(str_detect(df_3A$Index.Keywords,EMTREE_hypo))[2]
  hypo[3,2] <- table(str_detect(df_3A$Index.Keywords,EMTREE_nohy))[2]
  hypo[3,3] <- dim(df_3A)[1]
  
  hypo[4,1] <- table(str_detect(df_1B$Index.Keywords,EMTREE_hypo))[2]
  hypo[4,2] <- table(str_detect(df_1B$Index.Keywords,EMTREE_nohy))[2]
  hypo[4,3] <- dim(df_1B)[1]
  
  hypo[5,1] <- table(str_detect(df_2B$Index.Keywords,EMTREE_hypo))[2]
  hypo[5,2] <- table(str_detect(df_2B$Index.Keywords,EMTREE_nohy))[2]
  hypo[5,3] <- dim(df_2B)[1]
  
  hypo[6,1] <- table(str_detect(df_3B$Index.Keywords,EMTREE_hypo))[2]
  hypo[6,2] <- table(str_detect(df_3B$Index.Keywords,EMTREE_nohy))[2]
  hypo[6,3] <- dim(df_3B)[1]
  
  F_hypovsnonhypo <- function(df){
      c(table(str_detect(df$Index.Keywords,EMTREE_hypo))[2],
         table(str_detect(df$Index.Keywords,EMTREE_nohy))[2],
         dim(df)[1])
      }
  
  list(df_1A,df_2A,df_3A) %>% sapply(.,F_hypovsnonhypo) %>% t
  list(df_1B,df_2B,df_3B) %>% sapply(.,F_hypovsnonhypo) %>% t
  list(df_1_CM,df_2_CM,df_3_CM) %>% sapply(.,F_hypovsnonhypo) %>% t
  
  #rbind(F_hypovsnonhypo(df_1A),F_hypovsnonhypo(df_1B))
  
  F_list_hypo_byyear <- function(df){
  mt1 <- matrix(ncol=3,nrow=2017-1994+1)
   for (i in 1994:2017){
     mt1[i-1993,] <-  subset(df,Year==i) %>% F_hypovsnonhypo()
   }
  rownames(mt1) <- c(1994:2017)
  colnames(mt1) <- c("hypo n","non hypo n","year n")
  mt1
  }
  
  F_list_hypo_byyear(df_3A)
  
  F_list_hypo_byyear(df_3A) %>% as.data.frame %>% gather %>% cbind(.,rep(c(1994:2017),3)) %>% 
    `colnames<-`(c("key","n","year")) %>%  ##pipe line for colnaes(.) <- c(****)  
    ggplot(.,aes(x=as.numeric(year),y=n,color=key))+geom_point() -> p3
  p3 <- p3 + expand_limits(x = 1990:2020,y=0)
  p3
  
##step 3.1 find out what are the first few study
  ##noise hypothesis early days/counts
   F_list_hypo_byyear(df_3_CM)
   
   df_early_3_CM <- subset(df_3_CM,Year<2007)
   
   table(str_detect(df_early_3_CM$Index.Keywords,EMTREE_hypo))
   
   as.character(df_early_3_CM$Title[which(str_detect(df_early_3_CM$Index.Keywords,EMTREE_hypo)==T)])
   
   table(str_detect(df_early_3_CM$Abstract[which(str_detect(df_early_3_CM$Index.Keywords,EMTREE_hypo)==T)], c("work|patients") ))

outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}
