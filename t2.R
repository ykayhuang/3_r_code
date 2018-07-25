#mod:2018/5/24: make it clear and make it suitable for mental disorder
#mod:2018/7/4: add more mental disorder
#mod:2018/7/11:add VennD
#recode MN into 456 (4:no2-MN,5:pm25-MN,6:noise-MN)

#Sys.setlocale("LC_ALL", "English")
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(gridExtra)
library(tidyr)
library(VennDiagram)

##step 0. Function zone
##F1. FF calculate FF,reture a long table
FF <- function(input.df,expo,hecon,mesh){
  df2 <- input.df
  #1.make lower character
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

##F2. combine two, with intersect and nondiplicated
F_dual <- function(A,B){
  temp_df <- rbind(A,B)
  temp_df <- temp_df[temp_df$EID%in%intersect(A$EID,B$EID),]
  temp_df <- temp_df[!duplicated(temp_df$EID), ]
}

##F3. combine two, with union and nondiplicated
F_union_df <- function(A,B){
  temp_df <- rbind(A,B)
  temp_df <- temp_df[temp_df$EID%in%union(A$EID,B$EID),]
  temp_df <- temp_df[!duplicated(temp_df$EID), ]
}

##F4. 
F_hypovsnonhypo <- function(df){
  c(  df %>% .$Index.Keywords %>% str_detect(.,EMTREE_hypo) %>% table %>% .["TRUE"],
      #table(str_detect(df$Index.Keywords,EMTREE_nohy))["TRUE"],
      df %>% .[which(str_detect(.$Index.Keywords,EMTREE_hypo)==F),] %>% .$Index.Keywords %>% str_detect(.,EMTREE_nohy) %>% table %>% .["TRUE"],
      dim(df)[1])
}

##F5.
F_list_hypo_byyear <- function(df){
  mt1 <- matrix(ncol=3,nrow=2017-1994+1)
  for (i in 1994:2017){
    mt1[i-1993,] <-  subset(df,Year==i) %>% F_hypovsnonhypo()
  }
  rownames(mt1) <- c(1994:2017)
  colnames(mt1) <- c("hypo n","non hypo n","year n")
  mt1
}

##F6. 
F_occupationvcnon <- function(df){
  c(table(str_detect(df$Index.Keywords,"occupational"))["TRUE"],
    table(str_detect(df$Index.Keywords,"occupational"))["FALSE"],
    dim(df)[1])
}

##F7.
F_list_ocup_byyear <- function(df){
  mt1 <- matrix(ncol=3,nrow=2017-1994+1)
  for (i in 1994:2017){
    mt1[i-1993,] <-  subset(df,Year==i) %>% F_occupationvcnon()
  }
  rownames(mt1) <- c(1994:2017)
  colnames(mt1) <- c("ocup n","non ocup n","year n")
  mt1
}  

##function zone ends


##step 1. read  file
##raw data from scopus
raw_1_CM <- read.csv("2_raw_scopus_export/LB1_cm_no2.csv",header=T)
raw_2_CM <- read.csv("2_raw_scopus_export/LB2_cm_pm25.csv",header=T)
raw_3_CM <- read.csv("2_raw_scopus_export/LB3_cm_noise.csv",header=T)

raw_4_MN <- read.csv("2_raw_scopus_export/LB4_mncg_no2.csv",header=T)
raw_5_MN <- read.csv("2_raw_scopus_export/LB5_mncg_pm25.csv",header=T)
raw_6_MN <- read.csv("2_raw_scopus_export/LB6_mncg_noise.csv",header=T)

raw_tri_CM <- rbind(raw_1_CM,raw_2_CM,raw_3_CM) %>% .[!duplicated(.$EID), ]
raw_tri_MN <- rbind(raw_4_MN,raw_5_MN,raw_6_MN) %>% .[!duplicated(.$EID), ]


    #step 2. made MeSH(EMTREE) term for subset/ CM(cvd,dia)/ MN(dep,anx,add)
    #CM cardiomeabolic
    MeSHstr_cvd <- paste(c("stroke",
                           "brain ischemia",
                           "cardiovascular disease",
                           "congestive heart failure",
                           "hypertension",
                           "acute heart infarction",
                           "cerebrovascular accident",
                           "cardio-vascular disease"
    ), collapse = '|')  ##use all lower character!
    
    MeSHstr_dia <- paste(c("non insulin dependent diabetes mellitus",
                           "diabetes mellitus",
                           "type 2 diabetes mellitus",
                           "insulin resistance",
                           "glucose blood level",
                           "glucose homeostasis"
    ), collapse = '|')  ##use all lower character!
    
    MeSHstr_CM <- paste(c(  MeSHstr_cvd, MeSHstr_dia  ), collapse = '|')
    
    #MN mental disorder
    MeSHstr_dep <- paste(c("depression",
                           "major depression",
                           "st segment depression",
                           "center for epidemiological studies depression scale"
    ), collapse = '|')  ##use all lower character!
    
    MeSHstr_anx <- paste(c("anxiety"
    ), collapse = '|')  ##use all lower character!
  
    MeSHstr_add <- paste(c("attention deficit disorder",
                           "attention deficit disorder with hyperactivity"
    ), collapse = '|')  ##use all lower character!
    
    MeSHstr_cog <- paste(c("cognition",
                           "cognition disorders",
                           "cognitive functions",
                           "cognitive defect",
                           "cognitive assessment",
                           "cognitive development"
    ), collapse = '|')  ##use all lower character!
    
    MeSHstr_MN <- paste(c(  MeSHstr_dep, MeSHstr_anx,MeSHstr_add,MeSHstr_cog  ), collapse = '|')
    
    #excluding hearing impairment peoples and heart valve
    MeSHstr_EXCLU <- paste(c("persons with hearing impairments",
                           "hearing impaired person",
                           "hearing aid",
                           "heart valve",
                           "signal noise ratio"
    ), collapse = '|')  ##use all lower character!
    
    
##step 1.3 make subset (df_1A,etc)
#1 no2 / 2 pm2.5 / 3 noise 
#A CVD / B DIA / C DEP / D ANX / E ADD(ADHD)
df_1A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_1B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]
df_2A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_2B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]
df_3A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_3B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

#MNCG
df_1C <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_1D <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_1E <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_1F <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]
df_2C <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_2D <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_2E <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_2F <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]
df_3C <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_3D <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_3E <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_3F <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]


##step 1-4. made intersect file: with two or more exposure with same health

df_duale_12A <- F_dual(df_1A,df_2A)
df_duale_12B <- F_dual(df_1B,df_2B)

df_duale_13A <- F_dual(df_1A,df_3A)
df_duale_13B <- F_dual(df_1B,df_3B)

df_duale_23A <- F_dual(df_2A,df_3A)
df_duale_23B <- F_dual(df_2B,df_3B)

df_trie_123A <- F_dual(df_duale_12A,df_duale_13A) #n=8
df_trie_123B <- F_dual(df_duale_12B,df_duale_13B) #n=2

#MN
df_duale_12C <- F_dual(df_1C,df_2C)
df_duale_12D <- F_dual(df_1D,df_2D)
df_duale_12E <- F_dual(df_1E,df_2E)
df_duale_12F <- F_dual(df_1F,df_2F)

df_duale_13C <- F_dual(df_1C,df_3C)
df_duale_13D <- F_dual(df_1D,df_3D)
df_duale_13E <- F_dual(df_1E,df_3E)
df_duale_13F <- F_dual(df_1F,df_3F)

df_duale_23C <- F_dual(df_2C,df_3C)
df_duale_23D <- F_dual(df_2D,df_3D)
df_duale_23E <- F_dual(df_2E,df_3E)
df_duale_23F <- F_dual(df_2F,df_3F)

df_trie_123C <- F_dual(df_duale_12C,df_duale_13C) 
df_trie_123D <- F_dual(df_duale_12D,df_duale_13D) 
df_trie_123E <- F_dual(df_duale_12E,df_duale_13E) 
df_trie_123F <- F_dual(df_duale_12F,df_duale_13F) 

##step 1-2. made intersect file: with one exposure with cardiometebolic (A+B)  

df_1_CnM <- F_dual(df_1A,df_1B)
df_2_CnM <- F_dual(df_2A,df_2B)
df_3_CnM <- F_dual(df_3A,df_3B)

##step 1-3. made a function for union

df_1_CM <- F_union_df(df_1A,df_1B)
df_2_CM <- F_union_df(df_2A,df_2B)
df_3_CM <- F_union_df(df_3A,df_3B)

df_d12_CM <- F_dual(df_1_CM,df_2_CM)
df_d13_CM <- F_dual(df_1_CM,df_3_CM)
df_d23_CM <- F_dual(df_2_CM,df_3_CM)

df_trie_123_CM <- F_union_df(df_trie_123A,df_trie_123B) #n=1

#MN
df_1_MN <- F_union_df(df_1C,df_1D) %>% F_union_df(.,df_1E) %>% F_union_df(.,df_1F)
df_2_MN <- F_union_df(df_2C,df_2D) %>% F_union_df(.,df_2E) %>% F_union_df(.,df_2F)
df_3_MN <- F_union_df(df_3C,df_3D) %>% F_union_df(.,df_3E) %>% F_union_df(.,df_3F)

df_d12_MN <- F_dual(df_1_MN,df_2_MN)
df_d13_MN <- F_dual(df_1_MN,df_3_MN)
df_d23_MN <- F_dual(df_2_MN,df_3_MN)

df_trie_123_MN <- F_union_df(df_trie_123C,df_trie_123D) %>% F_union_df(.,df_trie_123E) %>% F_union_df(.,df_trie_123F)

#dual..any (1+2 2+3 3+1)...np (only 1+3 2+3)
df_duale_any_A <- F_union_df(df_duale_12A,df_duale_13A) %>% F_union_df(.,df_duale_23A)
df_duale_np_A <- F_union_df(df_duale_13A,df_duale_23A)

df_duale_any_B <- F_union_df(df_duale_12B,df_duale_13B) %>% F_union_df(.,df_duale_23B)
df_duale_np_B <- F_union_df(df_duale_13B,df_duale_23B)

df_duale_any_CM <- F_union_df(df_duale_any_A,df_duale_any_B)
df_duale_np_CM <- F_union_df(df_duale_np_A,df_duale_np_B)

#MN
df_duale_any_C <- F_union_df(df_duale_12C,df_duale_13C) %>% F_union_df(.,df_duale_23C)
df_duale_np_C <- F_union_df(df_duale_13C,df_duale_23C)

df_duale_any_D <- F_union_df(df_duale_12D,df_duale_13D) %>% F_union_df(.,df_duale_23D)
df_duale_np_D <- F_union_df(df_duale_13D,df_duale_23D)

df_duale_any_E <- F_union_df(df_duale_12E,df_duale_13E) %>% F_union_df(.,df_duale_23E)
df_duale_np_E <- F_union_df(df_duale_13E,df_duale_23E)

df_duale_any_F <- F_union_df(df_duale_12F,df_duale_13F) %>% F_union_df(.,df_duale_23F)
df_duale_np_F <- F_union_df(df_duale_13F,df_duale_23F)

df_duale_any_MN <- F_union_df(df_duale_any_C,df_duale_any_D) %>% F_union_df(.,df_duale_any_E) %>% F_union_df(.,df_duale_any_F)
df_duale_np_MN <- F_union_df(df_duale_np_C,df_duale_np_D) %>% F_union_df(.,df_duale_np_E) %>% F_union_df(.,df_duale_np_F)


##step 2.2 study design hypothesis-testing design vs non hypothesis design
EMTREE_hypo <-  paste(c("case control study",
                        "case control studies",
                        "case-control study",
                        "case-control studies",
                        "prospective study",
                        "prospective studies",
                        "cohort study",
                        "cohort studies",
                        "longitudinal study",
                        "longitudinal studies",
                        "retrospective study",
                        "retrospective studies"
                        #,"major clinical study"
                        #,"major clinical studies"
), collapse = '|') 

EMTREE_nohy <-  paste(c("cross-sectional",
                        "ecological"
                        ,"health survey"
), collapse = '|') 

#get how many
df_3_MN %>% .$Index.Keywords %>% str_detect(.,EMTREE_hypo) %>% table

#df_3A %>% .$Index.Keywords %>% str_detect(.,EMTREE_nohy) %>% table
df_1_MN %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% .$Index.Keywords %>% str_detect(.,EMTREE_nohy) %>% table


##step 3. make longtable for plot
##FF(inputdf,exposure,healthoutcome,mesh)
mlg.1A <- FF(df_1A,"no2","cvd",MeSHstr_cvd)
mlg.2A <- FF(df_2A,"pm2.5","cvd",MeSHstr_cvd)
mlg.3A <- FF(df_3A,"noise","cvd",MeSHstr_cvd)

mlg.1B <- FF(df_1B,"no2","diabetes",MeSHstr_dia)
mlg.2B <- FF(df_2B,"pm2.5","diabetes",MeSHstr_dia)
mlg.3B <- FF(df_3B,"noise","diabetes",MeSHstr_dia)

mlg.1C <- FF(df_1C,"no2","depression",MeSHstr_dep)
mlg.2C <- FF(df_2C,"pm2.5","depression",MeSHstr_dep)
mlg.3C <- FF(df_3C,"noise","depression",MeSHstr_dep)

mlg.1D <- FF(df_1D,"no2","anxitey",MeSHstr_anx)
mlg.2D <- FF(df_2D,"pm2.5","anxitey",MeSHstr_anx)
mlg.3D <- FF(df_3D,"noise","anxitey",MeSHstr_anx)

mlg.1E <- FF(df_1E,"no2","ADHD",MeSHstr_add)
mlg.2E <- FF(df_2E,"pm2.5","ADHD",MeSHstr_add)
mlg.3E <- FF(df_3E,"noise","ADHD",MeSHstr_add)

mlg.1F <- FF(df_1E,"no2","cognition",MeSHstr_cog)
mlg.2F <- FF(df_2F,"pm2.5","cognition",MeSHstr_cog)
mlg.3F <- FF(df_3F,"noise","cognition",MeSHstr_cog)

mlg.d12A <- FF(df_duale_12A,"no2+pm2.5","cvd",MeSHstr_cvd)
mlg.d23A <- FF(df_duale_23A,"pm2.5+noise","cvd",MeSHstr_cvd)
mlg.d13A <- FF(df_duale_13A,"no2+noise","cvd",MeSHstr_cvd)

mlg.d12B <- FF(df_duale_12A,"no2+pm2.5","diabetes",MeSHstr_dia)
mlg.d23B <- FF(df_duale_23A,"pm2.5+noise","diabetes",MeSHstr_dia)
mlg.d13B <- FF(df_duale_13A,"no2+noise","diabetes",MeSHstr_dia)

mlg.t123A <- FF(df_trie_123A,"no2+pm2.5+noise","cvd",MeSHstr_cvd)
mlg.t123B <- FF(df_trie_123B,"no2+pm2.5+noise","cvd",MeSHstr_dia)

#df C+M union
mlg.1_CM <- FF(df_1_CM,"no2","cardiometabolic",MeSHstr_CM)
mlg.2_CM <- FF(df_2_CM,"pm2.5","cardiometabolic",MeSHstr_CM)
mlg.3_CM <- FF(df_3_CM,"noise","cardiometabolic",MeSHstr_CM)

#df C+M union
mlg.1_MN <- FF(df_1_MN,"no2","mental disorder*",MeSHstr_MN)
mlg.2_MN <- FF(df_2_MN,"pm2.5","mental disorder*",MeSHstr_MN)
mlg.3_MN <- FF(df_3_MN,"noise","mental disorder*",MeSHstr_MN)

#df C&M intersect
mlg.1_CnM <- FF(df_1_CnM,"no2","cvd+dia",MeSHstr_CM)
mlg.2_CnM <- FF(df_2_CnM,"pm2.5","cvd+dia",MeSHstr_CM)
mlg.3_CnM <- FF(df_3_CnM,"noise","cvd+dia",MeSHstr_CM)

mlg.d12_CM <- FF(df_d12_CM,"joint TRAP","cardiometabolic",MeSHstr_CM)
mlg.d23_CM <- FF(df_d23_CM,"pm2.5+noise","cardiometabolic",MeSHstr_CM)
mlg.d13_CM <- FF(df_d13_CM,"no2+noise","cardiometabolic",MeSHstr_CM)

#df any dual combind
mlg.duale_any_A <- FF(df_duale_any_A,"joint exposure","cvd",MeSHstr_cvd)
mlg.duale_np_A <- FF(df_duale_np_A,"TRAP+noise","cvd",MeSHstr_cvd)
mlg.duale_any_B <- FF(df_duale_any_B,"joint exposure","diabetes",MeSHstr_cvd)
mlg.duale_np_B <- FF(df_duale_np_B,"TRAP+noise","diabetes",MeSHstr_cvd)

mlg.duale_any_CM <- FF(df_duale_any_CM,"joint exposure","cardiometabolic",MeSHstr_CM)
mlg.duale_np_CM <- FF(df_duale_np_CM,"TRAP+noise","cardiometabolic",MeSHstr_CM)

mlg.duale_any_C <- FF(df_duale_any_C,"joint exposure","depression",MeSHstr_dep)
mlg.duale_np_C <- FF(df_duale_np_C,"TRAP+noise","depression",MeSHstr_dep)
mlg.duale_any_D <- FF(df_duale_any_D,"joint exposure","anxitey",MeSHstr_anx)
mlg.duale_np_D <- FF(df_duale_np_D,"TRAP+noise","anxitey",MeSHstr_anx)
mlg.duale_any_E <- FF(df_duale_any_E,"joint exposure","ADHD",MeSHstr_add)
mlg.duale_np_E <- FF(df_duale_np_E,"TRAP+noise","ADHD",MeSHstr_add)
mlg.duale_any_F <- FF(df_duale_any_F,"joint exposure","cognition",MeSHstr_cog)
mlg.duale_np_F <- FF(df_duale_np_F,"TRAP+noise","cognition",MeSHstr_cog)

mlg.duale_any_MN <- FF(df_duale_any_CM,"joint exposure","mental disorder*",MeSHstr_MN)
mlg.duale_np_MN <- FF(df_duale_np_CM,"TRAP+noise","mental disorder*",MeSHstr_MN)



mlg.3_ocup_cm <- df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==T),] %>% 
  FF(.,"occupational noise","cardiometabolic",MeSHstr_CM)
mlg.3_nooc_cm <- df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% 
  FF(.,"non-occupational noise","cardiometabolic",MeSHstr_CM)


#put long together
mmasterlong <- do.call(rbind,mget((apropos("^mlg."))))

mmasterlong$h_e <- paste(mmasterlong$healthoutcome,mmasterlong$exposure,sep="_")
mmasterlong[mmasterlong==0] <- NA #make 0 to NA

##make plot df
#mlong_draw <- subset(mmasterlong,exposure%in% c("no2+pm2.5","no2+noise","pm2.5+noise"))
#mlong_draw <- subset(mmasterlong,healthoutcome%in%c("ADHD","anxitey","depression","cognition") & exposure %in% c("no2","pm2.5","noise") )
mlong_draw <- subset(mmasterlong,healthoutcome%in%c("cardiometabolic") & exposure %in% c("occupational noise","non-occupational noise") )

#plot
p <- ggplot(mlong_draw,aes(y=n,x=as.numeric(year),color=h_e))+geom_point()
p <- p+geom_line(linetype = "solid",show.legend = F)
p <- p+geom_point(aes(shape=exposure),size=3) #make shape for different exposure
p <- p+labs(y = "n references",x="year")
p

p1 <- p #+ expand_limits(x = 1990:2020,y=0)+geom_text(data = mlong_draw[mlong_draw$year=="2018",],aes(label=exposure),hjust=-0.5,show.legend=F)
#p1 <- p1 + geom_vline(xintercept =2009,color="firebrick",linetype="dashed")
p1 <- p1 + scale_x_continuous(breaks=c(1994,2000,2005,2010,2017))
#p1 <- p1 + scale_color_manual(values=c("firebrick4", "firebrick","firebrick1","steelblue4","steelblue","steelblue1"))
#p1 <- p1 + scale_color_manual(values=c("darkgoldenrod4","darkgoldenrod","darkgoldenrod1","firebrick4", "firebrick","firebrick1","steelblue4","steelblue","steelblue1","seagreen4","seagreen3","seagreen2"))
#p1 <- p1 + scale_color_manual(values=c("indianred4", "indianred2","indianred","steelblue4","indianred1","steelblue1"))
p1 <- p1 + theme_bw()
p1

#output
ggsave("4_r_output/out.png",p1,width = 12)



##step 4. to test hypo v nohy
#by 3 expo + 2 outcome
#hypo <- matrix(ncol=3,nrow=6)

#hypo[1,1] <- table(str_detect(df_1A$Index.Keywords,EMTREE_hypo))[2]
#hypo[1,2] <- table(str_detect(df_1A$Index.Keywords,EMTREE_nohy))[2]
#hypo[1,3] <- dim(df_1A)[1]

#hypo[2,1] <- table(str_detect(df_2A$Index.Keywords,EMTREE_hypo))[2]
#hypo[2,2] <- table(str_detect(df_2A$Index.Keywords,EMTREE_nohy))[2]
#hypo[2,3] <- dim(df_2A)[1]

#hypo[3,1] <- table(str_detect(df_3A$Index.Keywords,EMTREE_hypo))[2]
#hypo[3,2] <- table(str_detect(df_3A$Index.Keywords,EMTREE_nohy))[2]
#hypo[3,3] <- dim(df_3A)[1]

#hypo[4,1] <- table(str_detect(df_1B$Index.Keywords,EMTREE_hypo))[2]
#hypo[4,2] <- table(str_detect(df_1B$Index.Keywords,EMTREE_nohy))[2]
#hypo[4,3] <- dim(df_1B)[1]

#hypo[5,1] <- table(str_detect(df_2B$Index.Keywords,EMTREE_hypo))[2]
#hypo[5,2] <- table(str_detect(df_2B$Index.Keywords,EMTREE_nohy))[2]
#hypo[5,3] <- dim(df_2B)[1]

#hypo[6,1] <- table(str_detect(df_3B$Index.Keywords,EMTREE_hypo))[2]
#hypo[6,2] <- table(str_detect(df_3B$Index.Keywords,EMTREE_nohy))[2]
#hypo[6,3] <- dim(df_3B)[1]

##
list(df_1_CM,df_2_CM,df_3_CM) %>% sapply(.,F_hypovsnonhypo) %>% t
list(df_1A,df_2A,df_3A) %>% sapply(.,F_hypovsnonhypo) %>% t
list(df_1B,df_2B,df_3B) %>% sapply(.,F_hypovsnonhypo) %>% t

list(df_1_MN,df_2_MN,df_3_MN,df_3_CM) %>% sapply(.,F_hypovsnonhypo) %>% t
list(df_3F,df_3D,df_2F,df_2D) %>% sapply(.,F_hypovsnonhypo) %>% t

#rbind(F_hypovsnonhypo(df_1A),F_hypovsnonhypo(df_1B))

##F5


F_list_hypo_byyear(df_2F)

F_list_hypo_byyear(df_3A) %>% as.data.frame %>% gather %>% cbind(.,rep(c(1994:2017),3)) %>% 
  `colnames<-`(c("key","n","year")) %>%  .[1:48,] %>% ##pipe line for colnaes(.) <- c(****)  
  ggplot(.,aes(x=as.numeric(year),y=n,color=key))+geom_point(size=5) -> p3
p3 <- p3 + expand_limits(x = 1990:2020,y=0)
p3 <- p3+geom_vline(xintercept =2007,color="firebrick",linetype="dashed")
p3

##step 3.1 find out what are the first few study
##noise hypothesis early days/counts

df_early_3_CM <- subset(df_3_CM,Year<2007)

table(str_detect(df_early_3_CM$Index.Keywords,EMTREE_hypo))

as.character(df_early_3_CM$Title[which(str_detect(df_early_3_CM$Index.Keywords,EMTREE_hypo)==T)])

table(str_detect(df_early_3_CM$Abstract[which(str_detect(df_early_3_CM$Index.Keywords,EMTREE_hypo)==T)], c("worker|patients") ))



###stpe 3.2

df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==T),] %>% F_hypovsnonhypo
df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% F_hypovsnonhypo

#df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==T),] %>% .[which(str_detect(.$Index.Keywords,EMTREE_nohy)==T),] %>%.$Year
#df_3_CM %>% .[-which(str_detect(.$Index.Keywords,"occupational")==T),] %>% .[-which(str_detect(.$Index.Keywords,EMTREE_hypo)==T),] %>%
#  .[which(str_detect(.$Index.Keywords,EMTREE_nohy)==T),] %>%.$Title
df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% F_list_hypo_byyear()


outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

###find top cited literture
head(df_1_CM$Cited.by)
a1 <- df_1_CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)
a2 <- df_2_CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)
a3 <- df_3_CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)

a4 <- df_d12_CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)
a5 <- df_duale_np_CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)

write.csv(a5,"4_r_output/a.csv")

##find top indexkeyword
z1 <- df_1_CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)
z2 <- df_2_CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)
z3 <- df_3_CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)
z5 <- df_duale_np_CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)

write.csv(z3,"4_r_output/keyword20.csv")

###find certain articles (from the research trend)
cartlist <- df_2C %>% subset(.,.$Year %in% c(2002:2017) )
#cartlist <- df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% .[which(str_detect(.$Index.Keywords,EMTREE_hypo)==F),]

write.csv(cartlist,"4_r_output/find_certain_articles.csv")

##draw VennD (show the overlaping)
#change the df
F_threeVDbreakdown <- function(df_1,df_2,df_3,titletext){
    grid.newpage()
    vd.try <- draw.triple.venn( area1 = dim(df_1)[1], 
                                area2 = dim(df_2)[1], 
                                area3 = dim(df_3)[1],
                                n12 = (F_dual(df_1,df_2) %>% dim %>%.[1]), 
                                n13 = (F_dual(df_1,df_3) %>% dim %>%.[1]), 
                                n23 = (F_dual(df_2,df_3) %>% dim %>%.[1]), 
                                n123 =(F_dual(df_1,df_2) %>% F_dual(.,df_3) %>% dim %>% .[1]), 
                                category = c("no2", "pm2.5", "noise"), lty = "blank", 
                                fill = c("goldenrod1", "palevioletred1","steelblue"))
    #add title (by grid extra)
    grid.arrange(gTree(children=vd.try), top=titletext)
    }

F_threeVDbreakdown(df_1_CM,df_2_CM,df_3_CM,"cardiometabolic") %>% ggsave("4_r_output/vd_out.png",.,width=5,height=5)

union(df_1C$Title,df_2C$Title) %>% union(.,df_3C$Title) %>% length()