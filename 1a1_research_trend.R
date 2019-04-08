#mod:2018/5/24: make it clear and make it suitable for mental disorder
#mod:2018/7/4: add more mental disorder
#mod:2018/7/11:add VennD
#mod:2018/7/28: recode df names (MN:DEFG/ change duale12->d12 change TRAP+noise:see 1.7)
#               recode MN into 456 (4:no2-MN,5:pm25-MN,6:noise-MN)
#mod:2018/8/21 big change for the defination of df_1A....etc (from related to exposure to only exposure!)


#Sys.setlocale("LC_ALL", "English")
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(gridExtra)
library(tidyr)
library(VennDiagram)
library(wordcloud2)

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
  c(  df %>% .$Index.Keywords %>% str_detect(.,EMTREE_h.design) %>% table %>% .["TRUE"],
      #table(str_detect(df$Index.Keywords,EMTREE_nohy))["TRUE"],
      df %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)==F),] %>% .$Index.Keywords %>% str_detect(.,EMTREE_nh.design) %>% table %>% .["TRUE"],
      dim(df)[1])
}

##F5.
F_list_hnh_byyear <- function(df){
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


    #step 2. made MeSH(EMTREE) term for subset/ CM(cvd,dia)/ MN(dep,anx,add,cog)
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
    
#raw_tri_CM$Index.Keywords <- tolower(raw_tri_CM$Index.Keywords)
##step 1.3 make subset (df_1A,etc)
#1 no2 / 2 pm2.5 / 3 noise 
#A CVD / B DIA / C DEP / D ANX / E ADD(ADHD)
df_1A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_1B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]
df_2A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_2B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]
df_3A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_3B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

#MNCG
df_1D <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_1E <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_1F <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_1G <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]
df_2D <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_2E <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_2F <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_2G <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]
df_3D <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_3E <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_3F <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_3G <- raw_tri_MN %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]


##step 1-4. made intersect file: with two or more exposure with same health
df_d12A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_d12B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

df_d13A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_d13B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

df_d23A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_d23B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

df_t123A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_t123B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

#MN
df_d12D <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_d12E <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_d12F <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_d12G <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]

df_d13D <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_d13E <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_d13F <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_d13G <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]

df_d23D <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_d23E <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_d23F <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_d23G <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]

df_t123D <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dep)==T),]
df_t123E <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_anx)==T),]
df_t123F <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_add)==T),]
df_t123G <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cog)==T),]

##step 1-5. made intersect file: with one exposure with cardiometebolic (A+B) #not used
#deleted

##step 1-6. made a function for union
df_1CM <- F_union_df(df_1A,df_1B)
df_2CM <- F_union_df(df_2A,df_2B)
df_3CM <- F_union_df(df_3A,df_3B)

df_d12CM <- F_union_df(df_d12A,df_d12B)
df_d13CM <- F_union_df(df_d13A,df_d13B)
df_d23CM <- F_union_df(df_d23A,df_d23B)

df_t123CM <- F_union_df(df_t123A,df_t123B)

#MN
df_1MN <- F_union_df(df_1D,df_1E) %>% F_union_df(.,df_1F) %>% F_union_df(.,df_1G)
df_2MN <- F_union_df(df_2D,df_2E) %>% F_union_df(.,df_2F) %>% F_union_df(.,df_2G)
df_3MN <- F_union_df(df_3D,df_3E) %>% F_union_df(.,df_3F) %>% F_union_df(.,df_3G)

df_d12MN <- F_union_df(df_d12D,df_d12E) %>% F_union_df(.,df_d12F) %>% F_union_df(.,df_d12G)
df_d13MN <- F_union_df(df_d13D,df_d13E) %>% F_union_df(.,df_d13F) %>% F_union_df(.,df_d13G)
df_d23MN <- F_union_df(df_d23D,df_d23E) %>% F_union_df(.,df_d23F) %>% F_union_df(.,df_d23G)

df_t123MN <- F_union_df(df_t123D,df_t123E) %>% F_union_df(.,df_t123F) %>% F_union_df(.,df_t123G)

#dual..any (1+2 2+3 3+1)...np (only 1+3 2+3)
df_duale_any_A <- F_union_df(df_d12A,df_d13A) %>% F_union_df(.,df_d23A)
df_duale_np_A <- F_union_df(df_d13A,df_d23A)

df_duale_any_B <- F_union_df(df_d12B,df_d13B) %>% F_union_df(.,df_d23B)
df_duale_np_B <- F_union_df(df_d13B,df_d23B)

df_duale_any_CM <- F_union_df(df_duale_any_A,df_duale_any_B)
df_duale_np_CM <- F_union_df(df_duale_np_A,df_duale_np_B)

#MN
df_duale_any_D <- F_union_df(df_d12D,df_d13D) %>% F_union_df(.,df_d23D)
df_duale_np_D <- F_union_df(df_d13D,df_d23D)

df_duale_any_E <- F_union_df(df_d12E,df_d13E) %>% F_union_df(.,df_d23E)
df_duale_np_E <- F_union_df(df_d13E,df_d23E)

df_duale_any_F <- F_union_df(df_d12F,df_d13F) %>% F_union_df(.,df_d23F)
df_duale_np_F <- F_union_df(df_d13F,df_d23F)

df_duale_any_G <- F_union_df(df_d12G,df_d13G) %>% F_union_df(.,df_d23G)
df_duale_np_G <- F_union_df(df_d13G,df_d23G)

df_duale_any_MN <- F_union_df(df_duale_any_D,df_duale_any_E) %>% F_union_df(.,df_duale_any_F) %>% F_union_df(.,df_duale_any_G)
df_duale_np_MN <- F_union_df(df_duale_np_D,df_duale_np_E) %>% F_union_df(.,df_duale_np_F) %>% F_union_df(.,df_duale_np_G)

#step 1-7. make mulitple exposure dfs. (1)no2+pm2.5 and no noise (2)no2/pm2.5+noise (include no2+pm2.5+noise)
#CM
df_joint_no2pm25_A <- F_union_df(df_d12A,df_t123A) %>% .[!(.$EID %in% df_t123A$EID),]
df_joint_wnoise_A <- F_union_df(df_d13A,df_d23A) %>% F_union_df(.,df_t123A) 

df_joint_no2pm25_B <- F_union_df(df_d12B,df_t123B) %>% .[!(.$EID %in% df_t123B$EID),]
df_joint_wnoise_B <- F_union_df(df_d13B,df_d23B) %>% F_union_df(.,df_t123B) 

df_joint_no2pm25_CM <- F_union_df(df_d12CM,df_t123CM) %>% .[!(.$EID %in% df_t123CM$EID),]
df_joint_wnoise_CM <- F_union_df(df_d13CM,df_d23CM) %>% F_union_df(.,df_t123CM) 

#MN
df_joint_no2pm25_D <- F_union_df(df_d12D,df_t123D) %>% .[!(.$EID %in% df_t123D$EID),]
df_joint_wnoise_D <- F_union_df(df_d13D,df_d23D) %>% F_union_df(.,df_t123D)

df_joint_no2pm25_E <- F_union_df(df_d12E,df_t123E) %>% .[!(.$EID %in% df_t123E$EID),]
df_joint_wnoise_E <- F_union_df(df_d13E,df_d23E) %>% F_union_df(.,df_t123E)

df_joint_no2pm25_F <- F_union_df(df_d12F,df_t123F) %>% .[!(.$EID %in% df_t123F$EID),]
df_joint_wnoise_F <- F_union_df(df_d13F,df_d23F) %>% F_union_df(.,df_t123F)

df_joint_no2pm25_G <- F_union_df(df_d12G,df_t123G) %>% .[!(.$EID %in% df_t123G$EID),]
df_joint_wnoise_G <- F_union_df(df_d13G,df_d23G) %>% F_union_df(.,df_t123G)

df_joint_no2pm25_MN <- F_union_df(df_d12MN,df_t123MN) %>% .[!(.$EID %in% df_t123MN$EID),]
df_joint_wnoise_MN <- F_union_df(df_d13MN,df_d23MN) %>% F_union_df(.,df_t123MN)

##step 2.2 study design hypothesis-testing design vs non hypothesis design
EMTREE_h.design <-  paste(c("case control study",
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

EMTREE_nh.design <-  paste(c("cross-sectional",
                        "ecological"
                        ,"health survey"
), collapse = '|') 

#get how many
#df_3MN %>% .$Index.Keywords %>% str_detect(.,EMTREE_h.design) %>% table
#df_3A %>% .$Index.Keywords %>% str_detect(.,EMTREE_nh.design) %>% table
#df_1MN %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% .$Index.Keywords %>% str_detect(.,EMTREE_nh.design) %>% table
df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% dim() %>% .[1]

##step 3. make longtable for plot
##FF(inputdf,exposure,healthoutcome,mesh)
mlg.1A <- FF(df_1A,"no2","cvd",MeSHstr_cvd)
mlg.2A <- FF(df_2A,"pm2.5","cvd",MeSHstr_cvd)
mlg.3A <- FF(df_3A,"noise","cvd",MeSHstr_cvd)

mlg.1B <- FF(df_1B,"no2","diabetes",MeSHstr_dia)
mlg.2B <- FF(df_2B,"pm2.5","diabetes",MeSHstr_dia)
mlg.3B <- FF(df_3B,"noise","diabetes",MeSHstr_dia)

mlg.1D <- FF(df_1D,"no2","depression",MeSHstr_dep)
mlg.2D <- FF(df_2D,"pm2.5","depression",MeSHstr_dep)
mlg.3D <- FF(df_3D,"noise","depression",MeSHstr_dep)

mlg.1E <- FF(df_1E,"no2","anxitey",MeSHstr_anx)
mlg.2E <- FF(df_2E,"pm2.5","anxitey",MeSHstr_anx)
mlg.3E <- FF(df_3E,"noise","anxitey",MeSHstr_anx)

mlg.1F <- FF(df_1F,"no2","ADHD",MeSHstr_add)
mlg.2F <- FF(df_2F,"pm2.5","ADHD",MeSHstr_add)
mlg.3F <- FF(df_3F,"noise","ADHD",MeSHstr_add)

mlg.1G <- FF(df_1G,"no2","cognition",MeSHstr_cog)
mlg.2G <- FF(df_2G,"pm2.5","cognition",MeSHstr_cog)
mlg.3G <- FF(df_3G,"noise","cognition",MeSHstr_cog)

mlg.d12A <- FF(df_d12A,"1+2","cvd",MeSHstr_cvd)
mlg.d23A <- FF(df_d23A,"pm2.5+noise","cvd",MeSHstr_cvd)
mlg.d13A <- FF(df_d13A,"no2+noise","cvd",MeSHstr_cvd)

mlg.d12B <- FF(df_d12A,"1+2","diabetes",MeSHstr_dia)
mlg.d23B <- FF(df_d23A,"pm2.5+noise","diabetes",MeSHstr_dia)
mlg.d13B <- FF(df_d13A,"no2+noise","diabetes",MeSHstr_dia)

mlg.t123A <- FF(df_t123A,"no2+pm2.5+noise","cvd",MeSHstr_cvd)
mlg.t123B <- FF(df_t123B,"no2+pm2.5+noise","cvd",MeSHstr_dia)

mlg.1_CM <- FF(df_1CM,"no2","cardiometabolic",MeSHstr_CM)
mlg.2_CM <- FF(df_2CM,"pm2.5","cardiometabolic",MeSHstr_CM)
mlg.3_CM <- FF(df_3CM,"noise","cardiometabolic",MeSHstr_CM)

mlg.d12_CM <- FF(df_d12CM,"1+2","cardiometabolic",MeSHstr_CM)
mlg.d23_CM <- FF(df_d23CM,"pm2.5+noise","cardiometabolic",MeSHstr_CM)
mlg.d13_CM <- FF(df_d13CM,"no2+noise","cardiometabolic",MeSHstr_CM)

mlg.1_MN <- FF(df_1MN,"no2","mental disorder*",MeSHstr_MN)
mlg.2_MN <- FF(df_2MN,"pm2.5","mental disorder*",MeSHstr_MN)
mlg.3_MN <- FF(df_3MN,"noise","mental disorder*",MeSHstr_MN)

mlg.d12_MN <- FF(df_d12MN,"1+2","cardiometabolic",MeSHstr_CM)
mlg.d23_MN <- FF(df_d23MN,"pm2.5+noise","cardiometabolic",MeSHstr_CM)
mlg.d13_MN <- FF(df_d13MN,"no2+noise","cardiometabolic",MeSHstr_CM)

mlg.d12_CM <- FF(df_d12CM,"1+2","cardiometabolic",MeSHstr_CM)
mlg.d23_CM <- FF(df_d23CM,"pm2.5+noise","cardiometabolic",MeSHstr_CM)
mlg.d13_CM <- FF(df_d13CM,"no2+noise","cardiometabolic",MeSHstr_CM)

##
mlg.joint_no2pm25_A <- FF(df_joint_no2pm25_A,"no2+pm2.5","cvd",MeSHstr_cvd)
mlg.joint_no2pm25_B <- FF(df_joint_no2pm25_B,"no2+pm2.5","diabetes",MeSHstr_dia)
mlg.joint_no2pm25_CM <- FF(df_joint_no2pm25_CM,"no2+pm2.5","cardiometabolic",MeSHstr_CM)
mlg.joint_no2pm25_D <- FF(df_joint_no2pm25_D,"no2+pm2.5","depression",MeSHstr_dep)
mlg.joint_no2pm25_E <- FF(df_joint_no2pm25_E,"no2+pm2.5","anxitey",MeSHstr_anx)
mlg.joint_no2pm25_F <- FF(df_joint_no2pm25_F,"no2+pm2.5","ADHD",MeSHstr_add)
mlg.joint_no2pm25_G <- FF(df_joint_no2pm25_G,"no2+pm2.5","cognition",MeSHstr_cog)
mlg.joint_no2pm25_MN <- FF(df_joint_no2pm25_MN,"no2+pm2.5","mental disorder*",MeSHstr_MN)

mlg.joint_wnoise_A <- FF(df_joint_wnoise_A,"TRAP+noise","cvd",MeSHstr_cvd)
mlg.joint_wnoise_B <- FF(df_joint_wnoise_B,"TRAP+noise","diabetes",MeSHstr_dia)
mlg.joint_wnoise_CM <- FF(df_joint_wnoise_CM,"TRAP+noise","cardiometabolic",MeSHstr_CM)
mlg.joint_wnoise_D <- FF(df_joint_wnoise_D,"TRAP+noise","depression",MeSHstr_dep)
mlg.joint_wnoise_E <- FF(df_joint_wnoise_E,"TRAP+noise","anxitey",MeSHstr_anx)
mlg.joint_wnoise_F <- FF(df_joint_wnoise_F,"TRAP+noise","ADHD",MeSHstr_add)
mlg.joint_wnoise_G <- FF(df_joint_wnoise_G,"TRAP+noise","cognition",MeSHstr_cog)
mlg.joint_wnoise_MN <- FF(df_joint_wnoise_MN,"TRAP+noise","mental disorder*",MeSHstr_MN)

#
mlg.3_ocup_cm <- df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==T),] %>% 
  FF(.,"occupational noise","cardiometabolic",MeSHstr_CM)
mlg.3_nooc_cm <- df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% 
  FF(.,"non-occupational noise","cardiometabolic",MeSHstr_CM)


#put long together
mmasterlong <- do.call(rbind,mget((apropos("^mlg."))))

#mmasterlong$h_e <- paste(mmasterlong$exposure,mmasterlong$healthoutcome,sep="-")
mmasterlong$e_o <- paste(mmasterlong$exposure,mmasterlong$healthoutcome,sep="-")
mmasterlong[mmasterlong==0] <- NA #make 0 to NA

##make plot df
#mlong_draw <- subset(mmasterlong,exposure%in% c("no2+pm2.5","no2+noise","pm2.5+noise"))
#mlong_draw <- subset(mmasterlong,healthoutcome%in%c("ADHD","anxitey","depression","cognition") & exposure %in% c("no2","pm2.5","noise") )
mlong_draw <- subset(mmasterlong,healthoutcome%in%c("cardiometabolic") & exposure %in% c("no2","pm2.5","noise","no2+pm2.5","TRAP+noise") )

#mlong_draw$groups = factor(mlong_draw$e_o, levels=c("no2-cardiometabolic","pm2.5-cardiometabolic","noise-cardiometabolic"))
mlong_draw$groups = factor(mlong_draw$exposure, levels=c("no2","pm2.5","noise","no2+pm2.5","TRAP+noise"))

#plot
#p <- ggplot(mlong_draw,aes(y=n,x=as.numeric(year),color=h_e))+geom_point()
#p <- p+geom_line(linetype = "solid",show.legend = F)
#p <- p+geom_point(aes(shape=exposure),size=3) #make shape for different exposure
#p <- p+labs(y = "n references",x="year")
#p
p <- ggplot(mlong_draw,aes(y=n,x=as.numeric(year),color=groups))+geom_point()
p <- p+geom_line(linetype = "solid",show.legend = F)
p <- p+geom_point(aes(shape=mlong_draw$groups),size=5) #make shape for different exposure
p <- p+labs(y = "n, references",x="year")
p

#p1 <- p + expand_limits(x = 1990:2020,y=0)+geom_text(data = mlong_draw[mlong_draw$year=="2018",],aes(label=exposure),hjust=-0.5,show.legend=F)
#p1 <- p1 + geom_vline(xintercept =2009,color="firebrick",linetype="dashed")
p1 <- p + scale_x_continuous(breaks=c(1994,2000,2005,2010,2017))
#p1 <- p1 + scale_color_manual(values=c("firebrick4", "firebrick","firebrick1","steelblue4","steelblue","steelblue1"))
#p1 <- p1 + scale_color_manual(values=c("darkgoldenrod4","darkgoldenrod","darkgoldenrod1","firebrick4", "firebrick","firebrick1","steelblue4","steelblue","steelblue1","seagreen4","seagreen3","seagreen2"))
#p1 <- p1 + scale_color_manual(values=c("indianred4", "indianred2","indianred","steelblue4","indianred1","steelblue1"))
#p1 <- p1 + theme_bw()
#p1

#manual overwrite (the factor do not changed)
p1 <- p1+
  scale_colour_manual(name = "groups",
                      labels = c("sinlge-NO2","single-PM2.5","single-Noise","mulitple-no2+pm2.5","mulitple-with noise"),
                      values = c("gray54","gray54","gray54","gray0","gray0")) +   
  scale_shape_manual(name = "groups",
                     labels = c("sinlge-NO2","single-PM2.5","single-Noise","mulitple-no2+pm2.5","mulitple-with noise"),
                     values = c(15,16,17,16,17))
p1 <- p1 + theme_bw()+theme(axis.text=element_text(size=12),
                             axis.title=element_text(size=14,face="bold"))
p1

#output
#ggsave("4_r_output/out.png",p1,width = 12)
ggsave("4_r_output/out.png",p1,width = 8)


##step 4. to test h-nh desigh

##
list(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% sapply(.,F_hypovsnonhypo) %>% t
list(df_1A,df_2A,df_3A,df_joint_no2pm25_A,df_joint_wnoise_A) %>% sapply(.,F_hypovsnonhypo) %>% t
list(df_1B,df_2B,df_3B,df_joint_no2pm25_B,df_joint_wnoise_B) %>% sapply(.,F_hypovsnonhypo) %>% t

list(df_1MN,df_2MN,df_3MN,df_3CM) %>% sapply(.,F_hypovsnonhypo) %>% t
list(df_3F,df_3D,df_2F,df_2D) %>% sapply(.,F_hypovsnonhypo) %>% t

#rbind(F_hypovsnonhypo(df_1A),F_hypovsnonhypo(df_1B))

##F5


F_list_hnh_byyear(df_2F)

F_list_hnh_byyear(df_3A) %>% as.data.frame %>% gather %>% cbind(.,rep(c(1994:2017),3)) %>% 
  `colnames<-`(c("key","n","year")) %>%  .[1:48,] %>% ##pipe line for colnaes(.) <- c(****)  
  ggplot(.,aes(x=as.numeric(year),y=n,color=key))+geom_point(size=5) -> p3
p3 <- p3 + expand_limits(x = 1990:2020,y=0)
p3 <- p3+geom_vline(xintercept =2007,color="firebrick",linetype="dashed")
p3

##step 3.1 find out what are the first few study
##noise hypothesis early days/counts

df_early_3CM <- subset(df_3CM,Year<2007)
table(str_detect(df_early_3CM$Index.Keywords,EMTREE_h.design))

as.character(df_early_3CM$Title[which(str_detect(df_early_3CM$Index.Keywords,EMTREE_h.design)==T)])
table(str_detect(df_early_3CM$Abstract[which(str_detect(df_early_3CM$Index.Keywords,EMTREE_h.design)==T)], c("worker|patients") ))



###stpe 3.2
df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==T),] %>% F_hypovsnonhypo
df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% F_hypovsnonhypo

#df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==T),] %>% .[which(str_detect(.$Index.Keywords,EMTREE_nohy)==T),] %>%.$Year
#df_3_CM %>% .[-which(str_detect(.$Index.Keywords,"occupational")==T),] %>% .[-which(str_detect(.$Index.Keywords,EMTREE_hypo)==T),] %>%
#  .[which(str_detect(.$Index.Keywords,EMTREE_nohy)==T),] %>%.$Title
df_3CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% F_list_hnh_byyear()


outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

##part4
###find top cited literture
st_topcited_1CM <- df_1CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)
st_topcited_2CM <- df_2CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)
st_topcited_3CM <- df_3CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)

st_topcited_no2pm25CM <- df_joint_no2pm25_CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)
st_topcited_wnoiseCM <- df_joint_wnoise_CM %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19)] %>% head(10)

write.csv(st_topcited_wnoiseCM,"4_r_output/OUT_st_topcited.csv")

##find top indexkeyword
st_topindexkw_1CM <- df_1CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(df_1CM)/10) )
st_topindexkw_2CM <- df_2CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(df_2CM)/10) )
#st_topindexkw_2CM <- df_2CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)
st_topindexkw_3CM <- df_3CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(df_3CM)/10) )
st_topindexkw_no2pm25CM <- df_joint_no2pm25_CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(df_joint_no2pm25_CM)/10) )
st_topindexkw_wnoiseCM <- df_joint_wnoise_CM %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(df_joint_wnoise_CM)/10) )

st_topindexkw_overlap <- intersect(st_topindexkw_1CM[,1],st_topindexkw_2CM[,1]) %>% intersect(.,st_topindexkw_3CM[,1]) #%>% intersect(.,st_topindexkw_no2pm25CM[,1]) %>% intersect(.,st_topindexkw_wnoiseCM[,1]) 
#mannual add
st_topindexkw_overlap <- c(st_topindexkw_overlap,"analysis", "risk factor", "air pollutants", "air pollutant", "human", "pollution","iron","enviorment",
                           "noise","nitrogen dioxide","particulate matter","pollutant","health","controlled study","major clinical study","male","female",
                           "cohort analysis","age","risk","health risk","nitrogen dioxides","exposure","diseases","noise pollution","major clinical study")
#st_topindexkw_common <- rbind(df_1CM,df_2CM,df_3CM) %>% .[!duplicated(.$EID), ] %>% .[!duplicated(.$Title), ] %>% 
#                        .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% 
#                        .[which(st_topindexkw_common[,1]%in%st_topindexkw_overlap),] 
st_uniindexkw_1CM <- st_topindexkw_1CM[-which(st_topindexkw_1CM[,1]%in%st_topindexkw_overlap),]
st_uniindexkw_2CM <- st_topindexkw_2CM[-which(st_topindexkw_2CM[,1]%in%st_topindexkw_overlap),]
st_uniindexkw_3CM <- st_topindexkw_3CM[-which(st_topindexkw_3CM[,1]%in%st_topindexkw_overlap),]
st_uniindexkw_no2pm25CM <- st_topindexkw_no2pm25CM[-which(st_topindexkw_no2pm25CM[,1]%in%st_topindexkw_overlap),]
st_uniindexkw_wnoiseCM <- st_topindexkw_wnoiseCM[-which(st_topindexkw_wnoiseCM[,1]%in%st_topindexkw_overlap),]

#please use this wordcloud
F_recheck_re_freq <- function(df,w){
  a <- df %>% .$Index.Keywords %>% as.character %>% tolower 
  table(str_detect(a,w))["TRUE"]
}
F_recheck_re_freq2 <- function(aa1,aa2){
bbb <- as.character(aa1[,1]) %>% paste0("; ",.,";")%>% sapply(.,F_recheck_re_freq,df=aa2) %>% as.data.frame() %>% cbind(.,as.character(aa1[,1])) %>%`colnames<-`(c("Freq","term")) #%>% .[order(-.$Freq),] 
bbb$cut <- str_split_fixed(as.character(bbb$term),".T",2)
bb2 <- bbb[is.na(bbb$Freq)==F,]
bb3 <- cbind(bb2$cut[,1],(bb2$Freq)) %>% as.data.frame()
colnames(bb3) <- c("w","freq")
bb3$freq <- as.numeric(as.character(bb3$freq))
bb3}
#bb4 <-F_recheck_re_freq2(st_uniindexkw_3CM,df_3CM)
#bb4 <-F_recheck_re_freq2(st_uniindexkw_no2pm25CM,df_joint_no2pm25_CM)
bb4 <-F_recheck_re_freq2(st_uniindexkw_wnoiseCM,df_joint_wnoise_CM)

write.csv(bb4,"4_r_output/OUT_st_topindexkw.csv")

#1,3CM (24,12)/2CM (56,28) /4CM (31,16)--old
#1,3CM (24,12)/2CM (56,28) /4CM (31,16)--new
bb4 %>% wordcloud2(.,
                                 color = ifelse(.[, 2] > 4, 'Black', 
                                                ifelse(.[, 2] > 2, '#808080', '#D3D3D3')),size=0.15  )
#

##find top authorkeyword (rewnal)
#st_topauthorkw_1CM <- df_1CM %>% .$Author.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)
#st_topauthorkw_2CM <- df_2CM %>% .$Author.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)
#st_topauthorkw_3CM <- df_3CM %>% .$Author.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)
#st_topauthorkw_no2pm25CM <- df_joint_wnoise_CM %>% .$Author.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)
#st_topauthorkw_wnoiseCM <- df_1CM %>% .$Author.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% head(20)
#may count two more more from different system(EMTRMM+MESH)


write.csv(st_topauthorkw_3CM,"4_r_output/OUT_st_topauthorkw.csv")



#misc
###find certain articles (from the research trend)
#cartlist <- df_2A %>% subset(.,.$Year %in% c(2002:2017) )
#cartlist <- df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% .[which(str_detect(.$Index.Keywords,EMTREE_hypo)==F),]
cartlist <- df_joint_no2pm25_CM %>% .[which(str_detect(tolower(.$Index.Keywords),"Canada|canada")==T),] 
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

F_threeVDbreakdown(df_1CM,df_2CM,df_3CM,"cardiometabolic") %>% ggsave("4_r_output/vd_out.png",.,width=5,height=5)

union(df_1CM$Title,df_2CM$Title) %>% union(.,df_3CM$Title) %>% length()

#precision1
set.seed(515)
rrr <- rbind(  df_1CM[sample(1:120,12,replace = F),], 
               df_2CM[sample(1:556,56,replace = F),],
               df_3CM[sample(1:121,12,replace = F),], 
               df_joint_no2pm25_CM[sample(1:344,34,replace = F),], 
               df_joint_wnoise_CM[sample(1:38,4,replace = F),]    ) %>% cbind(c(rep("1",12),rep("2",56),rep("3",12),rep("4",34),rep("5",4)),.)
write.csv(rrr,"4_r_output/OUT_precision_515.csv")

#
rbind(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% F_hypovsnonhypo()

set.seed(515)
rr2 <- rbind (rbind(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)=="TRUE"),] %>% .[sample(1:176,18,replace = F),] ,
rbind(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)=="FALSE"),] %>% .[which(str_detect(.$Index.Keywords,EMTREE_nh.design)=="TRUE"),] %>% .[sample(1:95,10,replace = F),] ) %>%
  cbind(c(rep("H",18),rep("NH",10)),.)
write.csv(rr2,"4_r_output/OUT_precisionHNH_515.csv")

#master.big table
bigtable <- raw_tri_CM
bigtable$Classification_cardiometabolic <- " "
bigtable$Classification_cardiovascular <- " "
bigtable$Classification_diabetes  <- " "
bigtable$Classification_no2.only  <- " "
bigtable$Classification_pm2.5.only  <- " "
bigtable$Classification_noise.only  <- " "
bigtable$Classification_no2.pm2.5  <- " "
bigtable$Classification_no2.pm2.5.noise  <- " "

bigtable[which(bigtable$EID %in% c(as.character(df_1CM$EID),
                                   as.character(df_2CM$EID),
                                   as.character(df_3CM$EID),
                                   as.character(df_joint_no2pm25_CM$EID),
                                   as.character(df_joint_wnoise_CM$EID)))
         ,"Classification_cardiometabolic"] <- "Yes"
bigtable[which(bigtable$EID %in% c(as.character(df_1A$EID),
                                   as.character(df_2A$EID),
                                   as.character(df_3A$EID),
                                   as.character(df_joint_no2pm25_A$EID),
                                   as.character(df_joint_wnoise_A$EID)))
         ,"Classification_cardiovascular"] <- "Yes"
bigtable[which(bigtable$EID %in% c(as.character(df_1B$EID),
                                   as.character(df_2B$EID),
                                   as.character(df_3B$EID),
                                   as.character(df_joint_no2pm25_B$EID),
                                   as.character(df_joint_wnoise_B$EID)))
         ,"Classification_diabetes"] <- "Yes"

bigtable[which(bigtable$EID %in% df_1CM$EID),"Classification_no2.only"] <- "Yes"
bigtable[which(bigtable$EID %in% df_2CM$EID),"Classification_pm2.5.only"] <- "Yes"
bigtable[which(bigtable$EID %in% df_3CM$EID),"Classification_noise.only"] <- "Yes"
bigtable[which(bigtable$EID %in% df_joint_no2pm25_CM$EID),"Classification_no2.pm2.5"] <- "Yes"
bigtable[which(bigtable$EID %in% df_joint_wnoise_CM$EID),"Classification_no2.pm2.5.noise"] <- "Yes"

write.csv(bigtable,"4_r_output/OUT_PLOSONE_data.csv")