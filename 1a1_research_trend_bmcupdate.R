#this is the sensitivy analysis for the BMC review
#mod: 20190406, all based on the 1a1
#mod: use the title and abstract review to enhance the accuray, and redo all thing to see if differ?
#mod: no MN and no OCCU

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
##F7.

##function zone ends

##step 1. read  file
##raw data from scopus
raw_1_CM <- read.csv("2_raw_scopus_export/LB1_cm_no2.csv",header=T)
raw_2_CM <- read.csv("2_raw_scopus_export/LB2_cm_pm25.csv",header=T)
raw_3_CM <- read.csv("2_raw_scopus_export/LB3_cm_noise.csv",header=T)
raw_tri_CM <- rbind(raw_1_CM,raw_2_CM,raw_3_CM) %>% .[!duplicated(.$EID), ]

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

##step 1-4. made intersect file: with two or more exposure with same health
df_d12A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_d12B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

df_d13A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_d13B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

df_d23A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_d23B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

df_t123A <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),]
df_t123B <- raw_tri_CM %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==T),] %>% .[which(str_detect(.$Index.Keywords,c("noise"))==T),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_EXCLU)==F),] %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),]

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

#dual..any (1+2 2+3 3+1)...np (only 1+3 2+3)
df_duale_any_A <- F_union_df(df_d12A,df_d13A) %>% F_union_df(.,df_d23A)
df_duale_np_A <- F_union_df(df_d13A,df_d23A)
df_duale_any_B <- F_union_df(df_d12B,df_d13B) %>% F_union_df(.,df_d23B)
df_duale_np_B <- F_union_df(df_d13B,df_d23B)

df_duale_any_CM <- F_union_df(df_duale_any_A,df_duale_any_B)
df_duale_np_CM <- F_union_df(df_duale_np_A,df_duale_np_B)

#step 1-7. make mulitple exposure dfs. (1)no2+pm2.5 and no noise (2)no2/pm2.5+noise (include no2+pm2.5+noise)
#CM
df_joint_no2pm25_A <- F_union_df(df_d12A,df_t123A) %>% .[!(.$EID %in% df_t123A$EID),]
df_joint_wnoise_A <- F_union_df(df_d13A,df_d23A) %>% F_union_df(.,df_t123A) 

df_joint_no2pm25_B <- F_union_df(df_d12B,df_t123B) %>% .[!(.$EID %in% df_t123B$EID),]
df_joint_wnoise_B <- F_union_df(df_d13B,df_d23B) %>% F_union_df(.,df_t123B) 

df_joint_no2pm25_CM <- F_union_df(df_d12CM,df_t123CM) %>% .[!(.$EID %in% df_t123CM$EID),]
df_joint_wnoise_CM <- F_union_df(df_d13CM,df_d23CM) %>% F_union_df(.,df_t123CM) 

#step 1-8 (new,title review and abs review)
#first make a big df
df_1CM <- mutate(df_1CM,G.expo=1)
df_2CM <- mutate(df_2CM,G.expo=2)
df_3CM <- mutate(df_3CM,G.expo=3)
df_joint_no2pm25_CM <- mutate(df_joint_no2pm25_CM,G.expo=4)
df_joint_wnoise_CM <- mutate(df_joint_wnoise_CM,G.expo=5)

mf1 <- rbind(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% mutate(G.CM=1)
mf1$G.A <- ifelse(mf1$EID%in%c(df_1A$EID%>%as.character(),
                               df_2A$EID%>%as.character(),
                               df_3A$EID%>%as.character(),
                               df_joint_no2pm25_A$EID%>%as.character(),
                               df_joint_wnoise_A$EID%>%as.character()),1,0)
mf1$G.B <- ifelse(mf1$EID%in%c(df_1B$EID%>%as.character(),
                               df_2B$EID%>%as.character(),
                               df_3B$EID%>%as.character(),
                               df_joint_no2pm25_B$EID%>%as.character(),
                               df_joint_wnoise_B$EID%>%as.character()),1,0)
#then use the title review to kick out some ()
KICKOUT_title <- paste(c("ventilation",
                         "hearing",
                         "respiratory",
                         "pulmonary",
                         "shift work",
                         "lung","asthma","renal","kidney","copd","liver"
), collapse = '|')  ##use all lower character!

mf1$Title <- mf1$Title%>%as.character()%>%tolower()
mf1$Abstract <- mf1$Abstract%>%as.character()%>%tolower()
mf2 <- mf1 %>% .[which(str_detect(.$Title,KICKOUT_title)==F),] 
#use abtract to relocate muliple exposure
mf3 <- mf2
mf3$G.expo <- ifelse(mf3$EID %in% (
  mf3 %>% .[which(str_detect(.$Abstract,c("nitrogen dioxide|no2|no<inf>2"))==T),] %>% .[which(str_detect(.$Abstract,c("fine particulate matter|fine particul|pm2.|pm 2.|pm<inf>|aerodynamic diameter less than 2.5"))==T),] %>% .$EID
),4,mf3$G.expo)
# mf3$G.expo <- ifelse(mf3$EID %in% (
#   mf3 %>% .[which(str_detect(.$Abstract,c("nitrogen dioxide|no2|no<inf>"))==T),] %>% .[which(str_detect(.$Abstract,c("fine particulate matter|fine particul|pm2.|pm 2.|pm<inf>|aerodynamic diameter less than 2.5"))==T),] %>% .[which(str_detect(.$Abstract,c("noise|sound"))==T),] %>% .$EID
# ),5,mf3$G.expo)
mf3$G.expo <- ifelse(mf3$EID %in% (
   c(mf3 %>% .[which(str_detect(.$Abstract,c("fine particulate matter|fine particul|pm2.|pm 2.|pm<inf>|aerodynamic diameter less than 2.5"))==T),] %>% .[which(str_detect(.$Abstract,c("noise|sound"))==T),] %>% .$EID %>%as.character(),
     mf3 %>% .[which(str_detect(.$Abstract,c("nitrogen dioxide|no2|no<inf>2"))==T),] %>% .[which(str_detect(.$Abstract,c("noise|sound"))==T),] %>% .$EID %>%as.character())
),5,mf3$G.expo)

#expo=5,no2+noise
subset(mf3,G.expo==5) %>% .[which(str_detect(.$Index.Keywords,c("particulate matter"))==F),] %>% .[which(str_detect(.$Abstract,c("fine particulate matter|fine particul|pm2.|pm 2.|pm<inf>|aerodynamic diameter less than 2.5"))==F),]%>%dim
#expo=5,pm+noise
subset(mf3,G.expo==5) %>% .[which(str_detect(.$Index.Keywords,c("nitrogen dioxide"))==F),] %>% .[which(str_detect(.$Abstract,c("nitrogen dioxide|no2|no<inf>"))==F),]%>%dim


#put on the HNH tag
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

mf3$G.H <- ifelse(mf3$EID %in% (mf3 %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)==T),] %>% .$EID),1,NA)
mf3$G.NH <- ifelse(mf3$EID %in% (mf3 %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)==F),] %>% .[which(str_detect(.$Index.Keywords,EMTREE_nh.design)==T),] %>% .$EID),1,NA)

table(mf3$G.expo)
table(mf3$G.H)
table(mf3$G.NH)
#before going on, do the accuracy test
#precision1
set.seed(10)
acctest_1_EID <- c( mf3[mf3$G.expo==1,] %>% .$EID %>% sample(.,9,replace=F) %>% as.character() , 
                    mf3[mf3$G.expo==2,] %>% .$EID %>% sample(.,47,replace=F) %>% as.character(),
                    mf3[mf3$G.expo==3,] %>% .$EID %>% sample(.,10,replace=F) %>% as.character(),
                    mf3[mf3$G.expo==4,] %>% .$EID %>% sample(.,33,replace=F) %>% as.character(),
                    mf3[mf3$G.expo==5,] %>% .$EID %>% sample(.,5,replace=F) %>% as.character()
                    )
rrr <- subset(mf3,EID%in%acctest_1_EID)
#write.csv(rrr,file="8_outputforBMC201904/OUT_precision_103.csv")

#
set.seed(10)
acctest_2_EID <- c( subset(mf3,G.H==1) %>% .$EID %>% sample(.,16,replace=F) %>% as.character() , 
                    subset(mf3,G.NH==1) %>% .$EID %>% sample(.,8,replace=F) %>% as.character()
                    )
rrr2 <- subset(mf3,EID%in%acctest_2_EID)
#write.csv(rrr2,file="8_outputforBMC201904/OUT_precision_HNH_10.csv")

#mf3 <- mf3 %>% distinct(PubMed.ID, .keep_all = TRUE)
##step 3. make longtable for plot
##FF(inputdf,exposure,healthoutcome,mesh)
#mlg.1A <- FF(df_1A,"no2","cvd",MeSHstr_cvd)
mlg.1A <- FF(mf3[mf3$G.A==1&mf3$G.expo==1,],"no2","cvd",MeSHstr_cvd)
mlg.2A <- FF(mf3[mf3$G.A==1&mf3$G.expo==2,],"pm2.5","cvd",MeSHstr_cvd)
mlg.3A <- FF(mf3[mf3$G.A==1&mf3$G.expo==3,],"noise","cvd",MeSHstr_cvd)

mlg.1B <- FF(mf3[mf3$G.B==1&mf3$G.expo==1,],"no2","diabetes",MeSHstr_dia)
mlg.2B <- FF(mf3[mf3$G.B==1&mf3$G.expo==2,],"pm2.5","diabetes",MeSHstr_dia)
mlg.3B <- FF(mf3[mf3$G.B==1&mf3$G.expo==3,],"noise","diabetes",MeSHstr_dia)

mlg.1_CM <- FF(mf3[mf3$G.expo==1,],"no2","cardiometabolic",MeSHstr_CM)
mlg.2_CM <- FF(mf3[mf3$G.expo==2,],"pm2.5","cardiometabolic",MeSHstr_CM)
mlg.3_CM <- FF(mf3[mf3$G.expo==3,],"noise","cardiometabolic",MeSHstr_CM)

mlg.joint_no2pm25_A <- FF(mf3[mf3$G.A==1&mf3$G.expo==4,],"no2+pm2.5","cvd",MeSHstr_cvd)
mlg.joint_no2pm25_B <- FF(mf3[mf3$G.B==1&mf3$G.expo==4,],"no2+pm2.5","diabetes",MeSHstr_dia)
mlg.joint_no2pm25_CM <- FF(mf3[mf3$G.expo==4,],"no2+pm2.5","cardiometabolic",MeSHstr_CM)

mlg.joint_wnoise_A <- FF(mf3[mf3$G.A==1&mf3$G.expo==5,],"TRAP+noise","cvd",MeSHstr_cvd)
mlg.joint_wnoise_B <- FF(mf3[mf3$G.B==1&mf3$G.expo==5,],"TRAP+noise","diabetes",MeSHstr_dia)
mlg.joint_wnoise_CM <- FF(mf3[mf3$G.expo==5,],"TRAP+noise","cardiometabolic",MeSHstr_CM)

#put long together
mmasterlong <- do.call(rbind,mget((apropos("^mlg."))))

#mmasterlong$h_e <- paste(mmasterlong$exposure,mmasterlong$healthoutcome,sep="-")
mmasterlong$e_o <- paste(mmasterlong$exposure,mmasterlong$healthoutcome,sep="-")
mmasterlong[mmasterlong==0] <- NA #make 0 to NA

##make plot df
#mlong_draw <- subset(mmasterlong,exposure%in% c("no2+pm2.5","no2+noise","pm2.5+noise"))
mlong_draw <- subset(mmasterlong,healthoutcome%in%c("cardiometabolic") & exposure %in% c("no2","pm2.5","noise","no2+pm2.5","TRAP+noise") )

#mlong_draw$groups = factor(mlong_draw$e_o, levels=c("no2-cardiometabolic","pm2.5-cardiometabolic","noise-cardiometabolic"))
mlong_draw$groups = factor(mlong_draw$exposure, levels=c("no2","pm2.5","noise","no2+pm2.5","TRAP+noise"))

#plot
p <- ggplot(mlong_draw,aes(y=n,x=as.numeric(year),color=groups))+geom_point()
p <- p+geom_line(linetype = "solid",show.legend = F)
p <- p+geom_point(aes(shape=mlong_draw$groups),size=5) #make shape for different exposure
p <- p+labs(y = "n, references",x="year")
p

p1 <- p + scale_x_continuous(breaks=c(1994,2000,2005,2010,2017))

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
ggsave("8_outputforBMC201904/figure2.png",p1,width = 12)

##step 4. to test h-nh desigh

#list(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM) %>% sapply(.,F_hypovsnonhypo) %>% t
list(mf3[mf3$G.expo==1,],mf3[mf3$G.expo==2,],mf3[mf3$G.expo==3,],mf3[mf3$G.expo==4,],mf3[mf3$G.expo==5,]) %>% sapply(.,F_hypovsnonhypo) %>% t
#list(df_1A,df_2A,df_3A,df_joint_no2pm25_A,df_joint_wnoise_A) %>% sapply(.,F_hypovsnonhypo) %>% t
list(mf3[mf3$G.A==1&mf3$G.expo==1,],mf3[mf3$G.A==1&mf3$G.expo==2,],mf3[mf3$G.A==1&mf3$G.expo==3,],mf3[mf3$G.A==1&mf3$G.expo==4,],mf3[mf3$G.A==1&mf3$G.expo==5,]) %>% sapply(.,F_hypovsnonhypo) %>% t
#list(df_1B,df_2B,df_3B,df_joint_no2pm25_B,df_joint_wnoise_B) %>% sapply(.,F_hypovsnonhypo) %>% t
list(mf3[mf3$G.B==1&mf3$G.expo==1,],mf3[mf3$G.B==1&mf3$G.expo==2,],mf3[mf3$G.B==1&mf3$G.expo==3,],mf3[mf3$G.B==1&mf3$G.expo==4,],mf3[mf3$G.B==1&mf3$G.expo==5,]) %>% sapply(.,F_hypovsnonhypo) %>% t

#figure 3
#modinto big function with set time peroid#
#org
moda3a4 <- function(t1,t2,t3,t4){
  a3 <- function(indf,expo,health){
    c(expo,health,paste(expo,health,sep="+"),
      indf %>% subset(.,.$Year %in% c(t1:t2-1) ) %>% dim() %>% .[1],
      indf %>% subset(.,.$Year %in% c(t2:t3-1) ) %>% dim() %>% .[1],
      indf %>% subset(.,.$Year %in% c(t3:t4) ) %>% dim() %>% .[1]
    )
  }
  
  a4 <- function(b){
    c(
      c(as.numeric(b[1,4]),as.numeric(b[2,4]),as.numeric(b[3,4]),as.numeric(b[4,4]),as.numeric(b[5,4])  )/ sum(as.numeric(b[,4])),
      c(as.numeric(b[1,5]),as.numeric(b[2,5]),as.numeric(b[3,5]),as.numeric(b[4,5]),as.numeric(b[5,5])  )/ sum(as.numeric(b[,5])),
      c(as.numeric(b[1,6]),as.numeric(b[2,6]),as.numeric(b[3,6]),as.numeric(b[4,6]),as.numeric(b[5,6])  )/ sum(as.numeric(b[,6]))
    )
  }
  
  c2 <- c(
    rbind( a3(mf3[mf3$G.expo==1,],"just no2","cvd"),a3(mf3[mf3$G.expo==2,],"just pm","cvd"),a3(mf3[mf3$G.expo==3,],"just noise","cvd"),
           a3(mf3[mf3$G.expo==4,],"no2+pm2.5","diabetes"),a3(mf3[mf3$G.expo==5,],"M w noise","cardiometabolic")
    ) %>% a4,
    rbind( a3(mf3[mf3$G.A==1&mf3$G.expo==1,],"just no2","cvd"),
           a3(mf3[mf3$G.A==1&mf3$G.expo==2,],"just pm","cvd"),
           a3(mf3[mf3$G.A==1&mf3$G.expo==3,],"just noise","cvd"),
           a3(mf3[mf3$G.A==1&mf3$G.expo==4,],"no2+pm2.5","diabetes"),
           a3(mf3[mf3$G.A==1&mf3$G.expo==5,],"M w noise","cardiometabolic")
    ) %>% a4,
    rbind( a3(mf3[mf3$G.B==1&mf3$G.expo==1,],"just no2","cvd"),
           a3(mf3[mf3$G.B==1&mf3$G.expo==2,],"just pm","cvd"),
           a3(mf3[mf3$G.B==1&mf3$G.expo==3,],"just noise","cvd"),
           a3(mf3[mf3$G.B==1&mf3$G.expo==4,],"no2+pm2.5","diabetes"),
           a3(mf3[mf3$G.B==1&mf3$G.expo==5,],"M w noise","cardiometabolic")
    ) %>% a4  
  ) %>% cbind(.,c(rep("CM",15),rep("cvd",15),rep("diabetes",15))) %>% 
    cbind(., rep(c("single-no2","single-pm2.5","single-noise","mulit-no2+pm2.5","mulit-with noise"),3)) %>%
    cbind(., rep(c(rep(paste(t1,t2-1,sep="-"),5),rep(paste(t2,t3-1,sep="-"),5),rep(paste(t3,t4,sep="-"),5)),3)) %>% as.data.frame()
  
  colnames(c2) <- c("percent","health","expo","period")
  
  c2$e_f = factor(c2$expo, levels=c("single-no2","single-pm2.5","single-noise","mulit-no2+pm2.5","mulit-with noise"))
  #org
  c2
}
#moe end
#make it bw+change faccet name
labels <- c(CM = "cardiometabolic", cvd = "cardiovascular", diabetes= "diabetes")
p <- moda3a4(1994,2008,2013,2017) %>% ggplot(data=., aes(y=as.numeric(as.character(percent)),x=period,fill=e_f)) + geom_bar(stat="identity", position=position_dodge()) + facet_grid(. ~ health, labeller=labeller(health = labels))
p <- p+labs(y="percent",x="time period") #title="a"
p
p2 <- p+
  scale_fill_manual(name = "exposures",
                    labels = c("single-NO2","single-PM2.5","single-Noise","co-NO2+PM2.5","co-with noise"),
                    values = c("gray78","gray61","gray45","gray22","gray0"))
p2 <- p2 + theme_bw() +theme(axis.text=element_text(size=8),
                             axis.title=element_text(size=14,face="bold"))
p2
ggsave("8_outputforBMC201904/figure4.png",p2,width = 8)

#figure s
a <- function(indf,expo,health){
  c(expo,health,paste(expo,health,sep="+"),
    indf %>% subset(.,.$Year %in% c(1994:2007) ) %>% dim() %>% .[1],
    indf %>% subset(.,.$Year %in% c(2008:2012) ) %>% dim() %>% .[1],
    indf %>% subset(.,.$Year %in% c(2013:2017) ) %>% dim() %>% .[1]
  )
}

a2 <- function(b){
  c(as.numeric(b[1,4])/as.numeric(b[3,4]),as.numeric(b[1,5])/as.numeric(b[3,5]),as.numeric(b[1,6])/as.numeric(b[3,6]),
    as.numeric(b[2,4])/as.numeric(b[3,4]),as.numeric(b[2,5])/as.numeric(b[3,5]),as.numeric(b[2,6])/as.numeric(b[3,6]))
}

c <- c(
  rbind( a(mf3[mf3$G.A==1&mf3$G.expo==1,],"no2","cvd"),a(mf3[mf3$G.B==1&mf3$G.expo==1,],"no2","diabetes"),a(mf3[mf3$G.expo==1,],"no2","cardiometabolic")) %>% a2,
  rbind( a(mf3[mf3$G.A==1&mf3$G.expo==2,],"pm2.5","cvd"),a(mf3[mf3$G.B==1&mf3$G.expo==2,],"pm2.5","diabetes"),a(mf3[mf3$G.expo==2,],"pm2.5","cardiometabolic")) %>% a2,
  rbind( a(mf3[mf3$G.A==1&mf3$G.expo==3,],"noise","cvd"),a(mf3[mf3$G.B==1&mf3$G.expo==3,],"noise","diabetes"),a(mf3[mf3$G.expo==3,],"noise","cardiometabolic") ) %>% a2
) %>% cbind(.,c(rep("no2",6),rep("pm2.5",6),rep("noise",6))) %>% 
  cbind(., rep(c(rep("cvd",3),rep("diabetes",3)),3)) %>%
  cbind(., rep(c("1994-2007","2008-2012","2013-2017"),3)) %>% as.data.frame()

colnames(c) <- c("percent","expo","health","period")
c$e_f = factor(c$expo, levels=c("no2","pm2.5","noise"))
#p <- ggplot(data=c, aes(y=as.numeric(as.character(percent)),x=period,fill=health)) + geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~e_f)

labels <- c(no2 = "NO2", pm2.5 = "PM2.5", noise= "Noise")
p <- ggplot(data=c, aes(y=as.numeric(as.character(percent)),x=period,fill=health)) + geom_bar(stat="identity", position=position_dodge()) + facet_grid(. ~ e_f, labeller=labeller(e_f = labels))
p <- p+labs(y="percent",x="time period") #title="a"
#p
p2 <- p+
  scale_fill_manual(name = "health outcomes",
                    labels = c("cardiovascular","diabetes"),
                    values = c("gray44","gray3"))
p2 <- p2 + theme_bw()
p2
ggsave("8_outputforBMC201904/s2.png",p2,width = 8)


#a new one
c <- c(
  rbind( a(mf3[mf3$G.A==1&mf3$G.expo==1,],"no2","cvd"),a(mf3[mf3$G.B==1&mf3$G.expo==1,],"no2","diabetes"),a(mf3[mf3$G.expo==1,],"no2","cardiometabolic")) %>% a2,
  rbind( a(mf3[mf3$G.A==1&mf3$G.expo==2,],"pm2.5","cvd"),a(mf3[mf3$G.B==1&mf3$G.expo==2,],"pm2.5","diabetes"),a(mf3[mf3$G.expo==2,],"pm2.5","cardiometabolic")) %>% a2,
  rbind( a(mf3[mf3$G.A==1&mf3$G.expo==3,],"noise","cvd"),a(mf3[mf3$G.B==1&mf3$G.expo==3,],"noise","diabetes"),a(mf3[mf3$G.expo==3,],"noise","cardiometabolic") ) %>% a2,
  rbind( a(mf3[mf3$G.A==1&mf3$G.expo==4,],"np","cvd"),a(mf3[mf3$G.B==1&mf3$G.expo==4,],"np","diabetes"),a(mf3[mf3$G.expo==4,],"np","cardiometabolic") ) %>% a2,
  rbind( a(mf3[mf3$G.A==1&mf3$G.expo==5,],"wn","cvd"),a(mf3[mf3$G.B==1&mf3$G.expo==5,],"wn","diabetes"),a(mf3[mf3$G.expo==5,],"wn","cardiometabolic") ) %>% a2
) %>% cbind(.,c(rep("no2",6),rep("pm2.5",6),rep("noise",6),rep("np",6),rep("wn",6))) %>% 
  cbind(., rep(c(rep("cvd",3),rep("diabetes",3)),5)) %>%
  cbind(., rep(c("1994-2007","2008-2012","2013-2017"),5)) %>% as.data.frame()

colnames(c) <- c("percent","expo","health","period")
c$e_f = factor(c$expo, levels=c("no2","pm2.5","noise","np","wn"))
#p <- ggplot(data=c, aes(y=as.numeric(as.character(percent)),x=period,fill=health)) + geom_bar(stat="identity", position=position_dodge()) + facet_wrap(~e_f)

labels <- c(no2 = "NO2", pm2.5 = "PM2.5", noise= "Noise",np="no2+pm2.5",wn="no2/pm2.5+noise")
p <- ggplot(data=c, aes(y=as.numeric(as.character(percent)),x=period,fill=health)) + geom_bar(stat="identity", position=position_dodge()) + facet_grid(. ~ e_f, labeller=labeller(e_f = labels))
p <- p+labs(y="percent",x="time period") #title="a"
#p
p2 <- p+
  scale_fill_manual(name = "health outcomes",
                    labels = c("cardiovascular","diabetes"),
                    values = c("gray44","gray3"))
p2 <- p2 + theme_bw()
p2
ggsave("8_outputforBMC201904/s2_2.png",p2,width = 8)
#


###stpe 3.2
outersect <- function(x, y) {
  sort(c(setdiff(x, y),
         setdiff(y, x)))
}

##part4
###find top cited literture
st_topcited_1 <- mf3[mf3$G.expo==1,] %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19,32)] %>% head(10)
st_topcited_2 <- mf3[mf3$G.expo==2,] %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19,32)] %>% head(10)
st_topcited_3 <- mf3[mf3$G.expo==3,] %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19,32)] %>% head(10)
st_topcited_4 <- mf3[mf3$G.expo==4,] %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19,32)] %>% head(10)
st_topcited_5 <- mf3[mf3$G.expo==5,] %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19,32)] %>% head(10)
st_out_cited <- rbind(st_topcited_1,st_topcited_2,st_topcited_3,st_topcited_4,st_topcited_5)
write.csv(st_out_cited,"8_outputforBMC201904/OUT_st_topcited.csv")

#S3
st_topcited_s3 <- mf3[mf3$G.B==1,] %>% .[order(-.$Cited.by),] %>% .[,c(1:4,11,16:19,32)] %>% head(10)
write.csv(st_topcited_s3,"8_outputforBMC201904/OUT_st_st3.csv")
##find top indexkeyword
st_topindexkw_1 <- mf3[mf3$G.expo==1,] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(mf3[mf3$G.expo==1,])/10) )
st_topindexkw_2 <- mf3[mf3$G.expo==2,] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(mf3[mf3$G.expo==2,])/10) )
st_topindexkw_3 <- mf3[mf3$G.expo==3,] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(mf3[mf3$G.expo==3,])/10) )
st_topindexkw_4 <- mf3[mf3$G.expo==4,] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(mf3[mf3$G.expo==4,])/10) )
st_topindexkw_5 <- mf3[mf3$G.expo==5,] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% subset(.,Freq> (length(mf3[mf3$G.expo==5,])/10) )

st_topindexkw_overlap <- intersect(st_topindexkw_1[,1],st_topindexkw_2[,1]) %>% intersect(.,st_topindexkw_3[,1]) #%>% intersect(.,st_topindexkw_no2pm25CM[,1]) %>% intersect(.,st_topindexkw_wnoiseCM[,1]) 
#mannual add
st_topindexkw_overlap <- c(st_topindexkw_overlap,"analysis", "risk factor", "air pollutants", "air pollutant", "human", "pollution","iron","enviorment",
                           "noise","nitrogen dioxide","particulate matter","pollutant","health","controlled study","major clinical study","male","female",
                           "cohort analysis","age","risk","health risk","nitrogen dioxides","exposure","diseases","noise pollution","major clinical study")
#st_topindexkw_common <- rbind(df_1CM,df_2CM,df_3CM) %>% .[!duplicated(.$EID), ] %>% .[!duplicated(.$Title), ] %>% 
#                        .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] %>% 
#                        .[which(st_topindexkw_common[,1]%in%st_topindexkw_overlap),] 
st_uniindexkw_1 <- st_topindexkw_1[-which(st_topindexkw_1[,1]%in%st_topindexkw_overlap),]
st_uniindexkw_2 <- st_topindexkw_2[-which(st_topindexkw_2[,1]%in%st_topindexkw_overlap),]
st_uniindexkw_3 <- st_topindexkw_3[-which(st_topindexkw_3[,1]%in%st_topindexkw_overlap),]
st_uniindexkw_4 <- st_topindexkw_4[-which(st_topindexkw_4[,1]%in%st_topindexkw_overlap),]
st_uniindexkw_5 <- st_topindexkw_5[-which(st_topindexkw_5[,1]%in%st_topindexkw_overlap),]

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
#output
st_rec_topikw_1 <-F_recheck_re_freq2(st_uniindexkw_1,mf3[mf3$G.expo==1,]) %>% .[order(-.$freq),] %>%mutate(G.expo=1)
st_rec_topikw_2 <-F_recheck_re_freq2(st_uniindexkw_2,mf3[mf3$G.expo==2,]) %>% .[order(-.$freq),] %>%mutate(G.expo=2)
st_rec_topikw_3 <-F_recheck_re_freq2(st_uniindexkw_3,mf3[mf3$G.expo==3,]) %>% .[order(-.$freq),] %>%mutate(G.expo=3)
st_rec_topikw_4 <-F_recheck_re_freq2(st_uniindexkw_4,mf3[mf3$G.expo==4,]) %>% .[order(-.$freq),] %>%mutate(G.expo=4)
st_rec_topikw_5 <-F_recheck_re_freq2(st_uniindexkw_5,mf3[mf3$G.expo==5,]) %>% .[order(-.$freq),] %>%mutate(G.expo=5)
st_out_rec_topikw <- rbind(st_rec_topikw_1,st_rec_topikw_2,st_rec_topikw_3,st_rec_topikw_4,st_rec_topikw_5)
write.csv(st_out_rec_topikw,"8_outputforBMC201904/OUT_st_topindexkw.csv")

#1(18,9) 2(93,47) 3(20,10) 4(66,33) 5(10,5)
st_rec_topikw_1 %>% wordcloud2(.,
                   color = ifelse(.[, 2] > 18, 'Black', 
                                  ifelse(.[, 2] > 9, '#808080', '#D3D3D3')),size=0.4  )
st_rec_topikw_2 %>% wordcloud2(.,
                               color = ifelse(.[, 2] > 93, 'Black', 
                                              ifelse(.[, 2] > 47, '#808080', '#D3D3D3')),size=0.4  )
st_rec_topikw_3 %>% wordcloud2(.,
                               color = ifelse(.[, 2] > 20, 'Black', 
                                              ifelse(.[, 2] > 10, '#808080', '#D3D3D3')),size=.4  )
st_rec_topikw_4 %>% wordcloud2(.,
                               color = ifelse(.[, 2] > 66, 'Black', 
                                              ifelse(.[, 2] > 33, '#808080', '#D3D3D3')),size=.4  )
st_rec_topikw_5 %>% wordcloud2(.,
                               color = ifelse(.[, 2] > 10, 'Black', 
                                              ifelse(.[, 2] > 5, '#808080', '#D3D3D3')),size=.4  )

#stable1
st_ikwH <- mf3[mf3$G.H==1,] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[-which(.[,1]%in%st_topindexkw_overlap),] %>% .[order(-.$Freq),]
st_ikwH2 <- F_recheck_re_freq2(st_ikwH,mf3[mf3$G.H==1,]) %>% .[order(-.$freq),] %>% mutate(rr=freq/159,HNH=1) %>% mutate(Hrr=rr) 
st_ikwNH <- mf3[mf3$G.NH==1,] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[-which(.[,1]%in%st_topindexkw_overlap),] %>% .[order(-.$Freq),]
st_ikwNH2 <- F_recheck_re_freq2(st_ikwNH,mf3[mf3$G.NH==1,]) %>% .[order(-.$freq),] %>% mutate(rr=freq/80,HNH=0)%>% mutate(NHrr=rr)
st_st2 <- merge(st_ikwH2,st_ikwNH2,by="w",all = TRUE) 
st_st2[is.na(st_st2)]=0
st_st2$diffr <- st_st2$Hrr-st_st2$NHrr
write.csv(st_st2,"8_outputforBMC201904/OUT_st2.csv")
#misc
###find certain articles (from the research trend)
#cartlist <- df_2A %>% subset(.,.$Year %in% c(2002:2017) )
#cartlist <- df_3_CM %>% .[which(str_detect(.$Index.Keywords,"occupational")==F),] %>% .[which(str_detect(.$Index.Keywords,EMTREE_hypo)==F),]
cartlist <- df_joint_no2pm25_CM %>% .[which(str_detect(tolower(.$Index.Keywords),"Canada|canada")==T),] 
write.csv(cartlist,"4_r_output/find_certain_articles.csv")

##draw VennD (show the overlaping)

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

list(mf3[mf3$G.expo==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==1&mf3$G.A==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==1&mf3$G.B==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==2,] %>%.$Cited.by,
     mf3[mf3$G.expo==2&mf3$G.A==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==2&mf3$G.B==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==3,] %>%.$Cited.by,
     mf3[mf3$G.expo==3&mf3$G.A==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==3&mf3$G.B==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==4,] %>%.$Cited.by,
     mf3[mf3$G.expo==4&mf3$G.A==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==4&mf3$G.B==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==5,] %>%.$Cited.by,
     mf3[mf3$G.expo==5&mf3$G.A==1,] %>%.$Cited.by,
     mf3[mf3$G.expo==5&mf3$G.B==1,] %>%.$Cited.by) %>% sapply(.,median,na.rm=T)

