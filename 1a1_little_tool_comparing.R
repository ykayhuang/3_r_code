#a little tool that compare two Scopus outputs
#20180716

#Sys.setlocale("LC_ALL", "English")

library(dplyr)
library(tidyr)

#Scopus outputs 1
#raw_SCOP1 <- read.csv("2_raw_scopus_export/LB3_cm_noise.csv",header=T)
raw_SCOP1 <- read.csv("4_r_output/OUT_precision_515.csv",header=T)
#raw_SCOP1 <- read.csv("C:/Users/YK Huang/Downloads/scopus (14).csv",header=T)

#Scopus outputs 2
#raw_SCOP2 <- read.csv("C:/Users/YK Huang/Downloads/scopus (13).csv",header=T)
#raw_SCOP2 <- read.csv("9_sys_review_data/relevant_sheet_CM_noise.csv",header=T)
raw_SCOP2 <- read.csv("8_outputforBMC201904/OUT_precision_102.csv",header=T)

  #merge together and kickout the duplicate 
  df_nodup <- rbind(raw_SCOP1,raw_SCOP2) %>% .[!duplicated(.$EID), ] %>% .[!duplicated(.$Title), ] 
  write.csv(df_nodup,"4_r_output/SCOPTOOL_nodup.csv")

  #find those 1 have but 2 do not have (only in 1)
  df_only1 <- subset(raw_SCOP1,EID %in% setdiff(raw_SCOP1$EID,raw_SCOP2$EID))
  write.csv(df_only1,"4_r_output/SCOPTOOL_only1.csv")
  
  #find those 2 have but 1 do not have (only in 2)
  df_only2 <- subset(raw_SCOP2,EID %in% setdiff(raw_SCOP2$EID,raw_SCOP1$EID))
  write.csv(df_only2,"4_r_output/SCOPTOOL_only2.csv")

  #find those both have 
  df_intersect <- subset(raw_SCOP1,EID %in% intersect(raw_SCOP2$EID,raw_SCOP1$EID))
  write.csv(df_intersect,"4_r_output/SCOPTOOL_intersect.csv")
  a <- intersect(raw_SCOP1$Title,raw_SCOP2$Title)