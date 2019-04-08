#quick map the Aot and EPA

#Sys.setlocale("LC_ALL", "English")
library(dplyr)
library(ggplot2)
library(scales)
library(stringr)
library(gridExtra)
library(tidyr)
library(ggmap)

aot <- read.csv("C:/Users/YK Huang/Downloads/Array_of_Things_Locations.csv",header=T)


map <- get_map(location = 'Chicago', zoom = 12,maptype = "toner")
#map <- get_map(location = 'Chicago', zoom = 12,color = "bw")
p <- ggmap(map) + geom_point(aes(x = Longitude, y = Latitude,color=Status,shape=Status), data = aot,size=4)
p

ggsave("C:/Users/YK Huang/Downloads/aotout.png",p,width = 8,height=8)

#
arp <- read.csv("C:/Users/YK Huang/Downloads/airportsite.csv",header=T)

#map2 <- get_map(location = c(lon = -87.9, lat = 41.9), zoom = 11,color = "bw")
map2 <- get_map(location = c(lon = -87.8, lat = 41.9), zoom = 10,maptype = "toner")
p2 <- ggmap(map2) + geom_point(aes(x = Longitude, y = Latitude,color=System,shape=System), data = arp,size=4)
p2

ggsave("C:/Users/YK Huang/Downloads/arpout.png",p2,width = 8,height=8)


###
st_topindexkw_H_3CM <- df_3CM %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)==T),] %>% .$Index.Keywords %>% as.character %>% tolower %>% strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] 
st_topindexkw_NH_3CM <- df_3CM %>% .[which(str_detect(.$Index.Keywords,EMTREE_nh.design)==T),] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] 

st_topindexkw_H_3CM$ratio <- st_topindexkw_H_3CM$Freq/26
st_topindexkw_NH_3CM$ratio <- st_topindexkw_NH_3CM$Freq/40
colnames(st_topindexkw_H_3CM)[1] <- "word"
colnames(st_topindexkw_NH_3CM)[1] <- "word"

ss1 <- st_topindexkw_H_3CM[order(st_topindexkw_H_3CM$word),] %>% .[which(str_detect(.[,1],EMTREE_h.design)==F),]
ss2 <- st_topindexkw_NH_3CM[order(st_topindexkw_NH_3CM$word),] %>% .[which(str_detect(.[,1],EMTREE_nh.design)==F),]

ss3 <- merge(ss1,ss2, by = "word", all= TRUE)
ss3[is.na(ss3)] <- ""
#ss3$ratio_xy <- ss3$ratio.x-ss3$ratio.y
write.csv(ss3,"4_r_output/OUT_HNH.csv")

td_commom <- intersect(st_topindexkw_H_3CM[,1],st_topindexkw_NH_3CM[,1])
su_H_3CM <- st_topindexkw_H_3CM %>% .[-which(.[,1]%in%td_commom),] %>% .[which(str_detect(.[,1],EMTREE_h.design)==F),]
su_NH_3CM <- st_topindexkw_NH_3CM %>% .[-which(.[,1]%in%td_commom),] %>% .[which(str_detect(.[,1],EMTREE_nh.design)==F),]


##recheck each records has the indexkeyword about the proportion
sqq1 <- df_3CM %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)==T),] %>% .$Index.Keywords %>% as.character %>% tolower 
sqq2 <- df_3CM %>% .[which(str_detect(.$Index.Keywords,EMTREE_nh.design)==T),] %>% .$Index.Keywords %>% as.character %>% tolower 

table(str_detect(sqq2,"diabetes mellitus"))["TRUE"]

###1CM
###
st_topindexkw_H_2CM <- df_2CM %>% .[which(str_detect(.$Index.Keywords,EMTREE_h.design)==T),] %>% .$Index.Keywords %>% as.character %>% tolower %>% strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] 
st_topindexkw_NH_2CM <- df_2CM %>% .[which(str_detect(.$Index.Keywords,EMTREE_nh.design)==T),] %>% .$Index.Keywords %>% as.character %>% tolower %>%strsplit(.,"; ") %>% unlist %>% table %>% as.data.frame %>% .[order(-.$Freq),] 

st_topindexkw_H_2CM$ratio <- st_topindexkw_H_2CM$Freq/1038
st_topindexkw_NH_2CM$ratio <- st_topindexkw_NH_2CM$Freq/560
colnames(st_topindexkw_H_2CM)[1] <- "word"
colnames(st_topindexkw_NH_2CM)[1] <- "word"

ss1 <- st_topindexkw_H_2CM[order(st_topindexkw_H_2CM$word),] %>% .[which(str_detect(.[,1],EMTREE_h.design)==F),]
ss2 <- st_topindexkw_NH_2CM[order(st_topindexkw_NH_2CM$word),] %>% .[which(str_detect(.[,1],EMTREE_nh.design)==F),]

ss3 <- merge(ss1,ss2, by = "word", all= TRUE)
ss3[is.na(ss3)] <- ""
#ss3$ratio_xy <- ss3$ratio.x-ss3$ratio.y
write.csv(ss3,"4_r_output/OUT_HNH2.csv")

td_commom <- intersect(st_topindexkw_H_3CM[,1],st_topindexkw_NH_3CM[,1])
su_H_3CM <- st_topindexkw_H_3CM %>% .[-which(.[,1]%in%td_commom),] %>% .[which(str_detect(.[,1],EMTREE_h.design)==F),]
su_NH_3CM <- st_topindexkw_NH_3CM %>% .[-which(.[,1]%in%td_commom),] %>% .[which(str_detect(.[,1],EMTREE_nh.design)==F),]


aaa <- rbind(df_1CM,df_2CM,df_3CM,df_joint_no2pm25_CM,df_joint_wnoise_CM)
aaa %>% .[which(str_detect(.$Index.Keywords,MeSHstr_cvd)==T),] %>% dim
aaa %>% .[which(str_detect(.$Index.Keywords,MeSHstr_dia)==T),] %>% dim